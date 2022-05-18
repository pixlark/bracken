{-# LANGUAGE DuplicateRecordFields
           , DeriveDataTypeable
           , NamedFieldPuns
           , OverloadedStrings #-}

module Main where

import Control.Monad (unless, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except

import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text as Text
import qualified Data.Text.IO

import Options.Applicative as A

import qualified System.Directory as Dir
import System.Exit (ExitCode(..))
import System.FilePath
import qualified System.IO as IO
import qualified System.Process as P

import Text.JSON.Generic

--
-- Fancy output
--

data TextColor = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

colorCode x = case x of
  Black  -> 0; Red   -> 1; Green   -> 2;
  Yellow -> 3; Blue  -> 4; Magenta -> 5;
  Cyan   -> 6; White -> 7;

data TextStyle = Normal | Bold | Faint

styleCode x = case x of
  Normal -> Nothing
  Bold   -> Just 1
  Faint  -> Just 2

data Fancy = Fancy TextColor TextStyle

writeFancy :: Fancy -> String -> IO ()
writeFancy f s = putStrLn $ startEscape f ++ s ++ resetEscape
  where resetEscape = "\x1b[0m"
        wrap i = "\x1b[" ++ i ++ "m"
        startEscape (Fancy color style) =
          (fromMaybe "" $ (wrap . show) <$> styleCode style) ++
          (wrap $ ("3" ++) $ show $ colorCode color)

depthOf :: Int -> String
depthOf d = Prelude.take (d * 2) (repeat ' ')

indentPar :: Int -> String -> String
indentPar d = Prelude.concat . (indent:) . List.intersperse ('\n':indent) . Prelude.lines
  where indent = depthOf d

--
-- Command-line options
--

data GenerateOptions = GenerateOptions { sourcePath :: String }
  deriving (Show)

data RunOptions = RunOptions { testDir :: String }
  deriving (Show)

data Command = Generate GenerateOptions
             | Run      RunOptions
  deriving (Show)

data Options = Options { command :: Command }
  deriving (Show)

generateTemplate :: A.Parser GenerateOptions
generateTemplate = GenerateOptions
                   <$> strOption (  long "source"
                                 <> short 's'
                                 <> metavar "PATH"
                                 <> help "Source file to generate test from" )

runTemplate :: A.Parser RunOptions
runTemplate = RunOptions
              <$> strOption (  long "directory"
                            <> short 'd'
                            <> metavar "DIR"
                            <> help "Directory that containst test hierarchy" )

argTemplate :: A.Parser Options
argTemplate = Options <$> ((Generate <$> subparser generateCommand) <|> (Run <$> subparser runCommand))
  where generateCommand = A.command "generate" (info (generateTemplate <**> helper)
                                                 $ fullDesc
                                                <> progDesc "Generate an output file from some source"
                                                <> header   "cMajorTests-generate")
        runCommand = A.command "run" (info (runTemplate <**> helper)
                                       $ fullDesc
                                      <> progDesc "Run all tests recursively in a directory"
                                      <> header   "cMajorTests-run")

--
-- Test output management
--

data TestOutput =
  TestOutput { exitCode :: Int
             , compilerStdout :: Text
             , compilerStderr :: Text
             , programStdout  :: Text
             , programStderr  :: Text
             }
  deriving(Eq, Show, Typeable, Data)

-- So that root of JSON input/output is an object, as per the spec
data EncodingWrapper = EncodingWrapper TestOutput
  deriving(Show, Typeable, Data)

type ProcHandles = (IO.Handle, IO.Handle, P.ProcessHandle)

produceOutputFile :: FilePath -> TestOutput -> ExceptT String IO ()
produceOutputFile sourcePath output = do
  let outputPath = replaceExtension sourcePath ".output.json"
      toEncode = EncodingWrapper output
      encoded = pack $ encodeJSON toEncode
  liftIO $ IO.withFile outputPath IO.WriteMode $ flip Data.Text.IO.hPutStr $ encoded

--
-- Process management
--

runProcess :: String -> [String] -> ExceptT String IO (Int, Text, Text)
runProcess name args =
  do handles <- ExceptT startProcess
     ExceptT $ getOutput handles
       where processSpec =
               (P.proc name args) { P.std_out = P.CreatePipe
                                  , P.std_err = P.CreatePipe }
             startProcess :: IO (Either String ProcHandles)
             startProcess = 
               do procResult <- P.createProcess processSpec
                  return $ case procResult of
                             (_, Just pStdout, Just pStderr, pHandle) -> Right (pStdout, pStderr, pHandle)
                             _ -> Left $ "Failed to run process: '" ++ (Prelude.unwords (name:args)) ++ "'"

             getOutput :: ProcHandles -> IO (Either String (Int, Text, Text))
             getOutput (pStdout, pStderr, pHandle) =
               do exitCode <- P.waitForProcess pHandle
                  out <- pack <$> IO.hGetContents pStdout
                  err <- pack <$> IO.hGetContents pStderr
                  return $ Right $ (case exitCode of; ExitSuccess -> 0; ExitFailure x -> x, out, err)

runTestFile :: FilePath -> ExceptT String IO (Int, Text, Text)
runTestFile path = runProcess "stack" [ "run", "--silent", "--", "-s", path ]

--
-- Checking file types
--

data FileType = File | Directory | Neither
  deriving (Show)

typeOfFile :: FilePath -> IO FileType
typeOfFile f = do isDir  <- Dir.doesDirectoryExist f
                  isFile <- Dir.doesFileExist f
                  return $ case (isDir, isFile) of
                    (True,  False) -> Directory
                    (False, True ) -> File
                    (False, False) -> Neither
                    (True,  True ) -> error "unreachable"

--
-- generate subcommand
--

generate :: GenerateOptions -> ExceptT String IO ()
generate opts =
  do ExceptT checkExists
     (exitCode, compilerStdout, compilerStderr) <- runTestFile $ sourcePath opts
     let output = TestOutput { exitCode, compilerStdout, compilerStderr, programStdout = "", programStderr = "" }

     case exitCode of
       0   -> do (_, programStdout, programStderr) <- runProcess "./executable" []
                 liftIO $ Dir.removeFile "./executable"
                 produceOutputFile (sourcePath opts) $ output { programStdout, programStderr }
       _ -> produceOutputFile (sourcePath opts) output

       where checkExists :: IO (Either String ())
             checkExists =
               do exists <- Dir.doesFileExist $ sourcePath opts
                  return $ if exists
                           then Right ()
                           else Left $ Prelude.unwords ["Provided source file", sourcePath opts, "doesn't exist or isn't a file"]

--
-- run subcommand
--

-- todo: check program output. right now assumes program outputs nothing (our language can't print yet!)
runOnFile :: RunOptions -> FilePath -> Int -> ExceptT String IO ()
runOnFile opts path depth =
  do liftIO $ writeFancy (Fancy White Normal) $
       Prelude.concat [depthOf depth, "Testing ./", path, "..."]
     (code, out, err) <- runTestFile path
     let jsonPath = replaceExtension path ".output.json"
     expected <- liftIO $ (decodeJSON :: String -> TestOutput) <$> IO.readFile jsonPath
     let actual = TestOutput
           { exitCode = code
           , compilerStdout = out
           , compilerStderr = err
           , programStdout = ""
           , programStderr = ""
           }
     liftIO $ if expected == actual
              then writeFancy (Fancy Green Bold) $
                    Prelude.concat [depthOf depth, "Ok."]
              else do writeFancy (Fancy Red Bold) $
                        Prelude.concat [depthOf depth, "Failed!"]
                      writeFancy (Fancy White Faint) $ indentPar depth $ Prelude.concat ["expected: \n", unpack $ compilerStdout expected]
                      writeFancy (Fancy White Faint) $ indentPar depth $ Prelude.concat ["actual:   \n", unpack $ compilerStdout actual]

runInDirectory :: RunOptions -> FilePath -> Int -> ExceptT String IO ()
runInDirectory opts path depth =
  do liftIO $ writeFancy (Fancy Blue Bold) $
       Prelude.concat [depthOf depth, "./", path, "/"]
     files <- liftIO $ Dir.listDirectory path
     forM_ files $ \f ->
       do let fPath = path </> f
          fType <- liftIO $ typeOfFile fPath
          case fType of
            File      -> unless (".output.json" `List.isSuffixOf` fPath) $ runOnFile opts fPath depth
            Directory -> runInDirectory opts fPath (depth + 1)
            Neither   -> liftIO $ return ()

run :: RunOptions -> ExceptT String IO ()
run opts = do
  ExceptT $ dirExists
  runInDirectory opts (testDir opts) 0
  where dirExists = do exists <- Dir.doesDirectoryExist $ testDir opts
                       return $ if exists then Right() else Left $
                         Prelude.unwords ["Provided directory", testDir opts, "doesn't exist or isn't a directory"]

--
-- Main
--

main :: IO ()
main = do
  opts <- execParser (info (argTemplate <**> helper)
                       $  fullDesc
                       <> progDesc "cMajor output testing framework"
                       <> header "cMajorTests")
  err <- case opts of
           Options { command = Generate genOpts } -> runExceptT $ generate genOpts
           Options { command = Run runOpts }      -> runExceptT $ run runOpts
  case err of
    Left e -> putStrLn $ "Error: " ++ e
    Right () -> return ()
