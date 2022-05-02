{-# LANGUAGE DuplicateRecordFields
           , DeriveDataTypeable
           , NamedFieldPuns
           , OverloadedStrings #-}

module Main where

import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Either
import Data.Text
import qualified Data.Text.IO
import qualified System.Directory as Dir
import System.Exit (ExitCode(..))
import System.FilePath
import qualified System.IO as IO
import qualified System.Process as P

import Text.JSON.Generic

import Options.Applicative as A

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

data TestOutput =
  TestOutput { exitCode :: Int
             , compilerStdout :: Text
             , compilerStderr :: Text
             , programStdout  :: Text
             , programStderr  :: Text
             }
  deriving(Show, Typeable, Data)

-- So that root of JSON input/output is an object, as per the spec
data EncodingWrapper = EncodingWrapper TestOutput
  deriving(Show, Typeable, Data)

type ProcHandles = (IO.Handle, IO.Handle, P.ProcessHandle)

produceOutputFile :: IO.FilePath -> TestOutput -> ExceptT String IO ()
produceOutputFile sourcePath output = do
  let outputPath = replaceExtension sourcePath ".output.json"
      toEncode = EncodingWrapper output
      encoded = pack $ encodeJSON toEncode
  liftIO $ IO.withFile outputPath IO.WriteMode $ flip Data.Text.IO.hPutStr $ encoded

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

generate :: GenerateOptions -> ExceptT String IO ()
generate opts =
  do ExceptT checkExists
     (exitCode, compilerStdout, compilerStderr) <- runProcess "stack" [ "run", "--silent", "--", "-s", sourcePath opts ]
     let output = TestOutput { exitCode, compilerStdout, compilerStderr, programStdout = "", programStderr = "" }

     case exitCode of
       0   -> do (_, programStdout, programStderr) <- runProcess "./executable" []
                 produceOutputFile (sourcePath opts) $ output { programStdout, programStderr }
       _ -> produceOutputFile (sourcePath opts) output

       where checkExists :: IO (Either String ())
             checkExists =
               do exists <- Dir.doesFileExist $ sourcePath opts
                  return $ if exists
                           then Right ()
                           else Left $ Prelude.unwords ["Provided source file", sourcePath opts, "doesn't exist or isn't a file"]

run :: RunOptions -> ExceptT String IO ()
run opts = do
  ExceptT $ do exists <- Dir.doesDirectoryExist $ testDir opts
               return $ if exists
                        then Right ()
                        else Left $ Prelude.unwords ["Provided directory", testDir opts, "doesn't exist or isn't a directory"]

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
