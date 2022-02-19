{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (catch, IOException)
import Control.Monad (unless, when)
import qualified Data.Bifunctor as Bifunctor
import Data.Text (unpack)
import System.Environment (getArgs, getEnv)
import System.Exit (exitWith, ExitCode(..))
import System.IO

import Text.Megaparsec

import Options.Applicative as A
import Data.Semigroup ((<>))

import CC (compileCExecutable)
import Output (compileSource, emitC)
import Parser (langParser)
import Typechecker (typecheck)

cMajor :: String -> String -> Either String String
cMajor filename source
  = do ast <- Bifunctor.first errorBundlePretty
            $ runParser langParser filename source
       typecheck ast
       cAst <- compileSource ast
       return $ unpack $ emitC cAst

exitErr :: String -> IO ()
exitErr msg = do hPutStrLn stderr msg; exitWith $ ExitFailure 1

data Arguments = Arguments
  { sourceFile :: String
  , noExecutable :: Bool
  }

argTemplate :: A.Parser Arguments
argTemplate = Arguments
              <$> strOption ( long "source"
                            <> short 's'
                            <> metavar "PATH"
                            <> help "Source file to be compiled" )
              <*> switch ( long "noExecutable"
                         <> short 'n'
                         <> help "Prevent actual code generation from running" )

main :: IO ()
main = do
  args <- execParser (info (argTemplate <**> helper)
                       $ fullDesc
                      <> progDesc "Run the cMajor compiler"
                      <> header "cMajor - superpowered C")

--  unless ((length args) == 1)
--    $ do hPutStrLn stderr "Expecting one source file"
--         exitWith $ ExitFailure 1

  s <- catch (readFile $ sourceFile args)
    $ \(e :: IOException) -> do hPutStrLn stderr
                                  $ concat ["Source file '"
                                           , sourceFile args
                                           , "' doesn't exist"]
                                exitWith $ ExitFailure 1
                                return undefined -- Unreachable

  let result = Bifunctor.first ("Error: " ++) $ cMajor (sourceFile args) s
  genSrc <- case result of
    Left err -> do hPutStrLn stderr s; exitWith $ ExitFailure 1
    Right genSrc -> do putStrLn genSrc
                       return genSrc

  when (noExecutable args) $ exitWith ExitSuccess

  cc <- getEnv "CC"
  when (cc == "") $ do hPutStrLn stderr "Environment variable $CC is not set"
                       exitWith $ ExitFailure 1

  execName <- compileCExecutable genSrc cc
  case execName of
    Just name -> putStrLn $ "Successfully compiled to " ++ name
    Nothing -> return ()
