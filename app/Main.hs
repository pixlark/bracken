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
import Output
import Parser (langParser)
import Typechecker (typecheck)

exitErr :: String -> IO ()
exitErr msg = do hPutStrLn stderr msg; exitWith $ ExitFailure 1

data Arguments = Arguments
  { sourceFile :: String
  , noExecutable :: Bool
  , noEntry :: Bool
  }

argTemplate :: A.Parser Arguments
argTemplate = Arguments
              <$> strOption ( long "source"
                            <> short 's'
                            <> metavar "PATH"
                            <> help "Source file to be compiled" )
              <*> switch ( long "noExecutable"
                         <> short 'x'
                         <> help "Only generate C code, don't compile it" )
              <*> switch ( long "noEntry"
                         <> short 'e'
                         <> help "Don't require a specified entry point" )

cMajor :: String -> String -> Arguments -> Either String String
cMajor filename source args
  = do ast <- Bifunctor.first errorBundlePretty
            $ runParser langParser filename source
       typecheck ast
       cAst <- compileSource ast $ makeFlags $ if noEntry args then [NoEntryPoint] else []
       return $ unpack $ emitC cAst

main :: IO ()
main = do
  args <- execParser (info (argTemplate <**> helper)
                       $ fullDesc
                      <> progDesc "Run the cMajor compiler"
                      <> header "cMajor - superpowered C")

  s <- catch (readFile $ sourceFile args)
    $ \(e :: IOException) -> do hPutStrLn stderr
                                  $ concat ["Source file '"
                                           , sourceFile args
                                           , "' doesn't exist"]
                                exitWith $ ExitFailure 1
                                return undefined -- Unreachable

  let result = Bifunctor.first ("Error: " ++) $ cMajor (sourceFile args) s args
  genSrc <- case result of
    Left err -> do hPutStrLn stderr err; exitWith $ ExitFailure 1
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
