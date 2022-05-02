{-# LANGUAGE ScopedTypeVariables #-}

module CMajor (Options(..), run) where

import Control.Exception (catch, IOException)
import Control.Monad (unless, when)
import qualified Data.Bifunctor as Bifunctor
import Data.Text (unpack)
import System.Environment (getArgs, getEnv)
import System.Exit (exitWith, ExitCode(..))
import System.IO

import Text.Megaparsec

import CC (compileCExecutable)
import Output
import Parser (langParser)
import Typechecker (typecheck)

--
-- Entry point for the cMajor compiler.
--     Uses a set of options (likely originating from the command line)
--   to transpile some file(s) into C code, and then to compile those into
--   executables.
--

data Options = Options
  { sourceFile :: String
  , noExecutable :: Bool
  , noEntry :: Bool
  }

exitErr :: String -> IO ()
exitErr msg = do hPutStrLn stderr msg; exitWith $ ExitFailure 1

transpiler :: String -> String -> Options -> Either String String
transpiler filename source opts
  = do ast <- Bifunctor.first errorBundlePretty
            $ runParser langParser filename source
       typecheck ast
       cAst <- compileSource ast $ makeFlags $ if noEntry opts then [NoEntryPoint] else []
       return $ unpack $ emitC cAst

run :: Options -> IO ()
run opts = do
  -- Read source file
  s <- catch (readFile $ sourceFile opts)
    $ \(e :: IOException) -> do hPutStrLn stderr
                                  $ concat ["Source file '"
                                           , sourceFile opts
                                           , "' doesn't exist"]
                                exitWith $ ExitFailure 1
                                return undefined -- Unreachable

  -- Transpile into C code
  let result = Bifunctor.first ("Error: " ++) $ transpiler (sourceFile opts) s opts
  genSrc <- case result of
    Left err -> do hPutStrLn stderr err; exitWith $ ExitFailure 1
    Right genSrc -> do putStrLn genSrc
                       return genSrc

  when (noExecutable opts) $ exitWith ExitSuccess

  -- Compile into an executable
  cc <- getEnv "CC"
  when (cc == "") $ do hPutStrLn stderr "Environment variable $CC is not set"
                       exitWith $ ExitFailure 1

  execName <- compileCExecutable genSrc cc
  case execName of
    Just name -> putStrLn $ "Successfully compiled to " ++ name
    Nothing -> return ()
