{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (catch, IOException)
import Control.Monad (unless)
import qualified Data.Bifunctor as Bifunctor
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO
import Text.Megaparsec

import Typechecker (typecheck)
import Parser (langParser)

cMajor :: String -> String -> Either String String
cMajor filename
  = (Bifunctor.bimap ("Error: " ++) (("Result: " ++) . show))
    -- Run typecheck, pass AST on if no error
    . (\e -> either Left (const e) $ e >>= typecheck)
    -- Make parsing error human-readable
    . (Bifunctor.first errorBundlePretty)
    -- Parse into AST
    . (runParser langParser filename)

main :: IO ()
main = do
  args <- getArgs
  unless ((length args) == 1)
    $ do hPutStrLn stderr "Expecting one source file"
         exitWith $ ExitFailure 1
  let filename = head args
  s <- catch (readFile filename)
    $ \(e :: IOException) -> do hPutStrLn stderr
                                  $ concat ["Source file '"
                                           , filename
                                           , "' doesn't exist"]
                                exitWith $ ExitFailure 1
                                return undefined -- Unreachable
  either
    (\s -> do hPutStrLn stderr s; exitWith $ ExitFailure 1)
    (hPutStrLn stdout)
    $ cMajor filename s
