module Main where

import qualified Data.Bifunctor as Bifunctor
import Text.Megaparsec

import Parser (langParser)

main :: IO ()
main = interact
     $ (++ "\n") . reduceEither
     . (Bifunctor.bimap ("Error: " ++) ("Result: " ++))
     . (Bifunctor.bimap errorBundlePretty show)
     . (runParser langParser "(stdin)")
     where reduceEither :: Either a a -> a
           reduceEither (Left x)  = x
           reduceEither (Right x) = x
