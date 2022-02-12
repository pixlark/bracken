{-# LANGUAGE DuplicateRecordFields
           , NamedFieldPuns #-}

module Typechecker (typecheck) where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map.Strict as Map
import Data.List (foldl')

import Parser

type Env = Map.Map String TypeExpression

checkTopLevel :: Either String Env -> AST -> Either String Env
checkTopLevel (Left err) _ = Left err
checkTopLevel (Right env) (ToplevelVar (ValueBinding { identifier, typeExpr, expr })) =
  case Map.lookup identifier env of
    Just _  -> Left  $ "Variable '" ++ identifier ++ "' declared multiple times!"
    Nothing -> Right $ Map.insert identifier typeExpr env

typecheck :: [AST] -> Either String ()
typecheck ast = Bifunctor.second (const ()) $ foldl' checkTopLevel (Right Map.empty) ast
