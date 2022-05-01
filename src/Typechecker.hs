{-# LANGUAGE DuplicateRecordFields
           , NamedFieldPuns #-}

module Typechecker (typecheck) where

import Control.Monad (unless, void)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map.Strict as Map
import Data.List (foldl')

import Parser

-- Debugging purposes only
import Debug.Trace
--

type Env = Map.Map String TypeExpression

bindType :: Env -> (String, TypeExpression) -> Either String Env
bindType env (identifier, typeExpr) =
  case Map.lookup identifier env of
    Just _  -> Left  $ "Variable '" ++ identifier ++ "' declared multiple times!"
    Nothing -> Right $ Map.insert identifier typeExpr env

checkScopedStatement :: Env -> ScopedStatement -> Either String Env
checkScopedStatement env (ScopedExpression expr) = do _ <- checkExpression env expr; return env
checkScopedStatement env (ScopedBinding (ValueBinding { identifier, typeExpr }))
  = bindType env (identifier, typeExpr)

addable :: TypeExpression -> Bool
addable TypeInt = True
addable _ = False

checkExpression :: Env -> Expression -> Either String TypeExpression
checkExpression env (ExprNothing) = Right TypeNothing
checkExpression env (ExprInt  _)  = Right TypeInt
checkExpression env (ExprBool _)  = Right TypeBool
checkExpression env (ExprIdentifier name)
  = maybe (Left $ unwords ["Undeclared variable", name]) Right $ Map.lookup name env
checkExpression env (ExprScope statements ret)
  = do scopedEnv <- foldl' (\e s -> e >>= (flip checkScopedStatement) s) (Right env) statements
       maybe (Right TypeNothing) (checkExpression scopedEnv) ret
checkExpression env (ExprAdd left right)
  = do leftType  <- checkExpression env left
       rightType <- checkExpression env right
       let ok = (leftType == rightType) && (addable leftType) && (addable rightType)
       if ok then Right leftType else Left "Types added together must be the same type and addable"

checkFunction :: Env -> AST -> Either String ()
checkFunction globalEnv (TopLevelFunction func@(FunctionBinding { identifier
                                                                , typeExpr
                                                                , parameterNames
                                                                , body }))
       -- Type of function should be a function type, of course
  = do (parameterTypes, returnType) <- expectFunction typeExpr
       -- Construct local env
       let localEnv = Map.fromList $ (identifier, typeExpr) : zip parameterNames parameterTypes
       -- Expect body to evaluate to return type
       bodyType <- checkExpression (localEnv `Map.union` globalEnv) body
       unless (bodyType == returnType)
         (Left $ unwords ["Expected", identifier, "to return", show returnType, "but instead it returns", show bodyType])
       -- No issues, we're good!
       return ()
  where expectFunction :: TypeExpression -> Either String ([TypeExpression], TypeExpression)
        expectFunction (TypeFunction params ret) = Right (params, ret)
        expectFunction _ = Left $ unwords ["Internal Error: Somehow the function", identifier, "was assigned a non-function type."]
checkFunction _ _ = Right () -- Ignore all toplevel declarations that aren't functions

checkDirective :: Directive -> Either String ()
checkDirective (Directive { name = "entry", arguments })
  = case arguments of
      [ExprIdentifier ident] -> return ()
      _ -> Left "@entry directive takes one identifier as its argument"
checkDirective (Directive { name })
  = Left $ concat ["No such directive '", name, "' exists"]

checkBindings :: AST -> Env -> Either String Env
checkBindings (TopLevelVar (ValueBinding { identifier
                                         , typeExpr
                                         , expr })) env
  = do env <- bindType env (identifier, typeExpr)
       return env
checkBindings (TopLevelFunction func@(FunctionBinding { identifier
                                                      , typeExpr
                                                      , parameterNames
                                                      , body })) env
  = do env <- bindType env (identifier, typeExpr)
       return env
checkBindings (TopLevelDirective _) env = return env

typecheck :: [AST] -> Either String ()
typecheck ast = do
  -- Check all toplevel directives for correctness
  mapM_ checkDirective [d | TopLevelDirective d <- ast]
  -- Generate an environment based on toplevel bindings
  env <- foldl' (\env a -> env >>= checkBindings a) (Right Map.empty) ast
  -- Descend into function bodies and typecheck based on toplevel environment
  (const ()) <$> mapM_ (checkFunction env) ast
