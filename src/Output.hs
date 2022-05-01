{-# LANGUAGE DuplicateRecordFields
           , OverloadedStrings
           , NamedFieldPuns
           , FlexibleInstances #-}

module Output (compileSource, emitC, CompileFlag(..), CompileFlags, makeFlags) where

import Control.Monad (sequence)
import Data.Either (lefts, rights)
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (pack)

import Parser
import qualified DAG as DAG

import Debug.Trace

--
-- Emitting C ast as source code
--

class Emit a where
  emit :: a -> Text.Text

  emitAll :: [a] -> Text.Text
  emitAll s = Text.concat $ map emit s

  -- First `emit`s all members of the list before separating
  emitSeparated :: [a] -> String -> Text.Text
  emitSeparated s sep = separated (map emit s) sep

separated :: [Text.Text] -> String -> Text.Text
separated s sep = Text.concat $ intersperse (pack sep) $ s

-- Prevent function/variable names from clashing with important
-- runtime names
newtype Sanitized = Sanitized String
sanitize :: String -> Sanitized
sanitize name = Sanitized $ '_':name

instance Emit Sanitized where
  emit (Sanitized s) = pack s

data FuncAttribute = FuncStatic | FuncExtern
  deriving (Eq, Ord)

instance Emit FuncAttribute where
  emit FuncStatic = "static"
  emit FuncExtern = "extern"

data VarAttribute = VarConst | VarStatic | VarExtern
  deriving (Eq, Ord)

instance Emit VarAttribute where
  emit VarConst  = "const"
  emit VarStatic = "static"
  emit VarExtern = "extern"

data CTypeExpr = CTypeVoid
               | CTypeInt
               | CTypeFuncPtr (Maybe String) [CTypeExpr] CTypeExpr

instance Emit CTypeExpr where
  emit CTypeVoid = "void"
  emit CTypeInt  = "int64_t"
  emit (CTypeFuncPtr maybeName params ret)
    =  Text.concat [ emit ret
                   , pack "(*"
                   , fromMaybe "" $ (emit . sanitize) <$> maybeName
                   , emitSeparated params ","
                   , pack ")" ]

data CStmt = CVarDeclaration { name     :: String
                             , typeExpr :: CTypeExpr
                             , value    :: CExpr }
           | CStmtExpr CExpr

instance Emit CStmt where
  emit CVarDeclaration { name, typeExpr, value }
    = separated [ emit typeExpr
                , emit $ sanitize name
                , pack "="
                , emit value
                , pack ";" ] " "

data CExpr = CInt        Int
           | CIdentifier String
           | CAdd        CExpr  CExpr

instance Emit CExpr where
  emit (CInt x)        = pack $ show x
  emit (CIdentifier x) = emit $ sanitize x
  emit (CAdd a b)      = separated [emit a, pack "+", emit b] " "

newtype CParameter = CParameter (String, CTypeExpr)

data C = CFunction { name           :: String
                   , parameters     :: [CParameter]
                   , returnType     :: CTypeExpr
                   , funcAttributes :: Set.Set FuncAttribute
                   , body           :: Maybe CExpr }
       | CGlobal   { name          :: String
                   , typeExpr      :: CTypeExpr
                   , varAttributes :: Set.Set VarAttribute
                   , value         :: CExpr }
       | CText     { text :: Text.Text }

instance Emit CParameter where
  emit (CParameter (name, typeExpr)) = separated [emit typeExpr, emit $ sanitize name] " "

instance Emit C where
  emit (CFunction { name, parameters, returnType, funcAttributes, body })
    = Text.unwords [ emitSeparated (Set.toList funcAttributes) " "
                   , emit returnType
                   , emit $ sanitize name
                   , "("
                   , emitSeparated parameters ","
                   , ")"
                   , emittedBody ]
    where emittedBody = case body of
            Nothing -> ";"
            Just expr -> Text.unwords [ "{", "return", emit expr, "; }" ]
  emit (CGlobal {name, typeExpr, varAttributes, value })
    = Text.unwords [ emitSeparated (Set.toList varAttributes) " "
                   , emit typeExpr
                   , emit $ sanitize name
                   , "=", emit value, ";" ]
  emit (CText { text }) = text

--
-- Converting our AST to a C AST
--

instance Eq ValueBinding where
  (ValueBinding { identifier = id1 }) == (ValueBinding { identifier = id2 })
    = id1 == id2

instance Ord ValueBinding where
  (ValueBinding { identifier = id1}) `compare` (ValueBinding { identifier = id2 })
    = id1 `compare` id2

compileTypeExpression :: TypeExpression -> CTypeExpr
compileTypeExpression expr
  = case expr of
      TypeNothing      -> CTypeInt
      TypeInt          -> CTypeInt
      TypeBool         -> CTypeInt
      TypeFunction _ _ -> undefined -- AGHHHH

compileExpression :: Expression -> CExpr
compileExpression expr
  = case expr of
      ExprNothing      -> CInt 0
      ExprInt i        -> CInt i
      ExprBool b       -> CInt $ if b then 1 else 0
      ExprIdentifier i -> CIdentifier i
      ExprScope _ _    -> undefined -- AGHHHH
      ExprAdd a b      -> CAdd (compileExpression a) (compileExpression b)

compileValueBinding :: ValueBinding -> C
compileValueBinding (ValueBinding { identifier, typeExpr, expr })
  = CGlobal { name = identifier
            , varAttributes = Set.empty
            , typeExpr = compileTypeExpression typeExpr
            , value = compileExpression expr }

-- TODO(Brooke): Make sure that this works for any collection of
-- scoped variables, not just toplevel (global) variables
compileVariables :: [ValueBinding] -> Either String [C]
compileVariables vars = map compileValueBinding <$> sortedCVars
        -- Figure out the topological ordering of variable declarations
  where allReferences :: Expression -> [String]
        allReferences (ExprIdentifier i) = [i]
        allReferences (ExprAdd a b) = (allReferences a) ++ (allReferences b)
        allReferences (ExprScope statements maybeRet)
          =  (concat $ map allRefStmt statements)
          ++ (fromMaybe [] $ allReferences <$> maybeRet)
          where allRefStmt (ScopedExpression e) = allReferences e
                allRefStmt _ = []
        allReferences _ = []

        lookupVar :: String -> ValueBinding
        lookupVar i = case filter (\(ValueBinding { identifier } ) -> i == identifier) vars of
                        (v:_) -> v
                        []    -> error "Internal Error: Bad lookupVar!"

        genVarDeps :: [(ValueBinding, ValueBinding)] -> ValueBinding -> [(ValueBinding, ValueBinding)]
        genVarDeps pairs binding@(ValueBinding { expr })
          = [(binding, r) | r <- map lookupVar $ allReferences expr] ++ pairs
        varDeps :: [(ValueBinding, ValueBinding)]
        varDeps = foldl' genVarDeps [] vars

        disconnectedDag = DAG.fromNodes vars
        sortedCVars
          = maybe (Left "Cyclic dependency in global variables!") Right
          $ DAG.order
          $ foldl' (flip DAG.depends) disconnectedDag varDeps

compilePrototype :: FunctionBinding -> Either String C
compilePrototype (FunctionBinding { identifier, typeExpr, parameterNames, body })
  = do (paramTypes, retType) <- case typeExpr of
         TypeFunction p r -> return (p, r)
         _ -> Left "Internal Error: Function was assigned invalid typeExpr"
       return $ CFunction
         { name = identifier
         , parameters = map CParameter $ zip parameterNames (map compileTypeExpression paramTypes)
         , returnType = compileTypeExpression retType
         , funcAttributes = Set.empty
         , body = Nothing
         }

compileFunction :: FunctionBinding -> Either String C
compileFunction f@(FunctionBinding { body }) = do
  prototype <- compilePrototype f
  return (prototype { body = Just $ compileExpression body } :: C)

makeEntryFunction :: [C] -> String -> Either String C
makeEntryFunction prototypes entry =
  case find predicate prototypes of
    Just _ -> Right $ CText $ Text.pack $
      "int main() { " ++ ((\(Sanitized s) -> s) $ sanitize entry) ++ "(); }"
    Nothing -> Left $ "No such zero-argument function " ++ entry ++ " to satisfy @entry directive."
  where predicate (CFunction { name = entry, parameters = [] }) = True
        predicate _ = False

cPrelude :: Text.Text
cPrelude = Text.unlines ["#include <stdint.h>"]

data CompileFlag = NoEntryPoint
  deriving(Eq, Ord)

newtype CompileFlags = CompileFlags (Set.Set CompileFlag)

hasFlag :: CompileFlag -> CompileFlags -> Bool
hasFlag flag (CompileFlags set) = Set.member flag set

makeFlags :: [CompileFlag] -> CompileFlags
makeFlags = CompileFlags . Set.fromList

compileSource :: [AST] -> CompileFlags -> Either String [C]
compileSource declarations flags
  = do entrySymbol <- case [entry | dir@Directive { name = "entry"
                                                  , arguments = [ExprIdentifier entry] } <- directives] of
                        [e] -> Right $ Just e
                        [] -> if hasFlag NoEntryPoint flags
                              then Right Nothing
                              else Left "No @entry directive specified"
                        _  -> Left "Multiple @entry directives specified"
       cVars       <- compileVariables vars
       cPrototypes <- sequence $ map compilePrototype functions
       cFunctions  <- sequence $ map compileFunction  functions
       mainFunc    <- sequenceA $ makeEntryFunction cPrototypes <$> entrySymbol
       return $ concat $ [ cPrototypes
                         , cVars
                         , cFunctions
                         , fromMaybe [] ((\x -> [x]) <$> mainFunc) ]
        -- Divide toplevel items into variables and functions
  where vars       = [v | TopLevelVar       v <- declarations]
        functions  = [f | TopLevelFunction  f <- declarations]
        directives = [d | TopLevelDirective d <- declarations]

emitC :: [C] -> Text.Text
emitC = (Text.append cPrelude) . (flip emitSeparated) "\n"
