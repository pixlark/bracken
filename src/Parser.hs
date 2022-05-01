{-# LANGUAGE DuplicateRecordFields
           , NamedFieldPuns #-}

module Parser (langParser
              , TypeExpression(..)
              , Expression(..)
              , ScopedStatement(..)
              , ValueBinding(..)
              , FunctionBinding(..)
              , Directive(..)
              , AST(..)) where

import Data.Maybe (fromMaybe)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type LangParser = Parsec Void String

data TypeExpression = TypeNothing
                    | TypeInt
                    | TypeBool
                    | TypeFunction [TypeExpression] TypeExpression
  deriving(Show, Eq)

data ScopedStatement = ScopedExpression Expression
                     | ScopedBinding    ValueBinding
  deriving(Show)

data Expression = ExprNothing
                | ExprInt Int
                | ExprBool Bool
                | ExprIdentifier String
                | ExprScope [ScopedStatement] (Maybe Expression)
                | ExprAdd Expression Expression
  deriving(Show)

-- TODO(Brooke): Make explicit types optional (type inference)
data ValueBinding = ValueBinding { identifier :: String
                                 , typeExpr :: TypeExpression
                                 , expr :: Expression }
  deriving(Show)

data FunctionBinding = FunctionBinding { identifier :: String
                                       , typeExpr :: TypeExpression
                                       , parameterNames :: [String]
                                       , body :: Expression }
  deriving(Show)

data Directive = Directive { name :: String
                           , arguments :: [Expression] }
  deriving(Show)

data AST = TopLevelVar       ValueBinding
         | TopLevelFunction  FunctionBinding
         | TopLevelDirective Directive
  deriving(Show)

--
-- Utility
--

spaceConsumer = Lexer.space space1 empty empty

symbol = Lexer.symbol spaceConsumer
lexeme = Lexer.lexeme spaceConsumer

parentheses    = between (symbol "(") (symbol ")")
squareBrackets = between (symbol "[") (symbol "]")
curlyBrackets  = between (symbol "{") (symbol "}")

rightAssocOperator :: String -> (a -> a -> a) -> LangParser a -> LangParser a
rightAssocOperator s f p = do
  args <- sepBy1 p (symbol s)
  return $ foldr1 f args

leftAssocOperator :: String -> (a -> a -> a) -> LangParser a -> LangParser a
leftAssocOperator s f p = do
  args <- sepBy1 p (symbol s)
  return $ foldl1 f args

endedBy :: LangParser a -> LangParser b -> LangParser a
a `endedBy` b = do
  x <- a
  b
  return x

--
-- Identifiers and reserved words
--

identifierChar :: LangParser Char
identifierChar = alphaNumChar <|> (char '_')

reservedWord :: String -> LangParser String
reservedWord word = do
  space
  rword <- string word
  notFollowedBy identifierChar
  space
  return rword

reservedWords :: [String]
reservedWords = ["let", "true", "false", "func", "do", "nothing"]

identifierParser :: LangParser String
identifierParser = do
  c <- (letterChar <|> (char '_'))
  cs <- many identifierChar
  let str = (c:cs) in
    if str `elem` reservedWords
    then fail $ show str ++ " is a reserved word"
    else return str

directiveStart :: LangParser String
directiveStart = lexeme $ ((\x -> [x]) <$> char '@')

--
-- Type expressions
--

typeAtomParser :: LangParser TypeExpression
typeAtomParser = try functionParser
             <|> try recursiveParser
             <|>     builtinParser
  where recursiveParser = parentheses typeExpressionParser
        functionParser = do
          reservedWord "func"
          params <- parentheses (sepEndBy typeExpressionParser (symbol ","))
          symbol "->"
          ret <- typeExpressionParser
          return $ TypeFunction params ret
        builtinParser =
          let builtin s c = reservedWord s >> return c
          in    builtin "int"     TypeInt
            <|> builtin "bool"    TypeBool
            <|> builtin "nothing" TypeNothing

typeExpressionParser :: LangParser TypeExpression
typeExpressionParser = typeAtomParser

--
-- Expressions
--

expressionAtomParser :: LangParser Expression
expressionAtomParser = try recursiveParser
                   <|> try nothingParser
                   <|> try intParser
                   <|> try boolParser
                   <|> try bindingParser
                   <|>     scopeParser
  where recursiveParser = parentheses expressionParser
        nothingParser = reservedWord "nothing" >> return ExprNothing
        intParser = do
          ds <- lexeme $ some digitChar
          return $ ExprInt $ read ds
        boolParser = do
          b <- reservedWord "true" <|> reservedWord "false"
          return $ case b of
            "true"  -> ExprBool True
            "false" -> ExprBool False
        bindingParser = lexeme identifierParser >>= return . ExprIdentifier
        scopeParser = curlyBrackets $ do
          statements <- many $ try $ scopedStatementParser >>= (\x -> (symbol ";") >> return x)
          returnExpression <- optional expressionParser
          return $ ExprScope statements returnExpression

additionParser :: LangParser Expression
additionParser = leftAssocOperator "+" ExprAdd expressionAtomParser

expressionParser :: LangParser Expression
expressionParser = additionParser

scopedStatementParser :: LangParser ScopedStatement
scopedStatementParser =  (reservedWord "var" >> ScopedBinding <$> valueBindingParser)
                     <|> (ScopedExpression <$> expressionParser)

--
-- TopLevel bindings
--

-- For a var declaration, parses:
-- 'var name : type = value ;'
--      ^^^^^^^^^^^^^^^^^^^
-- The 'var' and ';' are handled further up.
valueBindingParser :: LangParser ValueBinding
valueBindingParser = do
  identifier <- lexeme identifierParser
  symbol ":"
  typeExpr <- typeExpressionParser
  symbol "="
  expr <- expressionParser
  return ValueBinding { identifier, typeExpr, expr }

-- For a func declaration, parses:
-- 'func name(args) -> Type do expr'
--       ^^^^^^^^^^^^^^^^^^^^^^^^^^
-- The 'func' is handled further up.
functionBindingParser :: LangParser FunctionBinding
functionBindingParser = do
  identifier <- lexeme identifierParser
  parameters <- parameterListParser
  symbol "->"
  returnType <- typeExpressionParser
  reservedWord "do"
  body <- expressionParser
  return FunctionBinding { identifier
                         , typeExpr = TypeFunction (map snd parameters) returnType
                         , parameterNames = map fst parameters
                         , body }
    where parameterParser = do
            identifier <- lexeme identifierParser
            symbol ":"
            typeExpr <- typeExpressionParser
            return (identifier, typeExpr)
          parameterListParser = parentheses $ sepEndBy parameterParser (symbol ",")

-- For a directive, parses:
-- '@directive(a, b, ..)'
--   ^^^^^^^^^^^^^^^^^^^
-- The '@' is handled further up
directiveParser :: LangParser Directive
directiveParser = do
  name <- lexeme identifierParser
  arguments <- fromMaybe []
    <$> (optional $ parentheses (sepEndBy expressionParser (symbol ",")))
  return Directive { name, arguments }

toplevelParser :: LangParser AST
toplevelParser = do
  defn <- reservedWord "var" <|> reservedWord "func" <|> directiveStart

  inner <- case defn of
    "var"  -> (valueBindingParser `endedBy` (symbol ";"))
          >>= return . TopLevelVar
    "func" -> functionBindingParser
          >>= return . TopLevelFunction
    "@"    -> directiveParser
          >>= return . TopLevelDirective

  space
  return inner

langParser :: LangParser [AST]
langParser = do
  toplevels <- many toplevelParser
  eof
  return toplevels
