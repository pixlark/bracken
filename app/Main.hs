{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.Bifunctor as Bifunctor
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type LangParser = Parsec Void String

data TypeExpression = TypeInt
                    | TypeString
                    | TypeBool
                    | TypeIdentifier String
                    | TypeArrow TypeExpression TypeExpression
  deriving(Show)

data Expression = ExprUnit
                | ExprInt Int
                | ExprString String
                | ExprBool Bool
                | ExprIdentifier String
                | ExprAdd Expression Expression
  deriving(Show)

data AST = TypeBinding  { identifier :: String
                        , typeExpr :: TypeExpression }
         | ValueBinding { identifier :: String
                        , typeExpr :: TypeExpression
                        , parameterList :: Maybe [String]
                        , expr :: Expression }
  deriving(Show)

--
-- Utility
--

combineEither :: Either a a -> a
combineEither (Left x)  = x
combineEither (Right x) = x

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
reservedWords = ["type", "define", "int", "string", "bool", "as", "unit", "true", "false"]

identifierParser :: LangParser String
identifierParser = do
  c <- (letterChar <|> (char '_'))
  cs <- many identifierChar
  let str = (c:cs) in
    if str `elem` reservedWords
    then fail $ show str ++ " is a reserved word"
    else return str

--
-- Type expressions
--

typeAtomParser :: LangParser TypeExpression
typeAtomParser = try recursiveParser
             <|> try builtinParser
             <|> bindingParser
  where recursiveParser = parentheses typeExpressionParser
        builtinParser =
          let builtin s c = reservedWord s >> return c
          in    builtin "int" TypeInt
            <|> builtin "string" TypeString
            <|> builtin "bool" TypeBool
        bindingParser = lexeme identifierParser >>= return . TypeIdentifier

arrowOperator :: LangParser TypeExpression
arrowOperator = rightAssocOperator "->" TypeArrow typeAtomParser

typeExpressionParser :: LangParser TypeExpression
typeExpressionParser = arrowOperator

--
-- Expressions
--

expressionAtomParser :: LangParser Expression
expressionAtomParser = try recursiveParser
                   <|> try unitParser
                   <|> try intParser
                   <|> try boolParser
                   <|> try stringParser
                   <|> try bindingParser
  where recursiveParser = parentheses expressionParser
        unitParser = reservedWord "unit" >> return ExprUnit
        intParser = do
          ds <- lexeme $ some digitChar
          return $ ExprInt $ read ds
        boolParser = do
          b <- reservedWord "true" <|> reservedWord "false"
          return $ case b of
            "true"  -> ExprBool True
            "false" -> ExprBool False
        stringParser = do
          _ <- symbol "\""
          str <- manyTill (satisfy $ const True) (symbol "\"")
          return $ ExprString str
        bindingParser = lexeme identifierParser >>= return . ExprIdentifier

additionParser :: LangParser Expression
additionParser = leftAssocOperator "+" ExprAdd expressionAtomParser

expressionParser :: LangParser Expression
expressionParser = additionParser

--
-- Toplevel bindings
--

typeBindingParser :: LangParser AST
typeBindingParser = do
  identifier <- lexeme identifierParser
  typeExpr <- squareBrackets $ typeExpressionParser
  return TypeBinding { identifier, typeExpr }

valueBindingParser :: LangParser AST
valueBindingParser = do
  identifier <- lexeme identifierParser
  typeExpr <- squareBrackets $ typeExpressionParser
  parameterList <- try $ optional parameterListParser
  reservedWord "as"
  expr <- expressionParser
  return ValueBinding { identifier, typeExpr, parameterList, expr }
    where parameterListParser = parentheses $ sepEndBy1 (lexeme identifierParser) (string ",")

toplevelParser :: LangParser AST
toplevelParser = do
  defn <- (reservedWord "type") <|> (reservedWord "define")

  inner <- case defn of
    "type" -> typeBindingParser
    "define" -> valueBindingParser

  space
  return inner

langParser :: LangParser [AST]
langParser = do
  toplevels <- many toplevelParser
  eof
  return toplevels

main :: IO ()
main = interact
     $ (++ "\n") . combineEither
     . (Bifunctor.bimap ("Error: " ++) ("Result: " ++))
     . (Bifunctor.bimap errorBundlePretty show)
     . (runParser langParser "(stdin)")
