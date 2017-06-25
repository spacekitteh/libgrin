{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, EmptyDataDecls,  TypeInType, TypeOperators, ExistentialQuantification, RankNTypes, DefaultSignatures,
GADTs, DuplicateRecordFields, PatternSynonyms, DeriveTraversable, DeriveGeneric, DeriveDataTypeable, DeriveLift, StandaloneDeriving, DeriveAnyClass #-}

module GRIN.Parser where

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L


import Data.Functor


import GRIN.Syntax
import Control.Monad.Freer.Fresh

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'parens' parses something between parenthesis.

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'integer' parses an integer.

integer :: Parser Integer
integer = lexeme L.integer

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws = ["unit", "store", "fetch", "update", "case", "call", "eval", "apply", "fetchnode", "alloc", "free", "fetchupdate", "fetchfield", "phi", "fixpoint"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

val :: Parser (Value String)
val = SimpleValue <$> sVal

sVal :: Parser (SimpleValue String)
sVal = (VariableValue <$> var) <|> (LiteralValue <$> lit) 

hole :: Parser (Variable String)
hole = do
  rword "_"
  return Hole

var :: Parser (Variable String)
var = hole <|> (do {a <- identifier;  return (Var a (Just a))})


lit :: Parser Literal
lit =  LitInteger <$> L.signed sc integer


expr :: Parser (Expression String)
expr =  try bind <|> try (parens expr) <|> caseExpr <|> (SimpleExpr <$> sExpr)

bind :: Parser (Expression String)
bind = do
  a <- sExpr
  string ";" *> sc
  string "\\" *> sc
  pat <- lambdaPattern
  rword "->"
  b <- expr
  return (Seq a pat b)

lambdaPattern :: Parser (LambdaPattern String)
lambdaPattern = ValuePattern <$> val

caseExpr :: Parser (Expression String)
caseExpr = do
  rword "case"
  v <- val
  rword "of"
  alts <- some alternative
  return (Case v alts)

alternative :: Parser (CaseAlternative String)
alternative = do
  pat <- constantPattern
  rword "->"
  exp <- expr
  return (Alternative pat exp)

constantPattern :: Parser (ConstantPattern String)
constantPattern = (LiteralPattern <$> lit) <|> (TagPattern <$> knownTag)

knownTag :: Parser Tag
knownTag = do
  t <- identifier
  case (head t) of
    'F' -> return $ FunctionCall (FunctionName t) Nothing
    'C' -> return $ Constructor (ConstructorName t) Nothing
    'P' -> return $ PartialApplication (FunctionName t) Nothing
  
  
sExpr :: Parser (SimpleExpression String)
sExpr =  application <|> unit <|> store <|> fetchNode <|> (NestedExpression <$> parens expr)



{-sOperators :: [[Operator Parser (GrinSimpleExpression a)]]
sOperators =
  [ [-}


application :: Parser (SimpleExpression String)
application = do
  rword "apply"
  f <- var
  args <- many val
  return (Apply f args)
unit :: Parser (SimpleExpression String)
unit = do
  rword "unit"
  v <- val
  return (Unit v)

store :: Parser (SimpleExpression String)
store = do
  rword "store"
  v <- val
  return (Store v)

fetchNode :: Parser (SimpleExpression String)
fetchNode = do
  rword "fetchnode"
  v <- var
  return (FetchNode v)


  
