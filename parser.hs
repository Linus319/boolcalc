module Parser (parseProgram) where

import Syntax
import Text.Parsec
import Text.Parsec.String (Parser)

parseVar :: Parser B
parseVar = do
    x <- oneOf ['a'..'z']
    return (VarB [x])

parseBoolB :: Parser E4
parseBoolB = do
    BoolB <$> parseVar

parseParen :: Parser E
parseParen = do
    char '('
    e <- parseExpr
    char ')'
    return (Paren e)

parseNot :: Parser E3
parseNot = do
    char '!'
    NotE <$> parseE3

parseAnd :: Parser E2
parseAnd = do
    e2 <- parseE2
    spaces
    char '*'
    spaces
    AndE e2 <$> parseE3

parseOr :: Parser E1
parseOr = do
    e1 <- parseE1
    spaces
    char '+'
    spaces
    OrE e1 <$> parseE2

parseEq :: Parser E
parseEq = do
    e1 <- parseExpr
    spaces
    string "=="
    spaces
    EqE e1 <$> parseExpr

parseNEq :: Parser E
parseNEq = do
    e1 <- parseExpr
    spaces
    string "!="
    spaces
    NEqE e1 <$> parseExpr

parseE4 :: Parser E4
parseE4 = (BaseE <$> parseParen) <|> parseBoolB

parseE3 :: Parser E3
parseE3 = parseNot <|> (PlainE4 <$> parseE4)

parseE2 :: Parser E2
parseE2 = parseAnd <|> (PlainE3 <$> parseE3)

parseE1 :: Parser E1
parseE1 = parseOr <|> (PlainE2 <$> parseE2)

parseExpr :: Parser E
parseExpr = parseEq <|> parseNEq <|> parseParen <|> (PlainE1 <$> parseE1)

parseProgram :: Parser Program
parseProgram = do
    spaces
    e <- parseExpr
    spaces
    return (Program e)