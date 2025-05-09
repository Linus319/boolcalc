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

parseNot :: Parser E3
parseNot = do
    char '!'
    NotE <$> parseE3

parseAnd :: Parser E2
parseAnd = do
    e3 <- parseE3
    rest (PlainE3 e3)
  where
    rest e2 = (do
        spaces
        char '*'
        spaces
        e3 <- parseE3
        rest (AndE e2 e3)) <|> return e2

parseOr :: Parser E1
parseOr = do
    spaces
    e2 <- parseE2
    rest (PlainE2 e2)
  where
    rest e1 = (do
        spaces
        char '+'
        spaces
        e2 <- parseE2
        rest (OrE e1 e2)) <|> return e1

parseE4 :: Parser E4
parseE4 = do
    spaces
    e4 <- (BaseE <$> parseParen) <|> parseBoolB
    spaces
    return e4

parseE3 :: Parser E3
parseE3 = parseNot <|> (PlainE4 <$> parseE4)

parseE2 :: Parser E2
parseE2 = parseAnd <|> (PlainE3 <$> parseE3)

parseE1 :: Parser E1
parseE1 = parseOr <|> (PlainE2 <$> parseE2)

parseParen :: Parser E
parseParen = do
    char '('
    e <- parseE1
    char ')'
    return (Paren (PlainE1 e))

parseExpr :: Parser E
parseExpr = do
    spaces
    expr <- (PlainE1 <$> parseE1)
    spaces
    eof
    return expr

parseProgram :: Parser Program
parseProgram = do
    spaces
    e <- parseE1
    spaces
    eof
    return (Program (PlainE1 e))