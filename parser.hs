module Parser (parseProgram) where

import Syntax
import Text.Parsec -- char, string, spaces, <|>, do
import Text.Parsec.String (Parser) -- Parser type
import Debug.Trace

-- confirmed good
parseVar :: Parser B
parseVar = do
    x <- oneOf ['a'..'z']
    return (VarB [x])

-- confirmed good
parseBoolB :: Parser E4
parseBoolB = do
    BoolB <$> parseVar

-- confirmed good
parseNot :: Parser E3
parseNot = do
    char '!'
    NotE <$> parseE3

-- confirmed good
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

-- confirmed good
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

-- confirmed good
parseE4 :: Parser E4
parseE4 = do
    spaces
    e4 <- (BaseE <$> parseParen) <|> parseBoolB
    spaces
    return e4

-- confirmed good
parseE3 :: Parser E3
parseE3 = parseNot <|> (PlainE4 <$> parseE4)

-- confirmed good
parseE2 :: Parser E2
parseE2 = parseAnd <|> (PlainE3 <$> parseE3)

-- confirmed good
parseE1 :: Parser E1
parseE1 = parseOr <|> (PlainE2 <$> parseE2)

-- confirmed good
parseParen :: Parser E
parseParen = do
    char '('
    e <- parseE1
    char ')'
    return (Paren (PlainE1 e))

-- works, exept it allows examples like "x == y == z"
parseEq :: Parser E
parseEq = do
    e1 <- parseE1
    spaces
    string "=="
    spaces
    e2 <- parseE1
    return (EqE (PlainE1 e1) (PlainE1 e2))

-- works except it allows examples like "x != y != z"
parseNEq :: Parser E
parseNEq = do
    e1 <- parseE1
    spaces
    try (string "!=")
    spaces
    e2 <- parseE1
    return (NEqE (PlainE1 e1) (PlainE1 e2))

parseExpr :: Parser E
parseExpr = do
    spaces
    expr <- parseEq <|> parseNEq <|> (PlainE1 <$> parseE1)
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