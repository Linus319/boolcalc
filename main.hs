module Main where

import Parser
import Evaluator
import Syntax
import Text.Parsec (parse)

main :: IO ()
main = do
    putStrLn "Enter a boolean expression:"
    input <- getLine
    case parse parseProgram "" input of
        Left err -> putStrLn ("Parse error: " ++ show err)
        Right program -> do
            let result = evalProgram program
            putStrLn "Truth Table:"
            print result