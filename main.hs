module Main where

import Parser
import Evaluator
import Syntax
import Text.Parsec (parse)

-- Parse the input string and evaluate the truth table
parseAndEvaluate :: String -> IO ()
parseAndEvaluate input = 
    case parse parseProgram "" input of
        Left err -> putStrLn $ "Error: " ++ show err
        Right program -> do
            let result = evalProgram program
            putStrLn $ "Truth table for: " ++ input
            putStrLn $ show result

main :: IO ()
main = do
    putStrLn "Enter a boolean expression (or type 'exit' to quit):"
    input <- getLine
    if input == "exit"
        then putStrLn "Goodbye!"
        else do
            parseAndEvaluate input
            main