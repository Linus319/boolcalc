import Evaluator
import Syntax
import Examples

main :: IO ()
main = do
    let env = [("a", True), ("b", False), ("c", True)]

    putStrLn $ "Show e1: " ++ show e1
    putStrLn $ "Result for e1: " ++ show (evalProgram (Program (PlainE1 e1)) env) ++ "\n"

    putStrLn $ "Show e2: " ++ show e2
    putStrLn $ "Result for e2: " ++ show (evalProgram (Program e2) env) ++ "\n"

    putStrLn $ "Show e3: " ++ show e3
    putStrLn $ "Result for e3: " ++ show (evalProgram (Program e3) env) ++ "\n"

    putStrLn $ "Show e4: " ++ show e4
    putStrLn $ "Result for e4: " ++ show (evalProgram (Program e4) env) ++ "\n"

    putStrLn $ "Show e5: " ++ show e5
    putStrLn $ "Result for e5: " ++ show (evalProgram (Program e5) env) ++ "\n"

    putStrLn $ "Show e6: " ++ show e6
    putStrLn $ "Result for e6: " ++ show (evalProgram (Program e6) env) ++ "\n"

    putStrLn $ "Show e7: " ++ show e7
    putStrLn $ "Result for e7: " ++ show (evalProgram (Program e7) env) ++ "\n"

    putStrLn $ "Show e8: " ++ show e8
    putStrLn $ "Result for e8: " ++ show (evalProgram (Program (PlainE1 e8)) env) ++ "\n"

    putStrLn $ "Show e9: " ++ show e9
    putStrLn $ "Result for e9: " ++ show (evalProgram (Program (PlainE1 $ PlainE2 e9)) env) ++ "\n"

    putStrLn $ "Show e10: " ++ show e10
    putStrLn $ "Result for e10: " ++ show (evalProgram (Program e10) env) ++ "\n"



