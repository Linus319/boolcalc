import Control.Monad (replicateM, forM_)

-- Generate all possible combinations of boolean values for n variables
boolCombinations :: Int -> [[Bool]]
boolCombinations n = replicateM n [False, True]

-- Apply a logical operation to a list of boolean values
applyExpression :: [Bool] -> Bool
applyExpression [p, q, r, s] = (p || q) && (r || s)

-- Generate and print the truth table for a given expression
printTruthTable :: IO ()
printTruthTable = do
    let combinations = boolCombinations 4
    putStrLn "P  | Q  | R  | S  | (P v Q) ^ (R v S)"
    putStrLn "----------------------------------------"
    forM_ combinations $ \[p, q, r, s] -> do
        let result = applyExpression [p, q, r, s]
        putStrLn $ show p ++ "  | " ++ show q ++ "  | " ++ show r ++ "  | " ++ show s ++ "  || " ++ show result

main :: IO ()
main = printTruthTable
