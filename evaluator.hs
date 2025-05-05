module Evaluator where
import Examples
import Syntax
import Data.List
import Control.Monad

getVars :: E -> [String]
getVars (Paren e) = getVars e
getVars (EqE e1 e2) = getVars e1 ++ getVars e2
getVars (NEqE e1 e2) = getVars e1 ++ getVars e2
getVars (PlainE1 e1) = getVarsE1 e1

getVarsE1 :: E1 -> [String]
getVarsE1 (PlainE2 e2) = getVarsE2 e2
getVarsE1 (OrE e1 e2) = getVarsE1 e1 ++ getVarsE2 e2

getVarsE2 :: E2 -> [String]
getVarsE2 (PlainE3 e3) = getVarsE3 e3
getVarsE2 (MulE e2 e3) = getVarsE2 e2 ++ getVarsE3 e3
getVarsE2 (AndE e2 e3) = getVarsE2 e2 ++ getVarsE3 e3

getVarsE3 :: E3 -> [String]
getVarsE3 (NotE e3) = getVarsE3 e3
getVarsE3 (PlainE4 e4) = case e4 of
    BoolB (VarB x) -> [x]
    BaseE e -> getVars e

evalProgram :: Program -> Result
evalProgram (Program e) = 
    let vars = nub (getVars e)
        combinations = replicateM (length vars) [True, False]
        evalCombination combo = 
            let env = zip vars combo
            in case eval e env of
                ValidTable vars [[b]] -> combo ++ [b]
                _ -> error "Unexpected evaluation result"
        table = map evalCombination combinations
        header = vars ++ ["Result"]
    in ValidTable header table


evalWithVars :: E -> [String] -> [Bool] -> [Bool]
evalWithVars e vars combo = 
    let env = zip vars combo
    in case eval e env of
        ValidTable _ [[b]] -> combo ++ [b]
        ValidTable _ _ -> error "unexpected table structure in evalWithVars"
        Invalid msg -> error msg

eval :: E -> [(String, Bool)] -> Result
eval (Paren e) env = eval e env
eval (EqE e1 e2) env = 
    case (eval e1 env, eval e2 env) of 
        (ValidTable _ [[v1]], ValidTable _ [[v2]]) -> ValidTable [] [[v1 == v2]]
        _ -> Invalid "Mismatched truth table dimnsions"
eval (NEqE e1 e2) env = 
    case (eval e1 env, eval e2 env) of
        (ValidTable _ [[v1]], ValidTable _ [[v2]]) -> ValidTable [] [[v1 /= v2]]
        _ -> Invalid "Mismatched truth table dimensions"
eval (PlainE1 e1) env = evalE1 e1 env

evalE1 :: E1 -> [(String, Bool)] -> Result
evalE1 (PlainE2 e2) env = evalE2 e2 env
evalE1 (OrE e1 e2) env = 
    case (evalE1 e1 env, evalE2 e2 env) of
        (ValidTable header1 [[v1]], ValidTable header2 [[v2]]) -> ValidTable (header1 ++ header2) [[v1 || v2]]
        _ -> Invalid "Mismatched truth table dimensions"

evalE2 :: E2 -> [(String, Bool)] -> Result
evalE2 (MulE e1 e2) env = 
    case (evalE2 e1 env, evalE3 e2 env) of
        (ValidTable header1 [[v1]], ValidTable header2 [[v2]]) -> ValidTable (header1 ++ header2) [[v1 && v2]]
        _ -> Invalid "mul e1 e2 failed"
evalE2 (AndE e1 e2) env = 
    case (evalE2 e1 env, evalE3 e2 env) of
        (ValidTable header1 [[v1]], ValidTable header2 [[v2]]) -> ValidTable (header1 ++ header2) [[v1 && v2]]
        _ -> Invalid "and e1 e2 failed"
evalE2 (PlainE3 e3) env = evalE3 e3 env

evalE3 :: E3 -> [(String, Bool)] -> Result
evalE3 (NotE e3) env = 
    case evalE3 e3 env of 
        ValidTable header [[v]] -> ValidTable header [[not v]]
        _ -> Invalid "not e3 failed"
evalE3 (PlainE4 e4) env = evalE4 e4 env

evalE4 :: E4 -> [(String, Bool)] -> Result
evalE4 (BaseE e) env = eval e env
evalE4 (BoolB b) env = 
    case b of
        VarB x -> case lookup x env of 
            Just v -> ValidTable [x] [[v]]
            Nothing -> Invalid ("Undefined variable: " ++ x)