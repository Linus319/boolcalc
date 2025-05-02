module Evaluator where
import Examples
import Syntax

evalProgram :: Program -> Env -> Result
evalProgram (Program e) env = eval e env

eval :: E -> Env -> Result
eval (Paren e) env = eval e env
eval (EqE e1 e2) env = 
    case (eval e1 env, eval e2 env) of
        (Valid v1, Valid v2) -> Valid (v1 == v2)
        (Invalid msg, _) -> Invalid msg
        (_, Invalid msg) -> Invalid msg
eval (NEqE e1 e2) env =
    case (eval e1 env, eval e2 env) of
        (Valid v1, Valid v2) -> Valid (v1 /= v2)
        (Invalid msg, _) -> Invalid msg
        (_, Invalid msg) -> Invalid msg
eval (PlainE1 e1) env = evalE1 e1 env

evalE1 :: E1 -> Env -> Result
evalE1 (OrE e1 e2) env = 
    case (evalE1 e1 env, evalE2 e2 env) of
        (Valid v1, Valid v2) -> Valid (v1 || v2)
        (Invalid msg, _) -> Invalid msg
        (_, Invalid msg) -> Invalid msg
evalE1 (PlainE2 e1) env = evalE2 e1 env

evalE2 :: E2 -> Env -> Result
evalE2 (MulE e1 e2) env =
    case (evalE2 e1 env, evalE3 e2 env) of 
        (Valid v1, Valid v2) -> Valid (v1 && v2)
        (Invalid msg, _) -> Invalid msg
        (_, Invalid msg) -> Invalid msg
evalE2 (AndE e1 e2) env =
    case (evalE2 e1 env, evalE3 e2 env) of
        (Valid v1, Valid v2) -> Valid (v1 && v2)
        (Invalid msg, _) -> Invalid msg
        (_, Invalid msg) -> Invalid msg
evalE2 (PlainE3 e1) env = evalE3 e1 env

evalE3 :: E3 -> Env -> Result
evalE3 (NotE e3) env = 
    case evalE3 e3 env of
        Valid v -> Valid (not v)
        Invalid msg -> Invalid msg
evalE3 (PlainE4 e4) env = evalE4 e4 env

evalE4 :: E4 -> Env -> Result
evalE4 (BaseE e) env = eval e env
evalE4 (BoolB b) env = evalB b env

evalB :: B -> Env -> Result
evalB (VarB x) env = 
    case lookup x env of
        Just b -> Valid b
        Nothing -> Invalid $ "Undefined variable: " ++ x