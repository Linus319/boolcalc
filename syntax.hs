{-
S -> E						            (program) 
E -> (E) | E == E | E != E | E1			(base expression) 
E1 -> E1 + E2 | E2				        (or expression) 
E2 -> E2 * E3 | E2 E3 | E3			    (and expression) 
E3 -> !E3 | E4					        (not expression) 
E4 -> E | B                             (boolean expression) 
B -> (a-z)			                    (boolean value) 
-}

module Syntax where
data B = VarB String deriving (Eq, Ord)
data E = Paren E | EqE E E | NEqE E E | PlainE1 E1
data E1 = OrE E1 E2 | PlainE2 E2
data E2 = MulE E2 E3 | AndE E2 E3 | PlainE3 E3
data E3 = NotE E3 | PlainE4 E4
data E4 = BaseE E | BoolB B

newtype Program = Program E
-- type Env = [(String, Bool)]
data Result = ValidTable [[Bool]] | Invalid String

instance Show E where
    show (Paren e) = "(" ++ show e ++ ")"
    show (EqE e1 e2) = show e1 ++ "==" ++ show e2
    show (NEqE e1 e2) = show e1 ++ "!=" ++ show e2
    show (PlainE1 e1) = show e1

instance Show E1 where
    show (OrE e1 e2) = show e1 ++ "+" ++ show e2
    show (PlainE2 e2) = show e2

instance Show E2 where
    show (MulE e1 e2) = show e1 ++ "*" ++ show e2
    show (AndE e1 e2) = show e1 ++ show e2
    show (PlainE3 e3) = show e3

instance Show E3 where
    show (NotE e3) = "!" ++ show e3
    show (PlainE4 e4) = show e4

instance Show E4 where
    show (BaseE e) = show e
    show (BoolB b) = show b

instance Show B where 
    show (VarB x) = x

instance Show Program where 
    show (Program e) = show e

instance Show Result where
    show (ValidTable b) = show "Valid: " ++ show b
    show (Invalid msg) = show "Invalid: " ++ msg