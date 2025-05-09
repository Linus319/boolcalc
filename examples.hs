module Examples where
import Syntax

var1, var2, var3, var4 :: B
var1= VarB "a"
var2 = VarB "b"
var3 = VarB "c"
var4 = VarB "d"

-- ab
e1 = AndE (PlainE3 (PlainE4 (BoolB (VarB "a")))) (PlainE4 (BoolB (VarB "b")))
p1 = Program (PlainE1 (PlainE2 e1))

-- (a+(b+(c+d)))
e5 = Paren $ PlainE1 $ PlainE2 $ PlainE3 $ PlainE4 $ BaseE $ PlainE1 $
    OrE
        (PlainE2 $ PlainE3 $ PlainE4 $ BoolB var1)
        (PlainE3 $ PlainE4 $ BaseE $ Paren $ PlainE1 $ PlainE2 $ PlainE3 $ PlainE4 $ BaseE $ PlainE1 $
            OrE
                (PlainE2 $ PlainE3 $ PlainE4 $ BoolB var2)
                (PlainE3 $ PlainE4 $ BaseE $ Paren $ PlainE1 $ PlainE2 $ PlainE3 $ PlainE4 $ BaseE $ PlainE1 $
                    OrE
                        (PlainE2 $ PlainE3 $ PlainE4 $ BoolB var3)
                        (PlainE3 $ PlainE4 $ BoolB var4)))
p5 = Program e5

-- ((a(b))c)
e7 = Paren $ 
    PlainE1 $ PlainE2 $ AndE
        (PlainE3 $ PlainE4 $ BaseE $ Paren $ PlainE1 $ PlainE2 $ AndE
            (PlainE3 $ PlainE4 $ BoolB var1)
            (PlainE4 $ BaseE $ Paren $ PlainE1 $ PlainE2 $ PlainE3 $ PlainE4 $ BoolB var2))
        (PlainE4 $ BoolB var3)
p7 = Program e7

-- ab+c
e8 = OrE
    (PlainE2 $ AndE
        (PlainE3 $ PlainE4 $ BoolB var1)
        (PlainE4 $ BoolB var2))
    (PlainE3 $ PlainE4 $ BoolB var3)
p8 = Program (PlainE1 e8)

-- (a+b)*c
e9 = MulE
    (PlainE3 $ PlainE4 $ BaseE $ Paren $ PlainE1 $ OrE
        (PlainE2 $ PlainE3 $ PlainE4 $ BoolB var1)
        (PlainE3 $ PlainE4 $ BoolB var2))
    (PlainE4 $ BoolB var3)
p9 = Program (PlainE1 (PlainE2 e9))