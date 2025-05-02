module Examples where
import Syntax

var1, var2, var3, var4 :: B
var1= VarB "a"
var2 = VarB "b"
var3 = VarB "c"
var4 = VarB "d"

-- bc==b*c
e2 = EqE
    (PlainE1 (PlainE2 (AndE (PlainE3 (PlainE4 (BoolB var2))) (PlainE4 (BoolB var3)))))
    (PlainE1 (PlainE2 (MulE (PlainE3 (PlainE4 (BoolB var2))) (PlainE4 (BoolB var3)))))

-- a!=b+c
e3 = NEqE
    (PlainE1 $ PlainE2 $ PlainE3 $ PlainE4 $ BoolB var1)
    (PlainE1 $ OrE
        (PlainE2 $ PlainE3 $ PlainE4 $ BoolB var2)
        (PlainE3 $ PlainE4 $ BoolB var3))

-- abc==a*b*c
e4 = EqE
    (PlainE1 $ PlainE2 $ AndE
        (AndE
            (PlainE3 $ PlainE4 $ BoolB var1)
            (PlainE4 $ BoolB var2))
        (PlainE4 $ BoolB var3))
    (PlainE1 $ PlainE2 $ MulE
        (MulE
            (PlainE3 $ PlainE4 $ BoolB var1)
            (PlainE4 $ BoolB var2))
        (PlainE4 $ BoolB var3))

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

-- a==!!a
e6 = EqE
    (PlainE1 $ PlainE2 $ PlainE3 $ PlainE4 $ BoolB var1)
    (PlainE1 $ PlainE2 $ PlainE3 $ NotE $ NotE $ PlainE4 $ BoolB var1)

-- ((a(b))c)
e7 = Paren $ 
    PlainE1 $ PlainE2 $ AndE
        (PlainE3 $ PlainE4 $ BaseE $ Paren $ PlainE1 $ PlainE2 $ AndE
            (PlainE3 $ PlainE4 $ BoolB var1)
            (PlainE4 $ BaseE $ Paren $ PlainE1 $ PlainE2 $ PlainE3 $ PlainE4 $ BoolB var2))
        (PlainE4 $ BoolB var3)

-- ab+c
e8 = OrE
    (PlainE2 $ AndE
        (PlainE3 $ PlainE4 $ BoolB var1)
        (PlainE4 $ BoolB var2))
    (PlainE3 $ PlainE4 $ BoolB var3)

-- (a+b)*c
e9 = MulE
    (PlainE3 $ PlainE4 $ BaseE $ Paren $ PlainE1 $ OrE
        (PlainE2 $ PlainE3 $ PlainE4 $ BoolB var1)
        (PlainE3 $ PlainE4 $ BoolB var2))
    (PlainE4 $ BoolB var3)

-- ab+c!=(a+b)*c
e10 = NEqE (PlainE1 e8) (PlainE1 $ PlainE2 e9)

p1 :: Program
p1 = Program e10