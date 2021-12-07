
-- Emma Sofia Ringström & Simon Stefanus Jacobsson
-- Grunduppgift: Skriv en simplifyer som kan förenkla ett uttryck givet en
-- mängd samband för variablerna som uttrycket består av.
-- Bonusuppgift: Skriv en parser så att man kan mata in uttryck och samband i
-- terminalen. Printa sen LaTeXkod för det förenklade uttrycket.

import Data.Maybe


newtype Variable = Var String
    deriving Eq

instance Show Variable where
    show (Var s) = s

data Expr =
    N Integer
    | V Variable
    | Add [Expr]
    | Mul [Expr]
    | Pow Expr Expr


infixl 6 .+
(.+) :: Expr -> Expr -> Expr
x .+ y = Add [x, y]

infixl 6 .*
(.*) :: Expr -> Expr -> Expr
x .* y = Mul [x, y]

infixl 6 .-
(.-) :: Expr -> Expr -> Expr
x .- y = Add [x, N (-1) .* y]

-- (./) :: Expr -> Expr -> Expr
-- x ./ y = TODO

infixr 8 .^
(.^) :: Expr -> Expr -> Expr
(.^) = Pow

-- TODO: make minus look nice
instance Show Expr where
    show (V (Var s))        = s
    show (N n)              = show n

    show (Add [t])          = show t
    show (Add (t:ts))       = show t ++ " + " ++ show (Add ts)
    show (Add [])           = ""

    show (Mul [Add ts])     = "(" ++ show (Add ts) ++ ")"
    show (Mul [f])          = show f
    show (Mul (Add ts:fs))  = "(" ++ show (Add ts) ++ ")*" ++ show (Mul fs)
    show (Mul (f:fs))       = show f ++ "*" ++ show (Mul fs)
    show (Mul [])           = ""

    show (Pow (N n) (N m))  = show n ++ "^" ++ show m
    show (Pow (V n) (N m))  = show n ++ "^" ++ show m
    show (Pow (N n) (V m))  = show n ++ "^" ++ show m
    show (Pow (V n) (V m))  = show n ++ "^" ++ show m

    show (Pow (N n) e)      = show n ++ "^(" ++ show e ++ ")"
    show (Pow (V n) e)      = show n ++ "^(" ++ show e ++ ")"
    show (Pow e (N n))      = "(" ++ show e ++ ")^" ++ show n
    show (Pow e (V n))      = "(" ++ show e ++ ")^" ++ show n

    show (Pow x1 x2)        = "(" ++ show x1 ++ ")^(" ++ show x2 ++ ")"


type Rule = (Variable, Expr)

-- generateMoreRules [x = y / z] = [x = y / z, y = x / z, z = y / x]

rule1 :: Rule
rule1 = (Var "x", Mul [N 2, y])


applyRule :: Rule -> Expr -> Expr
applyRule (x, e) (V y) | x == y  = e
applyRule r (Add ts)             = Add (applyRule r <$> ts)
applyRule r (Mul fs)             = Mul (applyRule r <$> fs)
applyRule r (Pow e1 e2)          = Pow (applyRule r e1) (applyRule r e2)
applyRule r e                    = e

---- applyRules :: [Rule] -> Expr -> Expr
---- applyRules rs (V x) = fromMaybe (V x) (lookup x rs)

-- flattenMuls :: Expr -> Expr
-- flattenAdds :: Expr -> Expr
-- sortMuls :: Expr -> Expr
-- sortAdds :: Expr -> Expr
-- sortFun (V Var s) = s
-- sortFun (N n) = show n

toCanonical :: Expr -> Expr
toCanonical (Mul (Add ts:fs))   = Add $ toCanonical <$> [Mul (t:fs) | t <- ts]
toCanonical (Mul (f:Add ts:fs)) = Add $ toCanonical <$> [Mul (f:t:fs) | t <- ts]
toCanonical e = e


---- simplify :: Expr -> Expr
---- simplify (Add (V x) Minus (V y)) | x == y = N 0
---- simplify (Add x _ (N 0)) = x
---- simplify (Add (N 0) Plus x) = x
---- simplify (Add (Mul (N n) Times x1) Plus x2) | x1 == x2 = Mul (N (n + 1)) Times x1
---- simplify (Add x1 Plus (Mul (N n) Times x2)) | x1 == x2 = Mul (N (n + 1)) Times x1

---- findSimplest :: Expr -> [Rule] -> Expr
---- findSimplest expr rules = head $ sortOn lengthOfExpr (findSimplestHelper ...)

---- findSimplestHelper :: Int -> Int -> [Expr] -> [Rule] -> [Expr]
---- findSimplestHelper depth maxLength currentExpr rules = ...

lengthOfExpr :: Expr -> Integer
lengthOfExpr (Add ts)       = sum $ lengthOfExpr <$> ts
lengthOfExpr (Mul fs)       = prod $ lengthOfExpr <$> fs
lengthOfExpr (Pow e1 e2)    = lengthOfExpr e1
lengthOfExpr _              = 1

---- TODO: QuickCheck properties
----    How do we know if a simplification is valid?
----    How do we know that the simplest form really has been found?
----
----    Generate sets of integers values for x, y, etc, and check if they satisfy the assumptions.
----    Then test if the expressions are equal if evaluated for these x, y, etc.
----
---- Bodge: Examples
x = V $ Var "x"
y = V $ Var "y"
z = V $ Var "z"
a = x
b = N 1 .+ (N (-2) .* x) .+ z
c = (x .+ y .+ z).^(x .+ y)
d = (x .+ y .+ z).*(x .+ y)
