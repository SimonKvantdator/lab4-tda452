-- Emma Sofia Ringström & Simon Stefanus Jacobsson
-- Grunduppgift: Skriv en simplifyer som kan förenkla ett uttryck givet en
-- mängd samband för variablerna som uttrycket består av.
-- Bonusuppgift: Skriv en parser så att man kan mata in uttryck och samband i
-- terminalen. Printa sen LaTeXkod för det förenklade uttrycket.

module Simplify
( simplify
, expand
, sortExpr
) where  

import Data.Maybe
import Data.List


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

isAdd :: Expr -> Bool
isAdd (Add _)   = True
isAdd _         = False

unpackAdd :: Expr -> [Expr]
unpackAdd (Add ts) = ts

isMul :: Expr -> Bool
isMul (Mul _)   = True
isMul _         = False

unpackMul :: Expr -> [Expr]
unpackMul (Mul fs) = fs

isVariable :: Expr -> Bool
isVariable (V _) = True
isVariable  _ = False

isNumeric :: Expr -> Bool
isNumeric (N _) = True
isNumeric _ = False

-- TODO: describe this operator
emap :: (Expr -> Expr) -> Expr -> Expr
emap f (Add ts) = Add $ map f ts
emap f (Mul fs) = Mul $ map f fs
emap f (Pow e1 e2) = Pow (f e1) (f e2)
emap f e = f e
(<$$>) = emap
infixr 4 <$$>

-- TODO: how to deal with empty Adds and Muls?
-- Does it make sense to simplify Add [] to 0 and Mul [] to 1?

(.+) :: Expr -> Expr -> Expr
x .+ y = Add [x, y]
infixl 6 .+

(.*) :: Expr -> Expr -> Expr
x .* y = Mul [x, y]
infixl 6 .*

(.-) :: Expr -> Expr -> Expr
x .- y = Add [x, N (-1) .* y]
infixl 6 .-

-- (./) :: Expr -> Expr -> Expr
-- x ./ y = TODO

(.^) :: Expr -> Expr -> Expr
(.^) = Pow
infixr 8 .^

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

-- More verbose show
show' :: Expr -> String
show' (V (Var s))   = s
show' (N n)         = show n
show' (Add ts)      = "Add [" ++ foldl (\x y -> x ++ "," ++ y) (show' $ head ts) (show' <$> tail ts) ++ "]"
show' (Mul fs)      = "Mul [" ++ foldl (\x y -> x ++ "," ++ y) (show' $ head fs) (show' <$> tail fs) ++ "]"
show' (Pow x y)     = "Pow (" ++ show' x ++ ") (" ++ show' y ++ ")"

type Rule = (Variable, Expr)

-- generateMoreRules [x = y / z] = [x = y / z, y = x / z, z = y / x]

rule1 :: Rule
rule1 = (Var "x", Mul [N 2, y])

-- applyRule :: Rule -> Expr -> Expr
-- applyRule (x, e) (V y) | x == y  = e
-- applyRule r (Add ts)             = Add (applyRule r <$> ts)
-- applyRule r (Mul fs)             = Mul (applyRule r <$> fs)
-- applyRule r (Pow e1 e2)          = Pow (applyRule r e1) (applyRule r e2)
-- applyRule r e                    = e
--
applyRule :: Rule -> Expr -> Expr
applyRule (x, e) (V y) | x == y = e
applyRule _ (N n)               = N n
applyRule r e                   = applyRule r <$$> e

---- applyRules :: [Rule] -> Expr -> Expr
---- applyRules rs (V x) = fromMaybe (V x) (lookup x rs)

sortExpr :: Expr -> Expr
sortExpr (Add ts)   = Add $ sortExpr <$> sortBy sortFun ts
sortExpr (Mul fs)   = Mul $ sortExpr <$> sortBy sortFun fs
sortExpr e          = e

-- TODO: create a strict ordering
sortFun :: Expr -> Expr -> Ordering
sortFun e1 e2 = show e1 `compare` show e2

-- -- TODO: do this without appending to end?
-- expand :: Expr -> Expr
-- expand (Mul fs)     = flattenAdd $ flattenMul $ expandHelper $ flattenAdd $ flattenMul e'
--     where
--     -- Put the factors with addition first
--     e' = Mul $ [f | f <- fs, isAdd f] ++ [f | f <- fs, not $ isAdd f]
--     expandHelper (Mul (Add ts:fs))  = Add $ expandHelper <$> [Mul (fs ++ [t]) | t <- ts]
--     expandHelper e                  = e
-- expand (Add ts)     = Add $ expand <$> ts
-- expand (Pow e1 e2)  = Pow (expand e1) (expand e2)
-- expand e            = e

-- TODO: make expression instance of applicative?
flattenAdd :: Expr -> Expr
flattenAdd e
    | isNumeric e   = e
    | isVariable e  = e
    | otherwise     = flattenAddSingle $ flattenAdd <$$> e
    where
    flattenAddSingle (Add ts)   =
        Add $ concat $ [t | t <- ts, not $ isAdd t]:[unpackAdd t | t <- ts, isAdd t]
    flattenAddSingle e          = e

flattenMul :: Expr -> Expr
flattenMul e
    | isNumeric e   = e
    | isVariable e  = e
    | otherwise     = flattenMulSingle $ flattenMul <$$> e
    where
    flattenMulSingle (Mul fs)   =
        Mul $ concat $ [f | f <- fs, not $ isMul f]:[unpackMul f | f <- fs, isMul f]
    flattenMulSingle e          = e

-- TODO: do this without appending to end?
expand :: Expr -> Expr
expand e     
    | isNumeric e   = e
    | isVariable e  = e
    | isMul e       = flattenMul $ flattenAdd $ expandHelper [] fs
    | otherwise     = expand <$$> e
    where
    fs = unpackMul $ flattenMul $ flattenAdd e
    -- expandHelper takes an initial list and a list of factors and returns an expansion
    expandHelper :: [Expr] -> [Expr] -> Expr
    expandHelper gs (Add ts:fs) = Add [expandHelper (t:gs) fs | t <- ts]
    expandHelper gs (f:fs)      = expandHelper (f:gs) fs
    expandHelper gs []          = Mul gs


-- toCanonical :: Expr -> Expr


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
lengthOfExpr (Mul fs)       = product $ lengthOfExpr <$> fs
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
e = y .+ (z .+ x)
f = (x .+ y) .* (y .+ z) .* (x .+ z)
