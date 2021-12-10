-- Emma Sofia Ringström & Simon Stefanus Jacobsson
-- Grunduppgift: Skriv en simplifyer som kan förenkla ett uttryck givet en
-- mängd samband för variablerna som uttrycket består av.
-- Bonusuppgift: Skriv en parser så att man kan mata in uttryck och samband i
-- terminalen. Printa sen LaTeXkod för det förenklade uttrycket.

module Simplify
( toCanonical
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

-- -- TODO: can I use emap?
-- sortExpr :: Expr -> Expr
-- sortExpr (Add ts)   = Add $ sortExpr <$> sortBy sortFun ts
-- sortExpr (Mul fs)   = Mul $ sortExpr <$> sortBy sortFun fs
-- sortExpr e          = e

-- -- TODO: create a strict ordering
-- sortFun :: Expr -> Expr -> Ordering
-- sortFun e1 e2 = show e1 `compare` show e2

sortExpr :: Expr -> Expr
sortExpr e = case expand e of
    Add es      -> Add $ sortTerms $ map sortExpr es
    Mul es      -> Mul $ sortTerms $ map sortExpr es
    Pow e1 e2   -> Pow (sortExpr e1) (sortExpr e2)
    e           -> e

sortTerms :: [Expr] -> [Expr]
sortTerms = sortBy comp


comp :: Expr -> Expr -> Ordering
comp (N n) (N s)                                                         = compare n s
comp (N n) _                                                             = LT
comp _ (N n)                                                             = GT

comp (Pow e1 e2) (Pow e3 e4) | comp e1 e3 == LT                          = LT
                                | comp e1 e3 == GT                          = GT
                                | otherwise                                 = comp e2 e4
comp (Pow _ _) _                                                         = GT
comp _ (Pow _ _)                                                         = LT

comp (V (Var v)) (V (Var s))                                             = compare v s

comp (Mul es1) (Mul es2)     | comp (head es1) (head es2) == LT          = LT
                                | comp (head es1) (head es2) == GT          = GT
                                | not (null (tail es1)) && not (null (tail es2))
                                = comp (Mul $ tail es1) (Mul $ tail es2)
                                | not (null (tail es1))                     = GT
                                | not (null (tail es2))                     = LT
comp (Mul es) _                                                          = GT
comp _ (Mul es)                                                          = LT

comp (Add es1) (Add es2)     | comp (head es1) (head es2) == LT          = LT
                                | comp (head es1) (head es2) == GT          = GT
                                | not (null (tail es1)) && not (null (tail es2))
                                = comp (Add $ tail es1) (Add $ tail es2)
                                | not (null (tail es1))                     = GT
                                | not (null (tail es2))                     = LT


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
toCanonical :: Expr -> Expr
toCanonical = sortExpr . toCanonical' . sortExpr

toCanonical' :: Expr -> Expr
toCanonical' (Mul [e])    = e
toCanonical' (Mul (e:es)) = case (e,head es) of
    (N 0, e)              -> N 0
    (e, N 0)              -> N 0
    (N 1, e)              -> Mul $ e:[toCanonical' (Mul $ tail es)]
    (e, N 1)              -> Mul $ e:[toCanonical' (Mul $ tail es)]
    (N n, N s)            -> toCanonical' (Mul $ N (n + s):tail es)
    (V (Var x), V (Var y)) | x == y
                          -> toCanonical' (Mul $ Pow (V (Var x)) (N 2):tail es)
    (Pow (V (Var x)) n, V (Var y)) | x == y
                          -> toCanonical' (Mul $ Pow (V (Var x)) (toCanonical' (n .+ N 1)):tail es)
    (V (Var x), Pow (V (Var y)) n) | x == y
                          -> toCanonical' (Mul $ Pow (V (Var y)) (toCanonical' (n .+ N 1)):tail es)
    (Pow (V (Var x)) s, Pow (V(Var y)) n) | x == y
                          -> toCanonical' (Mul $ Pow (V (Var x)) (toCanonical' (s .+ n)):tail es)
    (e,_)                 -> Mul $ e:[toCanonical' (Mul es)]

toCanonical' (Add [e])    = e
toCanonical' (Add (e:es)) = case (e,head es) of
    (N 0, e)                            -> toCanonical' (Add $ e:tail es)
    (e, N 0)                            -> toCanonical' (Add $ e:tail es)
    (N n, N s)                          -> toCanonical' (Add $ N (n + s):tail es)
    (V (Var x), V (Var y)) | x == y
                                        -> toCanonical' (Add $ Mul [N 2, V (Var x)]:tail es)
    (e1, e2) | comp e1 e2 == EQ         -> toCanonical' (Add $ Mul [N 2, e1]:tail es)
    (Mul [N n, e1], Mul [N s, e2]) | comp e1 e2 == EQ     
                                        -> toCanonical' (Add $ Mul [N (n+s), e1]:tail es)
    (Mul [N n, e1], e) | comp e1 e == EQ                 
                                        -> toCanonical' (Add $ Mul [N (n+1), e1]:tail es)
    (e, Mul [N s, e2]) | comp e e2 == EQ                   
                                        -> toCanonical' (Add $ Mul [N (s+1), e2]:tail es)
    (e,_)                               -> Add $ toCanonical' e:[toCanonical' (Add $ map toCanonical' es)]

toCanonical' (Pow (N 0) e) = N 0
toCanonical' (Pow (N 1) e) = N 1
toCanonical' (Pow e (N 0)) = N 1
toCanonical' (Pow e (N 1)) = e

toCanonical' e = e

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
g = N 1 .* x .* x.* y.* N 2
