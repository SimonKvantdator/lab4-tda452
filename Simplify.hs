-- Emma Sofia Ringström & Simon Stefanus Jacobsson
-- Grunduppgift: Skriv en simplifyer som kan förenkla ett uttryck givet en
-- mängd samband för variablerna som uttrycket består av.
-- Bonusuppgift: Skriv en parser så att man kan mata in uttryck och samband i
-- terminalen. Printa sen LaTeXkod för det förenklade uttrycket.

-- module Simplify
-- ( Variable(..)
-- , Expr(..)
-- , Rule
-- , add
-- , mul
-- , (.+)
-- , (.*)
-- , (.^)
-- , toCanonical
-- , expand
-- , sortExpr
-- , combineTerms
-- , applyRules
-- , show' -- bodge
-- ) where

-- bodge
module Simplify where

import Data.Maybe
import Data.List
import Data.Function (on)


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

type Rule = (Variable, Expr)

-- Smart constructors
add = flattenAdd . Add
mul = flattenMul . Mul

isAdd :: Expr -> Bool
isAdd (Add _)   = True
isAdd _         = False

fromAdd :: Expr -> [Expr]
fromAdd (Add ts)  = ts
fromAdd e         = [e]

isMul :: Expr -> Bool
isMul (Mul _)   = True
isMul _         = False

fromMul :: Expr -> [Expr]
fromMul (Mul fs)  = fs
fromMul e         = [e] -- TODO: reconsider

isVariable :: Expr -> Bool
isVariable (V _) = True
isVariable  _ = False

isNumeric :: Expr -> Bool
isNumeric (N _) = True
isNumeric _ = False

fromNumeric :: Expr -> Integer
fromNumeric (N n) = n

isPow :: Expr -> Bool
isPow (Pow _ _)   = True
isPow _         = False

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
x .+ y = add [x, y]
infixl 6 .+

(.*) :: Expr -> Expr -> Expr
x .* y = mul [x, y]
infixl 6 .*

(.-) :: Expr -> Expr -> Expr
x .- y = add [x, N (-1) .* y]
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
    show (Mul (N n:fs))
        | n < 0             = "(" ++ show n ++ ")*" ++ show (Mul fs)
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
show' (V v)         = "V " ++ show v
show' (N n)         = "N " ++ show n
show' (Add ts)      = "Add [" ++ foldl (\x y -> x ++ "," ++ y) (show' $ head ts) (show' <$> tail ts) ++ "]"
show' (Mul fs)      = "Mul [" ++ foldl (\x y -> x ++ "," ++ y) (show' $ head fs) (show' <$> tail fs) ++ "]"
show' (Pow x y)     = "Pow (" ++ show' x ++ ") (" ++ show' y ++ ")"

-- TODO: do we really want to do this?
instance Eq Expr
    where
    e == f = show e == show f

instance Ord Expr
    where
    compare = comp

--type Rule = (Variable, Expr)

-- generateMoreRules [x = y / z] = [x = y / z, y = x / z, z = y / x]

sortExpr :: Expr -> Expr
sortExpr e
    | isNumeric e = e
    | isVariable e = e
    | isAdd e   = sortExpr <$$> Add (sortBy comp $ fromAdd e)
    | isMul e   = sortExpr <$$> Mul (sortBy comp $ fromMul e)
    | isPow e   = sortExpr <$$> e

sortTerms :: [Expr] -> [Expr]
sortTerms = sortBy comp

comp :: Expr -> Expr -> Ordering
comp (N n) (N s)                                                         = compare n s
comp (N n) _                                                             = LT
comp _ (N n)                                                             = GT
comp e1 e2 = compare (show e1) (show e2)


-- TODO: make expression instance of applicative?
flattenAdd :: Expr -> Expr
flattenAdd e
    | isNumeric e   = e
    | isVariable e  = e
    | otherwise     = flattenAddSingle $ flattenAdd <$$> e
    where
    flattenAddSingle (Add ts)   =
        Add $ concat $ [t | t <- ts, not $ isAdd t]:[fromAdd t | t <- ts, isAdd t]
    flattenAddSingle e          = e

flattenMul :: Expr -> Expr
flattenMul e
    | isNumeric e   = e
    | isVariable e  = e
    | otherwise     = flattenMulSingle $ flattenMul <$$> e
    where
    flattenMulSingle (Mul fs)   =
        Mul $ concat $ [f | f <- fs, not $ isMul f]:[fromMul f | f <- fs, isMul f]
    flattenMulSingle e          = e

expand :: Expr -> Expr
expand = flattenAdd . expandHelper1
    where
    expandHelper1 e
        | isNumeric e   = e
        | isVariable e  = e
        | isAdd e       = expandHelper1 <$$> e
        | isPow e       = expandHelper1 <$$> e
        | isMul e       = expandHelper1 <$$> expandHelper2 [] (fromMul e)
    -- expandHelper2 takes an initial list and a list of factors and returns an expansion
    expandHelper2 :: [Expr] -> [Expr] -> Expr
    expandHelper2 gs (Add ts:fs) = Add [expandHelper2 (t:gs) fs | t <- ts]
    expandHelper2 gs (f:fs)      = expandHelper2 (f:gs) fs
    expandHelper2 gs []          = mul gs

combineNumsInAdd :: Expr -> Expr
combineNumsInAdd e
    | isNumeric e   = e
    | isVariable e  = e
    | isPow e       = combineNumsInAdd <$$> e
    | isMul e       = combineNumsInAdd <$$> e
    | isAdd e       = combineNumsInAdd <$$> (Add $
        N (sum [fromNumeric n | n <- ts, isNumeric n])
        :[t | t <- ts, not $ isNumeric t])
    where
    ts = fromAdd e

combineNumsInMul :: Expr -> Expr
combineNumsInMul e
    | isNumeric e   = e
    | isVariable e  = e
    | isPow e       = combineNumsInMul <$$> e
    | isAdd e       = combineNumsInMul <$$> e
    | isMul e       = combineNumsInMul <$$> (Mul $
        N (product [fromNumeric n | n <- fs, isNumeric n])
        :[f | f <- fs, not $ isNumeric f])
    where
    fs = fromMul e

removeNumericFactors :: Expr -> Expr
removeNumericFactors e
    | isMul e'  = mul $ filter (not . isNumeric) (fromMul e')
    | otherwise = e'
    where e' = flattenMul e

getNumericFactors :: Expr -> Integer
getNumericFactors e
    | isMul e'  = product $ fromNumeric <$> filter isNumeric (fromMul e')
    | otherwise = 1
    where e'    = flattenMul e

combineTerms :: Expr -> Expr
combineTerms e
    | isVariable e  = e
    | isNumeric e   = e
    | isPow e       = combineTerms <$$> e
    | isAdd e       = combineTerms <$$> combineTermsHelper2 (fromAdd e)
    | otherwise     = combineTerms <$$> e
    where
    combineTermsHelper2 :: [Expr] -> Expr
    combineTermsHelper2 (t1:t2:ts)
        | removeNumericFactors t1 == removeNumericFactors t2 =
            combineTermsHelper2 $
            mul (
                (N $ getNumericFactors t1 + getNumericFactors t2):
                [removeNumericFactors t1]
            ):ts
        | otherwise = t1 .+ combineTermsHelper2 (t2:ts)
    combineTermsHelper2 [t] = t
    combineTermsHelper2 _ = undefined -- Mul [] encountered
    --combineTermsHelper [] = 


removeMulBy0 :: Expr -> Expr
removeMulBy0 e
    | isVariable e                      = e
    | isNumeric e                       = e
    | isMul e && N 0 `elem` fromMul e   = N 0
    | otherwise                         = removeMulBy0 <$$> e

removeMulBy1 :: Expr -> Expr
removeMulBy1 e
    | isVariable e                          = e
    | isNumeric e                           = e
    | isMul e                               = removeMulBy1 <$$> Mul fs
    | otherwise                             = removeMulBy1 <$$> e
    where
    fs = case fs' of
        [] -> [N 1]
        _  -> fs'
    fs' = [f | f <- fromMul e, f /= N 1]

removeAdd0 :: Expr -> Expr
removeAdd0 e
    | isVariable e                          = e
    | isNumeric e                           = e
    | isAdd e                               = removeAdd0 <$$> Add ts
    | otherwise                             = removeAdd0 <$$> e
    where
    ts = case ts' of
        [] -> [N 0]
        _  -> ts'
    ts' = [t | t <- fromAdd e, t /= N 0]


-- toCanonical :: Expr -> Expr
toCanonical :: Expr -> Expr
toCanonical = removeAdd0 . removeMulBy1 . removeMulBy0 . sortExpr .
    combineNumsInAdd . combineNumsInMul .
    combineTerms . expand .
    combineNumsInAdd . combineNumsInMul .
    flattenMul . flattenAdd
-- sortExpr uses flattenAdd and flattenMul I think


applyRule :: Rule -> Expr -> Expr
applyRule (x, e) (V y) | x == y  = e
applyRule r (Add ts)             = Add (applyRule r <$> ts)
applyRule r (Mul fs)             = Mul (applyRule r <$> fs)
applyRule r (Pow e1 e2)          = Pow (applyRule r e1) (applyRule r e2)
applyRule r e                    = e

-- applies a list of rules in order to an expression
applyRules :: [Rule] -> Expr -> Expr
applyRules [] e = e
applyRules rs e = applyRules (tail rs) (applyRule (head rs) e)

findSimplest :: Expr -> [Rule] -> Expr
findSimplest e rs = minimumBy (compare `on` lengthOfExpr) $ map toCanonical (findSimplest' e $ concatMap subsequences (permutations rs))
    where
        findSimplest' :: Expr -> [[Rule]] -> [Expr]
        findSimplest' e [] = []
        findSimplest' e rss = e:applyRules (head rss) e:findSimplest' e (tail rss)

--e, e_r1, e_r2, ..., e_rn, e_r1r2, e_r1r3, ..., e_rnr(n-1)...r1

 --findSimplest expr rules = head $ sortOn lengthOfExpr (findSimplestHelper ...)

---- findSimplestHelper :: Int -> Int -> [Expr] -> [Rule] -> [Expr]
---- findSimplestHelper depth maxLength currentExpr rules = ...

lengthOfExpr :: Expr -> Integer
lengthOfExpr (Add ts)       = sum $ lengthOfExpr <$> ts
lengthOfExpr (Mul fs)       = product $ lengthOfExpr <$> fs
lengthOfExpr (Pow e1 e2)    = 1 --lengthOfExpr e1
lengthOfExpr _              = 1

---- TODO: QuickCheck properties
----    How do we know if a simplification is valid?
----    How do we know that the simplest form really has been found?
----
----    Generate sets of integers values for x, y, etc, and check if they satisfy the assumptions.
----    Then test if the expressions are equal if evaluated for these x, y, etc.
----
---- Bodge: Examples

toLatex :: Expr -> String
toLatex (Add [e]) = toLatex e
toLatex (Add (e:es)) = toLatex e ++ " + " ++ toLatex (Add es)
toLatex (Mul [e]) = toLatex e
toLatex (Mul (e:es)) = toLatex e ++  toLatex (Mul es)
toLatex (Pow e1@(V (Var x)) e2) = toLatex e1 ++ " ^{ " ++ toLatex e2 ++ " }"
toLatex (Pow e1@(N n) e2) = toLatex e1 ++ " ^{ " ++ toLatex e2 ++ " }"
toLatex (Pow e1 e2) = "(" ++ toLatex e1 ++ ")" ++ " ^{ " ++ toLatex e2 ++ " }"
toLatex (V (Var x)) = x
toLatex (N n) = show n
toLatex _ = ""


printLatex :: Expr -> IO()
printLatex e = putStrLn $ "$" ++ toLatex e ++ "$"

x = V $ Var "x"
y = V $ Var "y"
z = V $ Var "z"
-- a = x
-- b = N 1 .+ (N (-2) .* x) .+ z
-- c = (x .+ y .+ z).^(x .+ y)
-- d = (x .+ y .+ z).*(x .+ y)
-- e = y .+ (z .+ x)
-- f = (x .+ y) .* (y .+ z) .* (x .+ z)
-- g = N 1 .* x .* x.* y.* N 2
-- h = ((x .* N 2) .^ x) .+ ((x .+ N 2 .+ z).*(x .+ y)) .+ (((x .* N 2) .^ x).* N 2)
-- i = (N 3 .+ y) .* (N 8 .+ z) .* (N 4 .+ z)

h' = Add [Pow (Mul [N 2,x]) x,Mul [Pow (Mul [N 2,x]) x,N 2],Mul [x,x],Mul [x,y],Mul [x,z],Mul [y,z],Mul [N 2,x],Mul [N 2,y]]

ruleList = [(Var "x", Mul [N 2, V (Var "z")]),(Var "y", N 3),(Var "z",Mul [N 4, V (Var "y")])]
