module ToCanonical where

import Expression
import Data.Maybe
import Data.List

-- Sorting an expression according to ordering defined in comp
sortExpr :: Expr -> Expr
sortExpr e
    | isAdd e       = sortExpr <$$> Add (sortBy comp $ fromAdd e)
    | isMul e       = sortExpr <$$> Mul (sortBy comp $ fromMul e)
    | isPow e       = sortExpr <$$> e
    | otherwise     = e


-- Removes nestled Add expressions (Add [1, Add [2,3]] = Add [1,2,3])
flattenAdd :: Expr -> Expr
flattenAdd (Add []) = N 0
flattenAdd (Add [t]) = flattenAdd t
flattenAdd e
    | isPow e       = flattenAdd <$$> e
    | isMul e       = flattenAdd <$$> e
    | isAdd e       = flattenAddSingle $ flattenAdd <$$> e
    | otherwise     = e
    where
    flattenAddSingle (Add ts)   =
        Add $ concat $ [t | t <- ts, not $ isAdd t]:[fromAdd t | t <- ts, isAdd t]
    flattenAddSingle e          = e


-- Removes nestled Mul expressions (Mul [1, Mul [2,3]] = Mul [1,2,3])
flattenMul :: Expr -> Expr
flattenMul (Mul []) = N 1
flattenMul (Mul [f]) = flattenMul f
flattenMul e
    | isNumeric e   = e
    | isVariable e  = e
    | isPow e       = flattenMul <$$> e
    | isAdd e       = flattenMul <$$> e
    | otherwise     = flattenMulSingle $ flattenMul <$$> e
    where
        flattenMulSingle (Mul fs)   =
            Mul $ concat $ [f | f <- fs, not $ isMul f]:[fromMul f | f <- fs, isMul f]
        flattenMulSingle e          = e


-- Smart Constructors
add :: [Expr] -> Expr
add = flattenAdd . Add

mul :: [Expr] -> Expr
mul = flattenMul . Mul


-- Expands an expression from factored form to canonical form
expand :: Expr -> Expr
expand = flattenAdd . expandHelper1
    where
        expandHelper1 e
            | isAdd e       = expandHelper1 <$$> e
            | isPow e       = expandHelper1 <$$> e
            | isMul e       = expandHelper1 <$$> expandHelper2 [] (fromMul e)
            | otherwise     = e
        
        -- expandHelper2 takes an initial list and a list of factors and returns an expansion
        expandHelper2 :: [Expr] -> [Expr] -> Expr
        expandHelper2 gs (Add ts:fs) = Add [expandHelper2 (t:gs) fs | t <- ts]
        expandHelper2 gs (f:fs)      = expandHelper2 (f:gs) fs
        expandHelper2 gs []          = mul gs


-- Simplifies addition of integers
combineNumsInAdd :: Expr -> Expr
combineNumsInAdd e
    | isPow e       = combineNumsInAdd <$$> e
    | isMul e       = combineNumsInAdd <$$> e
    | isAdd e       = combineNumsInAdd <$$> Add (
        N (sum [fromNumeric n | n <- ts, isNumeric n])
        :[t | t <- ts, not $ isNumeric t])
    | otherwise     = e
    where
        ts = fromAdd e


-- Simplifies multiplication of integers
combineNumsInMul :: Expr -> Expr
combineNumsInMul e
    | isPow e       = combineNumsInMul <$$> e
    | isAdd e       = combineNumsInMul <$$> e
    | isMul e       = combineNumsInMul <$$> Mul (
        N (product [fromNumeric n | n <- fs, isNumeric n])
        :[f | f <- fs, not $ isNumeric f])
    | otherwise     = e
    where
        fs = fromMul e


-- Strips an expression of its constant factors
removeNumericFactors :: Expr -> Expr
removeNumericFactors e
    | isMul e       = mul $ filter (not . isNumeric) (fromMul e)
    | isNumeric e   = N 1
    | otherwise     = e


-- Extracts the constant factor of an expression
getNumericFactors :: Expr -> Integer
getNumericFactors e
    | isMul e       = product $ fromNumeric <$> filter isNumeric (fromMul e)
    | isNumeric e   = fromNumeric e
    | otherwise     = 1


-- Simplifies the addition of equal expressions with potentially differing constant factors
combineTerms :: Expr -> Expr
combineTerms e
    | isMul e       = combineTerms <$$> e
    | isPow e       = combineTerms <$$> e
    | isAdd e       = combineTerms <$$> combineTermsHelper2 (fromAdd e)
    | otherwise     = e
    where
        combineTermsHelper2 :: [Expr] -> Expr
        combineTermsHelper2 (t1:t2:ts)
            | removeNumericFactors t1 == removeNumericFactors t2 =
                combineTermsHelper2 $
                mul (
                    N (getNumericFactors t1 + getNumericFactors t2):
                    [removeNumericFactors t1]
                ):ts
            | otherwise = t1 .+ combineTermsHelper2 (t2:ts)
        combineTermsHelper2 [t] = t


-- Simplifies an expression containing an integer to the power of another integer
combineNumsInPow :: Expr -> Expr
combineNumsInPow (Pow (N 0) (N 0)) = Pow (N 0) (N 0)
combineNumsInPow (Pow (N n) (N m))
    | m >= 0    = N $ n^m
combineNumsInPow (Pow (N 0) _) = N 0
combineNumsInPow (Pow _ (N 0)) = N 1
combineNumsInPow e
    | isPow e   = combineNumsInPow <$$> e
    | isAdd e   = combineNumsInPow <$$> e
    | isMul e   = combineNumsInPow <$$> e
    | otherwise = e


-- Combines the multiplication of equal variables into one factor
combinePwrsInMul :: Expr -> Expr
combinePwrsInMul e
    | isPow e       = combinePwrsInMul <$$> e
    | isAdd e       = combinePwrsInMul <$$> e
    | isMul e       = combinePwrsInMul <$$> Mul (
            [v | v <- fs, numFact v == Add [N 1]]
            ++ nub ([Pow v (numFact v)| v <- nub fs, numFact v /= Add [N 1], not $ isPow v]
            ++ [Pow (fst $ fromPow v) (numFact (fst $ fromPow v))| v <- nub fs, numFact v /= Add [N 1], isPow v]))
    | otherwise     = e
    where
        fs = fromMul e
        numFact v = flattenAdd . removeAdd0 . Add $ N (fromIntegral $ length [t | t <- fs, t == v, not $ isPow t]):[snd (fromPow t) | t <- fs, isPow t, fst (fromPow t) == v]


-- Simplifies any expression multiplied by 0 to 0
removeMulBy0 :: Expr -> Expr
removeMulBy0 e
    | isVariable e                      = e
    | isNumeric e                       = e
    | isMul e && N 0 `elem` fromMul e   = N 0
    | otherwise                         = removeMulBy0 <$$> e


-- Simplifies any expression multiplied by 1 to the expression itself
removeMulBy1 :: Expr -> Expr
removeMulBy1 e
    | isVariable e                          = e
    | isNumeric e                           = e
    | isMul e                               = removeMulBy1 <$$> e'
    | otherwise                             = removeMulBy1 <$$> e
    where
        e' = case fs' of
            [] -> N 1
            _  -> Mul fs'
        fs' = [f | f <- fromMul e, f /= N 1]


-- Simplifies addition of 0 to any expression to the expression itself
removeAdd0 :: Expr -> Expr
removeAdd0 e
    | isVariable e                          = e
    | isNumeric e                           = e
    | isAdd e                               = removeAdd0 <$$> e'
    | otherwise                             = removeAdd0 <$$> e
    where
        e' = case ts' of
            [] -> N 0
            _  -> Add ts'
        ts' = [t | t <- fromAdd e, t /= N 0]


-- Combines all functions above to obtain an expression in its simplest canonical form
toCanonical :: Expr -> Expr
toCanonical =
    sortExpr .
    flattenMul . flattenAdd .
    removeAdd0 . removeMulBy1 . removeMulBy0 .
    combineTerms .
    combineNumsInAdd . combineNumsInMul . combinePwrsInMul . combineNumsInPow . 
    expand .
    combineNumsInAdd . combineNumsInMul .
    flattenMul . flattenAdd
