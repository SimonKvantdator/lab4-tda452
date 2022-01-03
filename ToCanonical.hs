module ToCanonical where

import Expression
import Data.Maybe
import Data.List

-- Sorting an expression according to ordering defined in comp
sortExpr :: Expr -> Expr
sortExpr e@(AC op es)       = AC op (sortBy comp $ fromAC (sortExpr <$$> e))
sortExpr e@(Pow _ _)        = sortExpr <$$> e
sortExpr e                  = e


-- Removes nestled AC expressions (AC Add [1, AC Add [2,3]] = AC Add [1,2,3])
flattenAC :: Op -> Expr -> Expr
flattenAC Add (AC Add [])       = N 0
flattenAC Mul (AC Mul [])       = N 1
flattenAC op (AC _ [t])         = flattenAC op t
flattenAC op e@(Pow _ _)        = flattenAC op <$$> e
flattenAC op1 e@(AC op2 es) 
    | op1 == op2                = flattenACSingle $ flattenAC op1 <$$> e
    | otherwise                 = flattenAC op1  <$$> e
    where
        flattenACSingle :: Expr -> Expr
        flattenACSingle (AC Add es)   =
            AC Add $ concat $ [t | t <- es, not $ isAdd t]:[fromAC t | t <- es, isAdd t]
        flattenACSingle(AC Mul es)    =
            AC Mul $ concat $ [f | f <- es, not $ isMul f]:[fromAC f | f <- es, isMul f]
        flattenACSingle e             = e
flattenAC _  e                  = e

-- TODO: Helper function which ?? 

-- Smart Constructors
add :: [Expr] -> Expr
add = flattenAC Add . AC Add

mul :: [Expr] -> Expr
mul = flattenAC Mul . AC Mul


-- Expands an expression from factored form to canonical form
expand :: Expr -> Expr
expand = flattenAC Add . expandHelper1
    where
        expandHelper1 e@(AC Add es)      = expandHelper1 <$$> e
        expandHelper1 e@(Pow _ _)        = expandHelper1 <$$> e
        expandHelper1 (AC Mul es)        = expandHelper1 <$$> expandHelper2 [] es
        expandHelper1 e                  = e
        
        -- expandHelper2 takes an initial list and a list of factors and returns an expansion
        expandHelper2 :: [Expr] -> [Expr] -> Expr
        expandHelper2 gs (AC Add ts:fs)  = AC Add [expandHelper2 (t:gs) fs | t <- ts]
        expandHelper2 gs (f:fs)          = expandHelper2 (f:gs) fs
        expandHelper2 gs []              = mul gs

-- Combines numeric values in AC operations
combineNumsInAC :: Op -> Expr -> Expr
combineNumsInAC op e@(Pow _ _)   = combineNumsInAC op <$$> e
combineNumsInAC Mul e@(AC Add es)  = combineNumsInAC Mul <$$> e
combineNumsInAC Mul e@(AC Mul es)  = combineNumsInAC Mul <$$> AC Mul (
        N (product [fromNumeric n | n <- es, isNumeric n])
        :[f | f <- es, not $ isNumeric f])
combineNumsInAC Add e@(AC Add es)  = combineNumsInAC Add <$$> AC Add (
        N (sum [fromNumeric n | n <- es, isNumeric n])
        :[t | t <- es, not $ isNumeric t])
combineNumsInAC Add e@(AC Mul es)  = combineNumsInAC Add <$$> e
combineNumsInAC _ e             = e


-- Strips an expression of its constant factors
removeNumericFactors :: Expr -> Expr
removeNumericFactors (AC Mul es) = mul $ filter (not . isNumeric) es
removeNumericFactors (N n)       = N 1
removeNumericFactors e           = e


-- Extracts the constant factor of an expression
getNumericFactors :: Expr -> Integer
getNumericFactors (AC Mul es) = product $ fromNumeric <$> filter isNumeric es
getNumericFactors (N n)       = n 
getNumericFactors _           = 1


-- Simplifies the addition of equal expressions with potentially differing constant factors
combineTerms :: Expr -> Expr
combineTerms e@(AC Mul _) = combineTerms <$$> e
combineTerms e@(Pow _ _)  = combineTerms <$$> e
combineTerms (AC Add es)  = combineTerms <$$> combineTermsHelper2 es
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
combineTerms e            = e

-- Simplifies an expression containing an integer to the power of another integer
combineNumsInPow :: Expr -> Expr
combineNumsInPow (Pow (N 0) (N 0))  = Pow (N 0) (N 0)
combineNumsInPow (Pow (N n) (N m))
    | m >= 0                        = N $ n^m
combineNumsInPow (Pow (N 0) _)      = N 0
combineNumsInPow (Pow _ (N 0))      = N 1
combineNumsInPow e@(Pow _ _)        = combineNumsInPow <$$> e
combineNumsInPow e@(AC _ _)         = combineNumsInPow <$$> e
combineNumsInPow e                  = e


-- Combines the multiplication of equal variables into one factor
combinePwrsInMul :: Expr -> Expr
combinePwrsInMul e@(Pow _ _)        = combinePwrsInMul <$$> e
combinePwrsInMul e@(AC Add _)       = combinePwrsInMul <$$> e
combinePwrsInMul e@(AC Mul fs)      = combinePwrsInMul <$$> AC Mul (
            [v | v <- fs, numFact v == AC Add [N 1]]
            ++ nub ([Pow v (numFact v)| v <- nub fs, numFact v /= AC Add [N 1], not $ isPow v]
            ++ [Pow (fst $ fromPow v) (numFact (fst $ fromPow v))| v <- nub fs, numFact v /= AC Add [N 1], isPow v]))
    where
        numFact v = flattenAC Add . removeAdd0 . AC Add $ N (fromIntegral $ length [t | t <- fs, t == v, not $ isPow t]):[snd (fromPow t) | t <- fs, isPow t, fst (fromPow t) == v]
combinePwrsInMul e                  = e

-- Simplifies any expression multiplied by 0 to 0
removeMulBy0 :: Expr -> Expr
removeMulBy0 e@(V var)  = e
removeMulBy0 e@(N n)    = e
removeMulBy0 e@(AC Mul es) 
    | N 0 `elem` es     = N 0
removeMulBy0 e          = removeMulBy0 <$$> e


-- Simplifies any expression multiplied by 1 to the expression itself
removeMulBy1 :: Expr -> Expr
removeMulBy1 e@(V var)      = e
removeMulBy1 e@(N n)        = e
removeMulBy1 (AC Mul es)    = removeMulBy1 <$$> e'
    where
        e' = case fs' of
            [] -> N 1
            _  -> AC Mul fs'
        fs' = [f | f <- es, f /= N 1]
removeMulBy1 e              = removeMulBy1 <$$> e

-- Simplifies addition of 0 to any expression to the expression itself
removeAdd0 :: Expr -> Expr
removeAdd0 e@(V var)    = e
removeAdd0 e@(N n)      = e
removeAdd0 (AC Add es)  = removeAdd0 <$$> e'
    where
        e' = case ts' of
            [] -> N 0
            _  -> AC Add ts'
        ts' = [t | t <- es, t /= N 0]

removeAdd0 e            = removeAdd0 <$$> e

-- Combines all functions above to obtain an expression in its simplest canonical form
toCanonical :: Expr -> Expr
toCanonical =
    sortExpr .
    flattenAC Mul . flattenAC Add .
    removeAdd0 . removeMulBy1 . removeMulBy0 .
    combineTerms .
    combineNumsInAC Add . combineNumsInAC Mul . combinePwrsInMul . combineNumsInPow . 
    expand .
    combineNumsInAC Add . combineNumsInAC Mul .
    flattenAC Mul . flattenAC Add
