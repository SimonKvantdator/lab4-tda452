module ToCanonical where

import Expression
import Data.Maybe
import Data.List

sortExpr :: Expr -> Expr
sortExpr e
    | isNumeric e   = e
    | isVariable e  = e
    | isAdd e       = sortExpr <$$> Add (sortBy comp $ fromAdd e)
    | isMul e       = sortExpr <$$> Mul (sortBy comp $ fromMul e)
    | isPow e       = sortExpr <$$> e

-- TODO: make expression instance of applicative?
flattenAdd :: Expr -> Expr
flattenAdd (Add []) = N 0
flattenAdd (Add [t]) = flattenAdd t
flattenAdd e
    | isNumeric e   = e
    | isVariable e  = e
    | isPow e       = flattenAdd <$$> e
    | isMul e       = flattenAdd <$$> e
    | isAdd e       = flattenAddSingle $ flattenAdd <$$> e
    where
    flattenAddSingle (Add ts)   =
        Add $ concat $ [t | t <- ts, not $ isAdd t]:[fromAdd t | t <- ts, isAdd t]
    flattenAddSingle e          = e

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
add = flattenAdd . Add
mul = flattenMul . Mul

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
    | isMul e       = mul $ filter (not . isNumeric) (fromMul e)
    | isNumeric e   = N 1
    | otherwise     = e

getNumericFactors :: Expr -> Integer
getNumericFactors e
    | isMul e       = product $ fromNumeric <$> filter isNumeric (fromMul e)
    | isNumeric e   = fromNumeric e
    | otherwise     = 1

combineTerms :: Expr -> Expr
combineTerms e
    | isVariable e  = e
    | isNumeric e   = e
    | isMul e       = combineTerms <$$> e
    | isPow e       = combineTerms <$$> e
    | isAdd e       = combineTerms <$$> combineTermsHelper2 (fromAdd e)
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

combineNumsInPow :: Expr -> Expr
combineNumsInPow (Pow (N n) (N m))
    | m >= 0    = N $ n^m
combineNumsInPow e
    | isPow e   = combineNumsInPow <$$> e
    | isAdd e   = combineNumsInPow <$$> e
    | isMul e   = combineNumsInPow <$$> e
    | otherwise = e


{-extractSingleElem :: Expr -> Expr
extractSingleElem (Mul [e]) = extractSingleElem e
extractSingleElem (Add [e]) = extractSingleElem e
extractSingleElem e = e -}
            

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
    | isMul e                               = removeMulBy1 <$$> e'
    | otherwise                             = removeMulBy1 <$$> e
    where
    e' = case fs' of
        [] -> N 1
        _  -> Mul fs'
    fs' = [f | f <- fromMul e, f /= N 1]

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

-- x = V $ Var "x"
-- y = V $ Var "y"
-- z = V $ Var "z"

-- h' = Add [Mul [Pow (Mul [N 2,x]) x,Mul [Pow (Mul [N 2,x]) x,N 2]],Mul [x,x],Mul [x,y],Mul [x,z],Mul [y,z],Mul [N 2,x],Mul [N 2,y]]

-- ruleList = [(Var "x", Mul [N 2, V (Var "z")]),(Var "y", N 3),(Var "z",N 4)]--(Var "z",Mul [N 4, V (Var "y")])] 

-- h = Add [N 2, Mul [N 3,Pow (N 3) (N 4)]]
