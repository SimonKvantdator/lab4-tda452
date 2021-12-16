-- Function eval was taken from from lecture 4A

import Expression
import ToCanonical
import Simplify
import Data.Maybe
import Test.QuickCheck

intLog :: Integer -> Integer
intLog = floor . logBase 2.0 . fromIntegral

(~=) :: (RealFloat p, Ord p) => p -> p -> Bool
a ~= b
    | (isInfinite a || isNaN a) || (isInfinite b || isNaN b) = True
    | otherwise = abs(a - b) <= epsilon * max (abs a) (abs b) + epsilon
    where
    epsilon = 0.0001
infix 4 ~=
 
instance Arbitrary Variable
    where
    arbitrary = Variable . (:[]) <$> choose ('t', 'z') -- This is the right amount of variables

instance Arbitrary Expr
    where
    arbitrary = rExpr

rExpr :: Gen Expr
rExpr = arbitraryPositiveInteger >>= rExprHelper . intLog
    where
    rExprHelper :: Integer -> Gen Expr
    rExprHelper n | n < 0 = undefined
    rExprHelper 0 = oneof [N <$> choose (-5, 5), V <$> arbitrary]
    rExprHelper n = frequency [
        (3, Add <$> vectorOf 2 (rExprHelper (n - 1))),
        (3, Mul <$> vectorOf 2 (rExprHelper (n - 1))),
        (1, Pow <$> rExprHelper (n - 1) <*> rExprHelper (n - 1))
        ]
    arbitraryPositiveInteger = getPositive <$> (arbitrary :: Gen (Positive Integer))


-- ###########################{ Testing ToCanonical }###########################

newtype EvalRules = EvalRules [(Variable, Float)]
    deriving Show

instance Arbitrary EvalRules where
    -- arbitrary = EvalRules . zip (Var . (:[]) <$> ['t'..'z']) <$> infiniteListOf (choose (-7, 7))
    arbitrary = EvalRules . zip (Variable . (:[]) <$> ['t'..'z']) <$> infiniteListOf (arbitrary :: Gen Float)

fromEvalRules (EvalRules r) = r

-- | Evaluating Symbolic Expressions
eval :: EvalRules -> Expr -> Float
eval rules (N n)        = fromInteger n
eval rules (V x)        = fromJust $ lookup x (fromEvalRules rules)
eval rules (Add ts)     = sum $ eval rules <$> ts
eval rules (Mul fs)     = product $ eval rules <$> fs
eval rules (Pow e1 e2)  = (eval rules e1)**(eval rules e2)

propFor :: (Expr -> Expr)
    -> (Expr, (EvalRules, EvalRules, EvalRules, EvalRules, EvalRules, EvalRules, EvalRules))
    -> Bool
propFor f (e, ruless) = (>= 6) . length $ filter id [eval rules e ~= eval rules (f e) | rules <- toList ruless]
    where
    toList (er1, er2, er3, er4, er5, er6, er7) = [er1, er2, er3, er4, er5, er6, er7]

flattenAddProp              = propFor flattenAdd
flattenMulProp              = propFor flattenMul
combineTermsProp            = propFor combineTerms
expandAndCombineTermsProp   = propFor $ expand . combineTerms
sortExprProp                = propFor sortExpr
expandProp                  = propFor expand
toCanonicalProp             = propFor toCanonical


-- ###########################{ Testing Simplify }###########################

instance Arbitrary Rule where
    arbitrary = return $ Rule (x, N 1)
    
-- 7-tuple of tuples of compatible EvalRules and Rules
rulesAndCompatibleEvalRules :: (([Rule], EvalRules), ([Rule], EvalRules), ([Rule], EvalRules), ([Rule], EvalRules), ([Rule], EvalRules), ([Rule], EvalRules), ([Rule], EvalRules))
rulesAndCompatibleEvalRules =
    (
        (
            Rule <$> [
                 (x, Add [V y, V z])
                ,(z, Mul [N 2, Pow (V y) (N 2)])
                ,(x, Add [V z, V w])
                ,(t, N 0)
                ,(u, N 0)
                ,(v, N 0)
                ,(w, N 0)
            ],
            EvalRules [
                 (t, 0)
                ,(u, 0)
                ,(v, 0)
                ,(w, 0)
                ,(x, 3)
                ,(y, 1)
                ,(z, 2)
            ]
        )
        ,(
            Rule <$> [
                 (x, Add[V y, Mul [N (-1), V z]])
                ,(t, Add[V u, V v])
                ,(x, V u)
                ,(t, V z)
            ]
            ,EvalRules [
                 (t, 1)
                ,(u, 2)
                ,(v, -1)
                ,(w, 0)
                ,(x, 2)
                ,(y, 3)
                ,(z, 1)
            ]
        )
        ,(
            Rule <$> [
                 (t, Mul [V u, V v])
                ,(u, Add [V y, Mul [N (-1), V z]])
                ,(y, Add [Mul [V u, V t], Pow (V x) (N 2), N 1])
            ]
            ,EvalRules [
                 (t, -2)
                ,(u, 2)
                ,(v, -1)
                ,(w, 0)
                ,(x, 2)
                ,(y, 3)
                ,(z, 1)
            ]
        )
        ,(
            Rule <$> [
                 (t, V y)
                ,(u, V t .+ V z)
                ,(v, V w .* V z .+ V u)
                ,(w, Pow (V z) (N 2))
                ,(x, V u .* V v .+ Pow (V y) (N 2))
                ,(y, N 3 .* V z)
                ,(z, V v .- V u)
            ]
            ,EvalRules [
                 (t, 3)
                ,(u, 4)
                ,(v, 5)
                ,(w, 1)
                ,(x, 29)
                ,(y, 3)
                ,(z, 1)
            ]
        )
        ,(
            Rule <$> [
                 (t, V w .* V x)
                ,(u, Pow (V w) (N 2) .* V y)
                ,(v, V u .* Pow (V y) (N (-1)) .* Pow (V w) (N 2))
                ,(w, V y .- V z)
            ]
            ,EvalRules [
                 (t, 10)
                ,(u, 12)
                ,(v, 16)
                ,(w, 2)
                ,(x, 5)
                ,(y, 3)
                ,(z, 1)
            ]
        )
        ,(
            Rule <$> [
                 (t, V y .- N 2 .* V z)
                ,(u, V y .- V w)
                ,(v, V w .* V y)
                ,(w, V t .+ V u)
            ]
            ,EvalRules [
                 (t, 1)
                ,(u, 1)
                ,(v, 6)
                ,(w, 2)
                ,(x, 0)
                ,(y, 3)
                ,(z, 1)
            ]
        )
        ,(
            Rule <$> [
                 (t, V y .- N 2 .* V t)
                ,(u, Pow (V w) (N 2))
                ,(v, V w .* V y)
                ,(w, V t .* N 2)
                ,(x, V z .* V w)
            ]
            ,EvalRules [
                 (t, 1)
                ,(u, 4)
                ,(v, 6)
                ,(w, 2)
                ,(x, 10)
                ,(y, 3)
                ,(z, 5)
            ]
        )
    )

-- rules = fst $ rulesAndCompatibleEvalRules!!0
rules = ruleList
-- evalRules = snd $ rulesAndCompatibleEvalRules!!0
evalRules = EvalRules [(x, 24), (y, 3), (z, 12), (t, 0), (u, 0), (v, 0), (w, 0)]

-- Property testing if evaluating the expression equals evaluating the simplified expression
findSimplestProp :: Expr -> Bool
findSimplestProp e = (>= 6) . length $ filter id [
    eval evalRules e ~=
    eval evalRules (findSimplest e rules)
    | (rules, evalRules) <- toList rulesAndCompatibleEvalRules]
    where
    toList (er1, er2, er3, er4, er5, er6, er7) = [er1, er2, er3, er4, er5, er6, er7]
    f = \e -> [(e, eval evalRules e), (findSimplest e rules, eval evalRules (findSimplest e rules))]


--Property testing if expression is larger than or equal in length to simplified expression
findSimplestSmallerProp :: Expr -> Bool 
findSimplestSmallerProp e = and [lengthOfExpr e >= lengthOfExpr (findSimplest e rs) 
                            | rs <- map fst (toList rulesAndCompatibleEvalRules)]
    where
        toList (er1, er2, er3, er4, er5, er6, er7) 
            = [er1, er2, er3, er4, er5, er6, er7]




t = Variable "t"
u = Variable "u"
v = Variable "v"
w = Variable "w"
x = Variable "x"
y = Variable "y"
z = Variable "z"

h = Add [Pow (Mul [N 2,V x]) (V x),Mul [Pow (Mul [N 2,V x]) (V x),N 2],Mul [V x,V x],Mul [V x,V y],Mul [V x,V z],Mul [V y,V z],Mul [N 2,V x],Mul [N 2,V y]]

ruleList = Rule <$> [(x, Mul [N 2, V z]),(y, N 3),(z,Mul [N 4, V y])]
