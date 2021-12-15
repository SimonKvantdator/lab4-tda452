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
    arbitrary = Var . (:[]) <$> choose ('t', 'z') -- This is the right amount of variables

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

newtype EvalRules = EvalRules [(Variable, Integer)]
    deriving Show

instance Arbitrary EvalRules where
    arbitrary = EvalRules . zip (Var . (:[]) <$> ['t'..'z']) <$> infiniteListOf (choose (-5, 5))

fromEvalRules (EvalRules r) = r

-- | Evaluating Symbolic Expressions
eval :: Floating p => EvalRules -> Expr -> p
eval rules (N n)        = fromInteger n
eval rules (V x)        = fromInteger $ fromJust $ lookup x (fromEvalRules rules)
eval rules (Add ts)     = sum $ eval rules <$> ts
eval rules (Mul fs)     = product $ eval rules <$> fs
eval rules (Pow e1 e2)  = eval rules e1**eval rules e2

propFor :: (Expr -> Expr)
    -> (Expr, (EvalRules, EvalRules, EvalRules, EvalRules, EvalRules, EvalRules, EvalRules))
    -> Bool
propFor f (e, rules) = (>= 6) . length $ filter id [eval rule e ~= eval rule (f e) | rule <- toList rules]
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
    
-- List of lists of compatible rules
{-rulesAndCompatibleEvalRules :: (([Rule], EvalRules), ([Rule], EvalRules), ([Rule], EvalRules), ([Rule], EvalRules), ([Rule], EvalRules), ([Rule], EvalRules), ([Rule], EvalRules))
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
    )
-}
-- rules = fst $ rulesAndCompatibleEvalRules!!0
rules = ruleList
-- evalRules = snd $ rulesAndCompatibleEvalRules!!0
evalRules = EvalRules [(x, 24), (y, 3), (z, 12), (t, 0), (u, 0), (v, 0), (w, 0)]
a = sample $ f <$> (arbitrary :: Gen Expr)
    where
    f = \e -> [(e, eval evalRules e), (findSimplest' e rules, eval evalRules (findSimplest' e rules))]


-- Property testing if evaluating the expression equals evaluating the simplified expression
findSimplestEvalEqualProp :: Expr -> [Rule] -> EvalRules -> Bool 
findSimplestEvalEqualProp e rs ers = eval ers e == eval ers (findSimplest e rs)

--Property testing if expression is larger than or equal in length to simplified expression
findSimplestSmallerProp :: Expr -> [Rule] -> Bool 
findSimplestSmallerProp e rs = lengthOfExpr e >= lengthOfExpr (findSimplest e rs)



t = Var "t"
u = Var "u"
v = Var "v"
w = Var "w"
x = Var "x"
y = Var "y"
z = Var "z"

h = Add [Pow (Mul [N 2,V x]) (V x),Mul [Pow (Mul [N 2,V x]) (V x),N 2],Mul [V x,V x],Mul [V x,V y],Mul [V x,V z],Mul [V y,V z],Mul [N 2,V x],Mul [N 2,V y]]

ruleList = Rule <$> [(x, Mul [N 2, V z]),(y, N 3),(z,Mul [N 4, V y])]
