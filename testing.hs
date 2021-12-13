-- Function eval was taken from from lecture 4A

import Expression
import ToCanonical
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

newtype EvalRule = Er [(Variable, Integer)]
    deriving Show

instance Arbitrary EvalRule
    where
    arbitrary = Er . zip (Var . (:[]) <$> ['t'..'z']) <$> infiniteListOf ( choose (-3, 3))

fromEvalRule (Er r) = r

-- | Evaluating Symbolic Expressions
eval :: Floating p => EvalRule -> Expr -> p
eval rule (N n)        = fromInteger n
eval rule (V x)        = fromInteger $ fromJust $ lookup x (fromEvalRule rule)
eval rule (Add ts)     = sum $ eval rule <$> ts
eval rule (Mul fs)     = product $ eval rule <$> fs
eval rule (Pow e1 e2)  = (eval rule e1)**(eval rule e2)
 
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
    rExprHelper 0 = oneof [N <$> arbitrary, V <$> arbitrary]
    rExprHelper n = frequency [
        (3, Add <$> vectorOf 2 (rExprHelper (n - 1))),
        (3, Mul <$> vectorOf 2 (rExprHelper (n - 1))),
        (1, Pow <$> rExprHelper (n - 1) <*> rExprHelper (n - 1))
        ]
    arbitraryPositiveInteger = getPositive <$> (arbitrary :: Gen (Positive Integer))
    -- arbitraryPositiveInteger = choose (1, 9)

-- rRulesForExpr :: Expr -> Gen [EvalRule]
-- rRulesForExpr e =
--     zip (vars e) <$> infiniteListOf (choose (-3, 3)) -- these values don't need to be large

flattenAddProp :: (Expr, EvalRule) -> Bool
flattenAddProp (e, rule) = n1 ~= n2
    where 
    n1 = eval rule e
    n2 = eval rule $ flattenAdd e

flattenMulProp :: (Expr, EvalRule) -> Bool
flattenMulProp (e, rule) = n1 ~= n2
    where 
    n1 = eval rule e
    n2 = eval rule $ flattenMul e

combineTermsProp :: (Expr, EvalRule) -> Bool
combineTermsProp (e, rule) = n1 ~= n2
    where 
    n1 = eval rule e
    n2 = eval rule $ combineTerms e

expandAndCombineTermsProp :: (Expr, EvalRule) -> Bool
expandAndCombineTermsProp (e, rule) = n1 ~= n2
    where 
    n1 = eval rule e
    n2 = eval rule $ combineTerms $ expand e

sortExprProp :: (Expr, EvalRule) -> Bool
sortExprProp (e, rule) = n1 ~= n2
    where 
    n1 = eval rule e
    n2 = eval rule $ sortExpr e

expandProp :: (Expr, EvalRule) -> Bool
expandProp (e, rule) = n1 ~= n2
    where 
    n1 = eval rule e
    n2 = eval rule $ expand e

toCanonicalProp :: (Expr, EvalRule) -> Bool
toCanonicalProp (e, rule) = n1 ~= n2
    where 
    n1 = eval rule e
    n2 = eval rule $ toCanonical e

