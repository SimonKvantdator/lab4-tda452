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

-- | Gathering variables
vars :: Expr -> [Variable]
vars (N n) = []
vars (V x) = [x]
vars (Add ts) = concat $ vars <$> ts
vars (Mul fs) = concat $ vars <$> fs
vars (Pow e1 e2) = vars e1 ++ vars e2

-- | Evaluating Symbolic Expressions
type EvalRule = (Variable, Integer)


eval :: Floating p => [EvalRule] -> Expr -> p
eval rules (N n)        = fromInteger n
eval rules (V x)        = fromInteger $ fromJust $ lookup x rules
eval rules (Add ts)     = sum $ eval rules <$> ts
eval rules (Mul fs)     = product $ eval rules <$> fs
eval rules (Pow e1 e2)  = (eval rules e1)**(eval rules e2)
 
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

rRulesForExpr :: Expr -> Gen [EvalRule]
rRulesForExpr e =
    zip (vars e) <$> infiniteListOf (choose (-3, 3)) -- these values don't need to be large

flattenAddProp :: Expr -> Bool
flattenAddProp e = n1 ~= n2
    where 
    n1 = eval rules e
    n2 = eval rules $ flattenAdd e
    rules = zip (Var . (:[]) <$> ['t'..'z']) [1..]

flattenMulProp :: Expr -> Bool
flattenMulProp e = n1 ~= n2
    where 
    n1 = eval rules e
    n2 = eval rules $ flattenMul e
    rules = zip (Var . (:[]) <$> ['t'..'z']) [1..]

sortExprProp :: Expr -> Bool
sortExprProp e = n1 ~= n2
    where 
    n1 = eval rules e
    n2 = eval rules $ sortExpr $ flattenAdd $ flattenMul e
    rules = zip (Var . (:[]) <$> ['t'..'z']) [1..]

expandProp :: Expr -> Bool
expandProp e = n1 ~= n2
    where 
    n1 = eval rules e
    n2 = eval rules $ expand e
    rules = zip (Var . (:[]) <$> ['t'..'z']) [1..]

toCanonicalProp :: Expr -> Bool
toCanonicalProp e = n1 ~= n2
    where 
    n1 = eval rules e
    n2 = eval rules $ toCanonical e
    rules = zip (Var . (:[]) <$> ['t'..'z']) [1..]

