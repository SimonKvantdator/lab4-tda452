import Simplify
import Data.Maybe
import Test.QuickCheck

-- | Gathering variables
vars :: Expr -> [String]
vars (N n) = []
vars (V x) = [show x]
vars (Add ts) = concat $ vars <$> ts
vars (Mul fs) = concat $ vars <$> fs


-- | Evaluating Symbolic Expressions
type EvalRule = (Variable, Integer)

eval :: [EvalRule] -> Expr -> Integer
eval env (N n)   = n
eval env (V x)   = fromJust $ lookup x env
eval env (Add ts) = sum $ eval env <$> ts
eval env (Mul fs) = product $ eval env <$> fs
eval env (Pow e1 e2) = (eval env e1)^(eval env e2)
 
instance Arbitrary Variable
    where
    arbitrary = Var . (:[]) <$> choose ('t', 'z') -- TODO: how many variables should we have?

instance Arbitrary Expr
    where
    arbitrary = rExpr

rExpr :: Gen Expr
rExpr = oneof $ rExprHelper <$> [1..6]
    where
    rExprHelper :: Integer -> Gen Expr
    rExprHelper n | n < 0 = undefined
    rExprHelper 0 = oneof [N <$> arbitrary, V <$> arbitrary]
    rExprHelper n = oneof [
        N <$> arbitrary,
        V <$> arbitrary,
        Add <$> listOf1 (rExprHelper (n - 1)),
        Mul <$> listOf1 (rExprHelper (n - 1)),
        Pow <$> rExprHelper (n - 1) <*> rExprHelper (n - 1)
        ]

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
h =  ((x .* N 2) .^ x) .+ ((x .+ N 2 .+ z).*(x .+ y)) .+ (((x .* N 2) .^ x).* N 2)
