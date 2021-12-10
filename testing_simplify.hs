import Simplify
import Data.Maybe

-- | Gathering variables
vars :: Expr -> [String]
vars (N n) = []
vars (V x) = [show x]
vars (Add ts) = concat $ vars <$> ts
vars (Mul fs) = concat $ vars <$> fs

-- | Substituting expressions for variables
substitute :: [(String, Expr)] -> Expr -> Expr
substitute env (N n) = N n
substitute env (V x) = fromJust $ lookup (show x) env
substitute env (Add ts) = Add $ substitute env <$> ts
substitute env (Mul fs) = Mul $ substitute env <$> fs

-- | Evaluating Symbolic Expressions
eval :: [(String,Integer)] -> Expr -> Integer
eval env (N n)   = n
eval env (V x)   = fromJust $ lookup (show x) env
eval env (Add ts) = sum $ eval env <$> ts
eval env (Mul fs) = product $ eval env <$> fs
eval env (Pow e1 e2) = (eval env e1)^(eval env e2)

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
