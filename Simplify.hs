
-- Emma Sofia Ringström & Simon Stefanus Jacobsson
-- Grunduppgift: Skriv en simplifyer som kan förenkla ett uttryck givet en
-- mängd samband för variablerna som uttrycket består av.
-- Bonusuppgift: Skriv en parser så att man kan mata in uttryck och samband i
-- terminalen. Printa sen LaTeXkod för det förenklade uttrycket.

import Data.Maybe


newtype Variable = Var String
    deriving Eq

instance Show Variable where
    show (Var s) = s

data Expr =
    N Integer
    | V Variable
    | Add Expr AddOp Expr
    | Mul Expr MulOp Expr
    | Pow Expr Expr

data AddOp =
   Plus | Minus 

data MulOp =
    Times | Div

(.+) :: Expr -> Expr -> Expr
x .+ y = Add x Plus y
(.-) :: Expr -> Expr -> Expr
x .- y = Add x Minus y
(.*) :: Expr -> Expr -> Expr
x .* y = Mul x Times y
(./) :: Expr -> Expr -> Expr
x ./ y = Mul x Div y
(.^) :: Expr -> Expr -> Expr
(.^) = Pow


instance Show Expr where
    show (V (Var s))                        = s
    show (N x)                              = show x

    show (Add x1 Plus x2)                   = show x1 ++ " + " ++ show x2
    show (Add x1 Minus x2@(Add _ Minus _))  = show x1 ++ " - (" ++ show x2 ++ ")"
    show (Add x1 Minus x2)                  = show x1 ++ " - " ++ show x2

    show (Mul x1@Add{} Times x2@Add{})      = "(" ++ show x1 ++ ") * (" ++ show x2 ++ ")"
    show (Mul x1@Add{} Times x2)            = "(" ++ show x1 ++ ") * " ++ show x2
    show (Mul x1 Times x2@Add{})            = show x1 ++ " * (" ++ show x2 ++ ")"
    show (Mul x1 Times x2)                  = show x1 ++ " * " ++ show x2

    show (Mul x1@Add{} Div x2@Add{})        = "(" ++ show x1 ++ ") / (" ++ show x2 ++ ")"
    show (Mul x1@Add{} Div x2)              = "(" ++ show x1 ++ ") / " ++ show x2
    show (Mul x1 Div x2@Add{})              = show x1 ++ " / (" ++ show x2 ++ ")"
    show (Mul x1 Div x2)                    = show x1 ++ " / " ++ show x2

    show (Pow (N n) (N m))                  = show n ++ "^" ++ show m
    show (Pow (V n) (N m))                  = show n ++ "^" ++ show m
    show (Pow (N n) (V m))                  = show n ++ "^" ++ show m
    show (Pow (V n) (V m))                  = show n ++ "^" ++ show m

    show (Pow (N n) e)                      = show n ++ "^(" ++ show e ++ ")"
    show (Pow (V n) e)                      = show n ++ "^(" ++ show e ++ ")"
    show (Pow e (N n))                      = "(" ++ show e ++ ")^" ++ show n
    show (Pow e (V n))                      = "(" ++ show e ++ ")^" ++ show n

    show (Pow x1 x2)                        = "(" ++ show x1 ++ ")^(" ++ show x2 ++ ")"


type Rule = (Variable, Expr)

-- generateMoreRules [x = y / z] = [x = y / z, y = x / z, z = y / x]

rule :: Rule
rule = (Var "x", Mul (N 2) Times y)


applyRule :: Rule -> Expr -> Expr
-- applyRules :: [Rule] -> Expr -> Expr
-- applyRules rs (V x) = fromMaybe (V x) (lookup x rs)

-- toCanonical (x1 + x2) * (x3 + x4) = x1 * x2 + x1 * x3 +...
-- toCanonical x * (N n) = (N n) * x

-- findSimplest :: Expr -> [Rule] -> Expr
-- findSimplest expr rules = head $ sortOn lengthOfExpr (findSimplestHelper ...)

-- findSimplestHelper :: Int -> Int -> [Expr] -> [Rule] -> [Expr]
-- -- findSimplestHelper depth maxLength currentExpr rules = ...

lengthOfExpr :: Expr -> Integer
lengthOfExpr (Mul e1 _ e2)  = lengthOfExpr e1 * lengthOfExpr e2
lengthOfExpr (Add e1 _ e2)  = lengthOfExpr e1 + lengthOfExpr e2
lengthOfExpr (Pow e1 e2)    = lengthOfExpr e1 + lengthOfExpr e2 -- TODO: review this decision
lengthOfExpr _              = 1

-- TODO: QuickCheck properties
--    How do we know if a simplification is valid?
--    How do we know that the simplest form really has been found?
--
-- Bodge: Examples
x = V $ Var "x"
y = V $ Var "y"
z = V $ Var "z"
a = x
b = x .+ z
c = (x .+ y .+ z).^(x .+ y)
