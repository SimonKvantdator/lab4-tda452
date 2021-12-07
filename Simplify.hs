
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
    -- | Add [Expr]
    -- | Mul [Expr]
    | Mul Expr MulOp Expr
    | Pow Expr Expr

-- sortFun (V Var s) = s
-- sortFun (N n) = show n

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

    show (Mul x1@Add{} Times x2@Add{})      = "(" ++ show x1 ++ ")*(" ++ show x2 ++ ")"
    show (Mul x1@Add{} Times x2)            = "(" ++ show x1 ++ ")*" ++ show x2
    show (Mul x1 Times x2@Add{})            = show x1 ++ "*(" ++ show x2 ++ ")"
    show (Mul x1 Times x2)                  = show x1 ++ "*" ++ show x2

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
applyRule (x, e) (V y)
    | x == y                = e
applyRule r (Add e1 op e2)  = Add (applyRule r e1) op (applyRule r e2)
applyRule r (Mul e1 op e2)  = Mul (applyRule r e1) op (applyRule r e2)
applyRule r (Pow e1 e2)     = Pow (applyRule r e1) (applyRule r e2)
applyRule r e               = e



-- applyRules :: [Rule] -> Expr -> Expr
-- applyRules rs (V x) = fromMaybe (V x) (lookup x rs)

toCanonical :: Expr -> Expr
toCanonical (Mul (Add x1 Plus y1) Times (Add x2 Plus y2))   = toCanonical (x1 .* x2) .+ toCanonical (x1 .* y2) .+ toCanonical (y1 .* x2) .+ toCanonical (y1 .* y2)
toCanonical (Mul (Add x1 Plus y1) Times (Add x2 Minus y2))  = toCanonical (x1 .* x2) .- toCanonical (x1 .* y2) .+ toCanonical (y1 .* x2) .- toCanonical (y1 .* y2)
toCanonical (Mul (Add x1 Minus y1) Times (Add x2 Plus y2))  = toCanonical (x1 .* x2) .+ toCanonical (x1 .* y2) .- toCanonical (y1 .* x2) .- toCanonical (y1 .* y2)
toCanonical (Mul (Add x1 Minus y1) Times (Add x2 Minus y2)) = toCanonical (x1 .* x2) .- toCanonical (x1 .* y2) .- toCanonical (y1 .* x2) .+ toCanonical (y1 .* y2)
toCanonical (Mul (Add x Plus y) Times e)                    = toCanonical (x .* e) .+ toCanonical (y .* e)
toCanonical (Mul (Add x Minus y) Times e)                   = toCanonical (x .* e) .- toCanonical (y .* e)
toCanonical (Mul e Times (Add x Plus y))                    = toCanonical (x .* e) .+ toCanonical (y .* e)
toCanonical (Mul e Times (Add x Minus y))                   = toCanonical (x .* e) .- toCanonical (y .* e)

toCanonical (Mul e Times (N n)) = N n .* e
toCanonical e = e


-- simplify :: Expr -> Expr
-- simplify (Add (V x) Minus (V y)) | x == y = N 0
-- simplify (Add x _ (N 0)) = x
-- simplify (Add (N 0) Plus x) = x
-- simplify (Add (Mul (N n) Times x1) Plus x2) | x1 == x2 = Mul (N (n + 1)) Times x1
-- simplify (Add x1 Plus (Mul (N n) Times x2)) | x1 == x2 = Mul (N (n + 1)) Times x1

-- findSimplest :: Expr -> [Rule] -> Expr
-- findSimplest expr rules = head $ sortOn lengthOfExpr (findSimplestHelper ...)

-- findSimplestHelper :: Int -> Int -> [Expr] -> [Rule] -> [Expr]
-- findSimplestHelper depth maxLength currentExpr rules = ...

lengthOfExpr :: Expr -> Integer
lengthOfExpr (Mul e1 _ e2)  = lengthOfExpr e1 * lengthOfExpr e2
lengthOfExpr (Add e1 _ e2)  = lengthOfExpr e1 + lengthOfExpr e2
lengthOfExpr (Pow e1 e2)    = lengthOfExpr e1
lengthOfExpr _              = 1

-- TODO: QuickCheck properties
--    How do we know if a simplification is valid?
--    How do we know that the simplest form really has been found?
--
--    Generate sets of integers values for x, y, etc, and check if they satisfy the assumptions.
--    Then test if the expressions are equal if evaluated for these x, y, etc.
--
-- Bodge: Examples
x = V $ Var "x"
y = V $ Var "y"
z = V $ Var "z"
a = x
b = x .+ z
c = (x .+ y .+ z).^(x .+ y)
d = (x .+ y .+ z).*(x .+ y)
