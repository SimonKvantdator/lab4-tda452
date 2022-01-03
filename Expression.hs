module Expression where

import Data.List

newtype Variable = Variable String
    deriving Eq

instance Show Variable where
    show (Variable s) = s

data Op = Add | Mul
    deriving Eq

data Expr =
    N Integer
    | V Variable
    | AC Op [Expr]
    | Pow Expr Expr

-- Function determining if an expression is an addition
isAdd :: Expr -> Bool 
isAdd (AC Add es) = True
isAdd _           = False

-- Function determining if an expression is an multiplication
isMul :: Expr -> Bool 
isMul (AC Mul es) = True
isMul _           = False

-- Function determining if an expression is a power
isPow :: Expr -> Bool 
isPow (Pow _ _)   = True
isPow _           = False

-- Function determining if an expression is a variable
isVariable :: Expr -> Bool 
isVariable (V var) = True
isVariable _       = False

-- Function determining if an expression is a number
isNumeric :: Expr -> Bool 
isNumeric (N n)   = True
isNumeric _       = False

-- Extracts the list of elements in AC-operation 
fromAC :: Expr -> [Expr]
fromAC (AC op ts)  = ts
fromAC e         = [e]

-- Extracts the numeric value as an integer 
fromNumeric :: Expr -> Integer
fromNumeric (N n) = n

-- Extracts a tuple of the base and the power
fromPow :: Expr -> (Expr, Expr)
fromPow (Pow s t) = (s,t)

-- TODO: describe this operator
emap :: (Expr -> Expr) -> Expr -> Expr
emap f (AC op ts) = AC op $ map f ts
emap f (Pow e1 e2) = Pow(f e1) (f e2)
emap f e = f e
(<$$>) = emap
infixr 4 <$$>


-- INFIX OPERATORS FOR EACH OPERATION
(.+) :: Expr -> Expr -> Expr
x .+ y = AC Add [x, y]
infixl 6 .+

(.*) :: Expr -> Expr -> Expr
x .* y = AC Mul [x, y]
infixl 6 .*

(.-) :: Expr -> Expr -> Expr
x .- y = AC Add [x, N (-1) .* y]
infixl 6 .-

(.^) :: Expr -> Expr -> Expr
(.^) = Pow
infixr 8 .^

-- INSTANCES OF EQ, ORD AND SHOW
instance Eq Expr
    where
    e == f = show1 e == show1 f

instance Ord Expr
    where
    compare = comp

instance Show Expr where
     show = show1
    --show = show2 -- More verbose show


-- Shows a nice representation of the expression
show1 :: Expr -> String
show1 (V v)                     = show v
show1 (N n)                     = show n

show1 (AC Add [t])              = show1 t
show1 (AC Add (t:ts))           = show1 t ++ " + " ++ show1 (AC Add ts)
show1 (AC Add [])               = ""

show1 (AC Mul [AC Add ts])      = "(" ++ show1 (AC Add ts) ++ ")"
show1 (AC Mul [f])              = show1 f
show1 (AC Mul (AC Add ts:fs))   = "(" ++ show1 (AC Add ts) ++ ")*" ++ show1 (AC Mul fs)
show1 (AC Mul (N n:fs))
            | n < 0             = "(" ++ show n ++ ")*" ++ show1 (AC Mul fs)
show1 (AC Mul (f:fs))           = show1 f ++ "*" ++ show1 (AC Mul fs)
show1 (AC Mul [])               = ""

show1 (Pow (N n) (N m))         = show n ++ "^" ++ show m
show1 (Pow (V n) (N m))         = show n ++ "^" ++ show m
show1 (Pow (N n) (V m))         = show n ++ "^" ++ show m
show1 (Pow (V n) (V m))         = show n ++ "^" ++ show m

show1 (Pow (N n) e)             = show n ++ "^(" ++ show1 e ++ ")"
show1 (Pow (V v) e)             = show v ++ "^(" ++ show1 e ++ ")"
show1 (Pow e (N n))             = "(" ++ show1 e ++ ")^" ++ show n
show1 (Pow e (V v))             = "(" ++ show1 e ++ ")^" ++ show v

show1 (Pow x1 x2)               = "(" ++ show1 x1 ++ ")^(" ++ show1 x2 ++ ")"


-- Shows expression on the form of the data type
show2 :: Expr -> String
show2 (V v)         = "V " ++ show v
show2 (N n)
    | n < 0         = "N (" ++ show n ++ ")"
    | otherwise     = "N " ++ show n
show2 (AC Add ts)   = "AC Add [" ++ foldl (\x y -> x ++ "," ++ y) (show2 $ head ts) (show2 <$> tail ts) ++ "]"
show2 (AC Mul fs)   = "AC Mul [" ++ foldl (\x y -> x ++ "," ++ y) (show2 $ head fs) (show2 <$> tail fs) ++ "]"
show2 (Pow x y)     = "Pow (" ++ show2 x ++ ") (" ++ show2 y ++ ")"


-- Ordering placing constants at the front of expressions
comp :: Expr -> Expr -> Ordering
comp (N n) (N s)    = compare n s
comp (N n) _        = LT
comp _ (N n)        = GT
comp e1 e2          = compare (show1 e1) (show1 e2)
