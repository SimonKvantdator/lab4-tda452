module Expression where

import Data.List

newtype Variable = Var String
    deriving Eq

instance Show Variable where
    show (Var s) = s

data Expr =
    N Integer
    | V Variable
    | Add [Expr]
    | Mul [Expr]
    | Pow Expr Expr


isAdd :: Expr -> Bool
isAdd (Add _)   = True
isAdd _         = False

fromAdd :: Expr -> [Expr]
fromAdd (Add ts)  = ts
fromAdd e         = [e]

isMul :: Expr -> Bool
isMul (Mul _)   = True
isMul _         = False

fromMul :: Expr -> [Expr]
fromMul (Mul fs)  = fs
fromMul e         = [e]

isVariable :: Expr -> Bool
isVariable (V _) = True
isVariable  _ = False

isNumeric :: Expr -> Bool
isNumeric (N _) = True
isNumeric _ = False

fromNumeric :: Expr -> Integer
fromNumeric (N n) = n

isPow :: Expr -> Bool
isPow (Pow _ _)   = True
isPow _         = False

fromPow :: Expr -> (Expr, Expr)
fromPow (Pow s t) = (s,t)

-- TODO: describe this operator
emap :: (Expr -> Expr) -> Expr -> Expr
emap f (Add ts) = Add $ map f ts
emap f (Mul fs) = Mul $ map f fs
emap f (Pow e1 e2) = Pow (f e1) (f e2)
emap f e = f e
(<$$>) = emap
infixr 4 <$$>

(.+) :: Expr -> Expr -> Expr
x .+ y = Add [x, y]
infixl 6 .+

(.*) :: Expr -> Expr -> Expr
x .* y = Mul [x, y]
infixl 6 .*

(.-) :: Expr -> Expr -> Expr
x .- y = Add [x, N (-1) .* y]
infixl 6 .-

-- (./) :: Expr -> Expr -> Expr
-- x ./ y = TODO

(.^) :: Expr -> Expr -> Expr
(.^) = Pow
infixr 8 .^

-- TODO: make minus look nice
instance Show Expr where
    show = show1
    -- show = show2 -- More verbose show

show1 :: Expr -> String
show1 (V v)              = show v
show1 (N n)              = show n

show1 (Add [t])          = show1 t
show1 (Add (t:ts))       = show1 t ++ " + " ++ show1 (Add ts)
show1 (Add [])           = ""

show1 (Mul [Add ts])     = "(" ++ show1 (Add ts) ++ ")"
show1 (Mul [f])          = show1 f
show1 (Mul (Add ts:fs))  = "(" ++ show1 (Add ts) ++ ")*" ++ show1 (Mul fs)
show1 (Mul (N n:fs))
    | n < 0             = "(" ++ show n ++ ")*" ++ show1 (Mul fs)
show1 (Mul (f:fs))       = show1 f ++ "*" ++ show1 (Mul fs)
show1 (Mul [])           = ""

show1 (Pow (N n) (N m))  = show n ++ "^" ++ show m
show1 (Pow (V n) (N m))  = show n ++ "^" ++ show m
show1 (Pow (N n) (V m))  = show n ++ "^" ++ show m
show1 (Pow (V n) (V m))  = show n ++ "^" ++ show m

show1 (Pow (N n) e)      = show n ++ "^(" ++ show1 e ++ ")"
show1 (Pow (V v) e)      = show v ++ "^(" ++ show1 e ++ ")"
show1 (Pow e (N n))      = "(" ++ show1 e ++ ")^" ++ show n
show1 (Pow e (V v))      = "(" ++ show1 e ++ ")^" ++ show v

show1 (Pow x1 x2)        = "(" ++ show1 x1 ++ ")^(" ++ show1 x2 ++ ")"

show2 :: Expr -> String
show2 (V v)     = "V " ++ show v
show2 (N n)
    | n < 0     = "N (" ++ show n ++ ")"
    | otherwise = "N " ++ show n
show2 (Add ts)  = "Add [" ++ foldl (\x y -> x ++ "," ++ y) (show2 $ head ts) (show2 <$> tail ts) ++ "]"
show2 (Mul fs)  = "Mul [" ++ foldl (\x y -> x ++ "," ++ y) (show2 $ head fs) (show2 <$> tail fs) ++ "]"
show2 (Pow x y) = "Pow (" ++ show2 x ++ ") (" ++ show2 y ++ ")"

instance Eq Expr
    where
    e == f = show1 e == show1 f

instance Ord Expr
    where
    compare = comp

sortTerms :: [Expr] -> [Expr]
sortTerms = sortBy comp

comp :: Expr -> Expr -> Ordering
comp (N n) (N s)                                                         = compare n s
comp (N n) _                                                             = LT
comp _ (N n)                                                             = GT
comp e1 e2 = compare (show1 e1) (show1 e2)
