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
fromMul e         = [e] -- TODO: reconsider

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

-- TODO: describe this operator
emap :: (Expr -> Expr) -> Expr -> Expr
emap f (Add ts) = Add $ map f ts
emap f (Mul fs) = Mul $ map f fs
emap f (Pow e1 e2) = Pow (f e1) (f e2)
emap f e = f e
(<$$>) = emap
infixr 4 <$$>

-- TODO: how to deal with empty Adds and Muls?
-- Does it make sense to simplify Add [] to 0 and Mul [] to 1?

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
    show (V (Var s))        = s
    show (N n)              = show n

    show (Add [t])          = show t
    show (Add (t:ts))       = show t ++ " + " ++ show (Add ts)
    show (Add [])           = ""

    show (Mul [Add ts])     = "(" ++ show (Add ts) ++ ")"
    show (Mul [f])          = show f
    show (Mul (Add ts:fs))  = "(" ++ show (Add ts) ++ ")*" ++ show (Mul fs)
    show (Mul (N n:fs))
        | n < 0             = "(" ++ show n ++ ")*" ++ show (Mul fs)
    show (Mul (f:fs))       = show f ++ "*" ++ show (Mul fs)
    show (Mul [])           = ""

    show (Pow (N n) (N m))  = show n ++ "^" ++ show m
    show (Pow (V n) (N m))  = show n ++ "^" ++ show m
    show (Pow (N n) (V m))  = show n ++ "^" ++ show m
    show (Pow (V n) (V m))  = show n ++ "^" ++ show m

    show (Pow (N n) e)      = show n ++ "^(" ++ show e ++ ")"
    show (Pow (V n) e)      = show n ++ "^(" ++ show e ++ ")"
    show (Pow e (N n))      = "(" ++ show e ++ ")^" ++ show n
    show (Pow e (V n))      = "(" ++ show e ++ ")^" ++ show n

    show (Pow x1 x2)        = "(" ++ show x1 ++ ")^(" ++ show x2 ++ ")"

-- More verbose show
show' :: Expr -> String
show' (V v)         = "V " ++ show v
show' (N n)         = "N " ++ show n
show' (Add ts)      = "Add [" ++ foldl (\x y -> x ++ "," ++ y) (show' $ head ts) (show' <$> tail ts) ++ "]"
show' (Mul fs)      = "Mul [" ++ foldl (\x y -> x ++ "," ++ y) (show' $ head fs) (show' <$> tail fs) ++ "]"
show' (Pow x y)     = "Pow (" ++ show' x ++ ") (" ++ show' y ++ ")"

-- TODO: do we really want to do this?
instance Eq Expr
    where
    e == f = show e == show f

instance Ord Expr
    where
    compare = comp

sortTerms :: [Expr] -> [Expr]
sortTerms = sortBy comp

comp :: Expr -> Expr -> Ordering
comp (N n) (N s)                                                         = compare n s
comp (N n) _                                                             = LT
comp _ (N n)                                                             = GT
comp e1 e2 = compare (show e1) (show e2)
