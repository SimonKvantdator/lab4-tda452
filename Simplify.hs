-- Emma Sofia Ringström & Simon Stefanus Jacobsson
-- Grunduppgift: Skriv en simplifyer som kan förenkla ett uttryck givet en
-- mängd samband för variablerna som uttrycket består av.
-- Bonusuppgift: Skriv en parser så att man kan mata in uttryck och samband i
-- terminalen. Printa sen LaTeXkod för det förenklade uttrycket.

module Simplify where

import Expression
import ToCanonical
import Data.List
-- import Data.List.Unique
import Data.Function (on)

newtype Rule = Rule (Variable, Expr)
    deriving Show

fromRule :: Rule -> (Variable, Expr)
fromRule (Rule (x, e)) = (x, e)

-- generateMoreRules [x = y / z] = [x = y / z, y = x / z, z = y / x]

applyRule :: Rule -> Expr -> Expr
applyRule (Rule (x, e)) (V y) | x == y  = e
applyRule r (Add ts)                    = Add (applyRule r <$> ts)
applyRule r (Mul fs)                    = Mul (applyRule r <$> fs)
applyRule r (Pow e1 e2)                 = Pow (applyRule r e1) (applyRule r e2)
applyRule r e                           = e

-- applies a list of rules in order to an expression
applyRules :: [Rule] -> Expr -> Expr
applyRules [] e = e
applyRules rs e = applyRules (tail rs) (applyRule (head rs) e)

findSimplest :: Expr -> [Rule] -> Expr
findSimplest e rs = head $ findSimplestHelper depth [e] rs
    where
    findSimplestHelper :: Int -> [Expr] -> [Rule] -> [Expr]
    findSimplestHelper 0 es _  = es
    findSimplestHelper d es rs =
        findSimplestHelper
            (d - 1)
            (take numToKeepEachIteration $
                sortBy (compare `on` lengthOfExpr) $
                nub $ toCanonical <$> [applyRule r e| r <- rs, e <- es])
            rs
    depth = 30
    numToKeepEachIteration = 10

findSimplest' :: Expr -> [Rule] -> Expr
findSimplest' e rs = minimumBy (compare `on` lengthOfExpr) $
    map toCanonical (findSimplestHelper e $
    (map . map) Rule $
    nub $
    concatMap subsequences (permutations $ fromRule <$> rs))
    where
    findSimplestHelper :: Expr -> [[Rule]] -> [Expr]
    findSimplestHelper e []     = []
    findSimplestHelper e rss    = applyRules (head rss) e:findSimplestHelper e (tail rss)

lengthOfExpr :: Expr -> Integer
lengthOfExpr (Add ts)       = sum $ lengthOfExpr <$> ts
lengthOfExpr (Mul fs)       = product $ lengthOfExpr <$> fs
lengthOfExpr (Pow e1 e2)    = 1 --lengthOfExpr e1
lengthOfExpr _              = 1

-- TODO: fix toLatex $ Add [(N 16) .* (x), N (-9), N 17, Mul [N (-14), N (-4), N 12, y]]
toLatex :: Expr -> String
toLatex (Add [e])               = toLatex e
toLatex (Add (e:es))            = toLatex e ++ " + " ++ toLatex (Add es)
toLatex (Mul [e])               = toLatex e
toLatex (Mul (e:es))            = toLatex e ++  toLatex (Mul es)
toLatex (Pow e1@(V (Var x)) e2) = toLatex e1 ++ " ^{ " ++ toLatex e2 ++ " }"
toLatex (Pow e1@(N n) e2)       = toLatex e1 ++ " ^{ " ++ toLatex e2 ++ " }"
toLatex (Pow e1 e2)             = "(" ++ toLatex e1 ++ ")" ++ " ^{ " ++ toLatex e2 ++ " }"
toLatex (V (Var x))             = x
toLatex (N n)                   = show n
toLatex _                       = ""


printLatex :: Expr -> IO()
printLatex e = putStrLn $ "$" ++ toLatex e ++ "$"

-- x = V $ Var "x"
-- y = V $ Var "y"
-- z = V $ Var "z"

-- h' = Add [Pow (Mul [N 2,x]) x,Mul [Pow (Mul [N 2,x]) x,N 2],Mul [x,x],Mul [x,y],Mul [x,z],Mul [y,z],Mul [N 2,x],Mul [N 2,y]]

-- ruleList = Rule <$> [(Var "x", Mul [N 2, V (Var "z")]),(Var "y", N 3),(Var "z",Mul [N 4, V (Var "y")])]
