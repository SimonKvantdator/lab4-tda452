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

-- Transforms a rule to simply a tuple of a variable and an expression
fromRule :: Rule -> (Variable, Expr)
fromRule (Rule (x, e)) = (x, e)


-- Applies a rule to an expression
applyRule :: Rule -> Expr -> Expr
applyRule (Rule (x, e)) (V y) | x == y  = e
applyRule r (Add ts)                    = Add (applyRule r <$> ts)
applyRule r (Mul fs)                    = Mul (applyRule r <$> fs)
applyRule r (Pow e1 e2)                 = Pow (applyRule r e1) (applyRule r e2)
applyRule r e                           = e


-- Finds simplest canonical expression using a list of rules
findSimplest :: Expr -> [Rule] -> Expr
findSimplest e rs = minimumBy (compare `on` lengthOfExpr) $
    findSimplestHelper depth [e] rs
    where
        findSimplestHelper :: Int -> [Expr] -> [Rule] -> [Expr]
        findSimplestHelper 0 es _  = es
        findSimplestHelper d es rs =
            es ++ findSimplestHelper
                (d - 1)
                (
                    filter ((< cutoff) . lengthOfExpr) $
                    sortBy (compare `on` lengthOfExpr) $
                    nub
                    (toCanonical <$> [applyRule r e| r <- rs, e <- es])
                )
                rs
        depth = 7
        cutoff = lengthOfExpr e + 2


-- Calculates the length of an expression (number of terms)
lengthOfExpr :: Expr -> Integer
lengthOfExpr (Add ts)       = sum $ lengthOfExpr <$> ts
lengthOfExpr (Mul fs)       = product $ lengthOfExpr <$> fs
lengthOfExpr (Pow e1 e2)    = 1 
lengthOfExpr _              = 1


-- Creates string on the form of LaTeX code for a simplified expression
toLatex :: Expr -> String
toLatex (Add [e])               = toLatex e
toLatex (Add (e:es))            = toLatex e ++ " + " ++ toLatex (Add es)
toLatex (Mul [e])               = toLatex e
toLatex (Mul (e:es))            = toLatex e ++  toLatex (Mul es)
toLatex (Pow e1@(V x) e2)       = toLatex e1 ++ " ^{ " ++ toLatex e2 ++ " }"
toLatex (Pow e1@(N n) e2)       = toLatex e1 ++ " ^{ " ++ toLatex e2 ++ " }"
toLatex (Pow e1 e2)             = " ( " ++ toLatex e1 ++ " ) " ++ " ^{ " ++ toLatex e2 ++ " }"
toLatex (V x)                   = show x
toLatex (N n)                   = show n
toLatex _                       = ""


-- Prints the LaTeX code for a simplified expression in the terminal
printLatex :: Expr -> IO()
printLatex e = putStrLn $ "$ " ++ toLatex e ++ " $"

