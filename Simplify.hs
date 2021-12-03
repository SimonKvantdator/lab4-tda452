
-- Emma Sofia Ringström & Simon Stefanus Jacobsson
-- Grunduppgift: Skriv en simplifyer som kan förenkla ett uttryck givet en
-- mängd samband för variablerna som uttrycket består av.
-- Bonusuppgift: Skriv en parser så att man kan mata in uttryck och samband i
-- terminalen. Printa sen LaTeXkod för det förenklade uttrycket.

newtype Variable = Var String
    deriving Eq

instance Show Variable where
    show (Var s) = s

type Rule = Expression -> Expression

rule :: Rule
rule (Var "x") = 2 `Times` (Var "y")

applyRule :: Rule -> Expression -> Expression

simplify :: Expression -> [Rule] -> Expression
simplifyHelper :: Int -> Int -> [Expression] -> [Rule] -> [Expression]
simplifyHelper depth maxLength currentExpressions rules = ...

lengthOfExpression :: Expression -> Integer


