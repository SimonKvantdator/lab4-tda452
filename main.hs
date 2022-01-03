import Testing
import Test.QuickCheck

main = do
    -- prop1 uses numerical evaluation to check that expressions before and
    -- after function application are equal.
    putStrLn "Numerical tests of toCanonical"
    quickCheck flattenAddProp
    quickCheck flattenMulProp
    quickCheck combineTermsProp
    quickCheck expandAndCombineTermsProp
    quickCheck sortExprProp
    quickCheck expandProp
    quickCheck toCanonicalProp

    -- prop2 checks that the function actually does what is advertised
    putStrLn "Alegebraic tests of toCanonical"
    quickCheck flattenAddProp2
    quickCheck flattenMulProp2
    quickCheck combineTermsProp2
    quickCheck sortExprProp2
    quickCheck expandProp2

    putStrLn "testing findSimplest"
    quickCheck findSimplestProp
    quickCheck findSimplestSmallerProp
