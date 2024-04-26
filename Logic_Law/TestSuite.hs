-- Test cases for distribute function
testDistribute1 = distribute (And A (Or B C)) -- Expected: Or (And A B) (And A C)
testDistribute2 = distribute (Or A (And B C)) -- Expected: And (Or A B) (Or A C)
testDistribute3 = distribute (And (And A B) (Or C D)) -- Expected: And (And A B) (Or (And A C) (And B D))
testDistribute4 = distribute (Or (Or A B) (And C D)) -- Expected: Or (Or A B) (And (Or A C) (Or B D))
testDistribute5 = distribute (And A B) -- Expected: And A B (no change)

-- Test cases for deMorgan function
testDeMorgan1 = deMorgan (Not (And A B)) -- Expected: Or (Not A) (Not B)
testDeMorgan2 = deMorgan (Not (Or A B)) -- Expected: And (Not A) (Not B)
testDeMorgan3 = deMorgan (Not (Not A)) -- Expected: Not (Not A) (no change, nested negation)
testDeMorgan4 = deMorgan (And (Not A) (Not B)) -- Expected: And (Not (Or A B)) (Not (Or A B))
testDeMorgan5 = deMorgan (Or (Not A) (Not B)) -- Expected: Or (Not (And A B)) (Not (And A B))

main :: IO ()
main = do
    putStrLn "Testing distribute function:"
    print testDistribute1
    print testDistribute2
    print testDistribute3
    print testDistribute4
    print testDistribute5

    putStrLn "\nTesting deMorgan function:"
    print testDeMorgan1
    print testDeMorgan2
    print testDeMorgan3
    print testDeMorgan4
    print testDeMorgan5
