module TestSuite where

import Logic

-- Helper function to assert equality of Logic expressions
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    if expected == actual
        then putStrLn $ label ++ " passed"
        else putStrLn $ label ++ " FAILED: expected " ++ show expected ++ ", got " ++ show actual

-- Test cases for distribute function
testDistribute1 = assertEqual "testDistribute1" (Or (And A B) (And A C)) (distribute (And A (Or B C)))
testDistribute2 = assertEqual "testDistribute2" (And (Or A B) (Or A C)) (distribute (Or A (And B C)))

-- Test cases for deMorgan function
testDeMorgan1 = assertEqual "testDeMorgan1" (And (Not A) (Not B)) (deMorgan (Not (Or A B)))
testDeMorgan2 = assertEqual "testDeMorgan2" (Or (Not A) (Not B)) (deMorgan (Not (And A B)))

-- Main function to run all tests
main :: IO ()
main = do
    testDistribute1
    testDistribute2
    testDeMorgan1
    testDeMorgan2
