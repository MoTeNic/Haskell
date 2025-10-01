-- Tasks.hs
-- HC2T1 - HC2T7 Solutions

module Main where

-- HC2T2: Function signatures and implementations
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings a b = a ++ b

-- HC2T3: Immutable variables
myAge :: Int
myAge = 22

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

-- HC2T5: Functions
circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c = max a (max b c)

-- HC2T6: Int vs Integer
smallNumber :: Int
smallNumber = 262

bigNumber :: Integer
bigNumber = 2127

-- HC2T7: Boolean expressions
expr1 :: Bool
expr1 = (5 > 2) && (3 < 4)   -- True

expr2 :: Bool
expr2 = False || False       -- False

expr3 :: Bool
expr3 = not False            -- True

expr4 :: Bool
expr4 = 10 < 5               -- False

-- Main function to demonstrate everything
main :: IO ()
main = do
    putStrLn "=== HC2T1: Checking Types (sample values) ==="
    print (42 :: Int)
    print (3.14 :: Double)
    print ("Haskell" :: String)
    print ('Z' :: Char)
    print (True && False)

    putStrLn "\n=== HC2T2: Function Tests ==="
    print (add 5 7)
    print (isEven 10)
    print (isEven 7)
    print (concatStrings "Hello, " "World!")

    putStrLn "\n=== HC2T3: Immutable Variables ==="
    print myAge
    print piValue
    print greeting
    print isHaskellFun

    putStrLn "\n=== HC2T4: Infix vs Prefix ==="
    print ((+) 5 3)
    print (10 * 4)
    print ((&&) True False)
    print (7 + 2)
    print ((* ) 6 5)
    print (True && False)

    putStrLn "\n=== HC2T5: Function Tests ==="
    print (circleArea 5)
    print (maxOfThree 3 7 2)
    print (maxOfThree 10 4 9)

    putStrLn "\n=== HC2T6: Int vs Integer ==="
    print smallNumber
    print bigNumber
    putStrLn "Evaluating 2^64 as Int may overflow, as Integer it works:"
    print (2^64 :: Int)
    print (2^64 :: Integer)

    putStrLn "\n=== HC2T7: Boolean Expressions ==="
    print expr1
    print expr2
    print expr3
    print expr4
