-- Chapter 12 Practical Tasks


import Data.Char (toLower, toUpper)
import Data.List (sort)
import Control.Exception (catch, IOException)

-- HC12T1: Print a Welcome Message
welcomeMessage :: IO ()
welcomeMessage = putStrLn "Welcome to Haskell Programming!"

-- HC12T2: Add Two Numbers
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

-- HC12T3: Factorial Function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC12T4: First 10 Fibonacci Numbers
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fibonacciList :: [Int]
fibonacciList = [fibonacci n | n <- [0..9]]

-- HC12T5: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome s = cleaned == reverse cleaned
  where cleaned = map toLower (filter (/= ' ') s)

-- HC12T6: Sort a List of Integers
sortIntegers :: [Int] -> [Int]
sortIntegers = sort

-- HC12T7: Calculate Circle Area
calculateCircleArea :: Floating a => a -> a
calculateCircleArea r = pi * r * r

-- HC12T8: Merge Two Sorted Lists
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
  | x < y     = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

-- HC12T9: Read and Print File Content (safe fallback)
readAndPrintFile :: FilePath -> IO ()
readAndPrintFile filename = do
    putStrLn $ "Attempting to read: " ++ filename
    content <- safeRead filename
    putStrLn "\nFile content:"
    putStrLn content
  where
    safeRead :: FilePath -> IO String
    safeRead f = readFile f `catch` handleErr
    handleErr :: IOException -> IO String
    handleErr _ = return "⚠️ Could not read file (permission denied or not found). Showing fallback text instead."

-- HC12T10: Mathematical Operations
add :: Int -> Int -> Int
add x y = x + y

subtract' :: Int -> Int -> Int
subtract' x y = x - y

multiply :: Int -> Int -> Int
multiply x y = x * y

divide :: Float -> Float -> Float
divide x y =
  if y /= 0
     then x / y
     else error "Division by zero"

-- Main function to run all tasks
main :: IO ()
main = do
    putStrLn "=== HC12T1: Welcome Message ==="
    welcomeMessage

    putStrLn "\n=== HC12T2: Add Two Numbers ==="
    putStrLn $ "5 + 3 = " ++ show (addTwoNumbers 5 3)

    putStrLn "\n=== HC12T3: Factorial ==="
    putStrLn $ "Factorial of 5 = " ++ show (factorial 5)

    putStrLn "\n=== HC12T4: First 10 Fibonacci Numbers ==="
    print fibonacciList

    putStrLn "\n=== HC12T5: Palindrome Checker ==="
    putStrLn $ "'madam' is palindrome? " ++ show (isPalindrome "madam")
    putStrLn $ "'hello' is palindrome? " ++ show (isPalindrome "hello")

    putStrLn "\n=== HC12T6: Sort Integers ==="
    print $ sortIntegers [5,1,4,2,3]

    putStrLn "\n=== HC12T7: Calculate Circle Area ==="
    putStrLn $ "Area of circle with radius 5 = " ++ show (calculateCircleArea 5)

    putStrLn "\n=== HC12T8: Merge Two Sorted Lists ==="
    print $ mergeLists [1,3,5] [2,4,6]

    putStrLn "\n=== HC12T9: Read and Print File Content ==="
    readAndPrintFile "example.txt"

    putStrLn "\n=== HC12T10: Mathematical Operations ==="
    putStrLn $ "5 + 3 = " ++ show (add 5 3)
    putStrLn $ "10 - 4 = " ++ show (subtract' 10 4)
    putStrLn $ "6 * 7 = " ++ show (multiply 6 7)
    putStrLn $ "20 / 5 = " ++ show (divide 20 5)
