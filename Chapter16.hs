-- Chapter 16
import Data.Char (toUpper)
import Data.List (nub, sort, group, sortOn)

-- HC16T1
reverseString :: String -> String
reverseString = reverse

-- HC16T2
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

-- HC16T3
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC16T4
filterEven :: [Int] -> [Int]
filterEven = filter even

-- HC16T5
uppercaseString :: String -> String
uppercaseString = map toUpper

-- HC16T6
nthFibonacci :: Int -> Integer
nthFibonacci n
  | n <= 0    = 0
  | n == 1    = 1
  | otherwise = nthFibonacci (n - 1) + nthFibonacci (n - 2)

-- HC16T7
elementExists :: Eq a => a -> [a] -> Bool
elementExists = elem

-- HC16T8
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insert y zs

-- HC16T9
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = nub

-- HC16T10
charFrequency :: String -> [(Char, Int)]
charFrequency s = map (\grp -> (head grp, length grp)) . group . sort $ s

-- Main 
main :: IO ()
main = do
    putStrLn "=== HC16T1: Reverse a String ==="
    print $ reverseString "hello"

    putStrLn "\n=== HC16T2: Palindrome Checker ==="
    print $ isPalindrome "radar"
    print $ isPalindrome "hello"

    putStrLn "\n=== HC16T3: Factorial ==="
    print $ factorial 5

    putStrLn "\n=== HC16T4: Filter Even Numbers ==="
    print $ filterEven [1..10]

    putStrLn "\n=== HC16T5: Uppercase String ==="
    print $ uppercaseString "hello world"

    putStrLn "\n=== HC16T6: nth Fibonacci Number ==="
    print $ nthFibonacci 10

    putStrLn "\n=== HC16T7: Element Existence in List ==="
    print $ elementExists 5 [1,2,3,4,5]
    print $ elementExists 6 [1,2,3,4,5]

    putStrLn "\n=== HC16T8: Insertion Sort ==="
    print $ insertionSort [5,2,9,1,5,6]

    putStrLn "\n=== HC16T9: Remove Duplicates from List ==="
    print $ removeDuplicates [1,2,2,3,3,3,4]

    putStrLn "\n=== HC16T10: Character Frequency in String ==="
    print $ charFrequency "hello world"
