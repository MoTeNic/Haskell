-- Haskell Chapter 6 Practical Tasks: Recursion and List Processing

-------------------------------
-- HC6T1: Factorial (Recursive)
-------------------------------
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-------------------------------
-- HC6T2: Fibonacci (Recursive)
-------------------------------
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-------------------------------
-- HC6T3: Sum of Elements Using foldr
-------------------------------
sumFoldr :: [Int] -> Int
sumFoldr = foldr (+) 0

-------------------------------
-- HC6T4: Product of Elements Using foldl
-------------------------------
productFoldl :: [Int] -> Int
productFoldl = foldl (*) 1

-------------------------------
-- HC6T5: Reverse a List (Recursive)
-------------------------------
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-------------------------------
-- HC6T6: Element Exists in List
-------------------------------
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists x (y:ys)
  | x == y = True
  | otherwise = elementExists x ys

-------------------------------
-- HC6T7: List Length
-------------------------------
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

-------------------------------
-- HC6T8: Filter Even Numbers
-------------------------------
filterEven :: [Int] -> [Int]
filterEven = filter even

-------------------------------
-- HC6T9: Map Implementation
-------------------------------
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-------------------------------
-- HC6T10: Digits of a Number (Recursive)
-------------------------------
digits :: Int -> [Int]
digits n
  | n < 0     = error "Digits not defined for negative numbers"
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

-------------------------------
-- Main: Run all tasks
-------------------------------
main :: IO ()
main = do
  -- HC6T1
  let n1 = 5
  putStrLn ("HC6T1: Factorial of " ++ show n1 ++ " = " ++ show (factorial n1))

  -- HC6T2
  let n2 = 10
  putStrLn ("HC6T2: Fibonacci(" ++ show n2 ++ ") = " ++ show (fibonacci n2))

  -- HC6T3
  let nums3 = [1,2,3,4,5]
  putStrLn ("HC6T3: Sum using foldr = " ++ show (sumFoldr nums3))

  -- HC6T4
  let nums4 = [1,2,3,4,5]
  putStrLn ("HC6T4: Product using foldl = " ++ show (productFoldl nums4))

  -- HC6T5
  let nums5 = [1,2,3,4,5]
  putStrLn ("HC6T5: Reversed list = " ++ show (reverseList nums5))

  -- HC6T6
  let e6 = 3
      nums6 = [1,2,3,4,5]
  putStrLn ("HC6T6: Element " ++ show e6 ++ " exists = " ++ show (elementExists e6 nums6))

  -- HC6T7
  let nums7 = [1,2,3,4,5]
  putStrLn ("HC6T7: Length of list = " ++ show (listLength nums7))

  -- HC6T8
  let nums8 = [1,2,3,4,5,6]
  putStrLn ("HC6T8: Even numbers = " ++ show (filterEven nums8))

  -- HC6T9
  let nums9 = [1,2,3,4,5]
  putStrLn ("HC6T9: myMap (*2) = " ++ show (myMap (*2) nums9))

  -- HC6T10
  let n10 = 12345
  putStrLn ("HC6T10: Digits of " ++ show n10 ++ " = " ++ show (digits n10))
