-- ==========================================
-- HC5_Assignment.hs
-- HC5T1 – HC5T10
-- ==========================================

import Data.Char (isUpper)

-- HC5T1: Using applyTwice
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

-- HC5T2: Filtering Odd Numbers
oddNumbers :: [Int]
oddNumbers = filter odd [1..30]

-- HC5T3: Checking for Uppercase Letters
hasUppercaseWord :: [String] -> Bool
hasUppercaseWord = any (\word -> not (null word) && isUpper (head word))

-- HC5T4: Using Lambda Functions
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

-- HC5T5: Partial Application
multiplyByFive :: Num a => a -> a
multiplyByFive = (*5)

-- HC5T6: Function Composition
evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)

-- HC5T7: Using the $ Operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- HC5T8: Point-Free Style
addFive :: Num a => a -> a
addFive = (+5)

-- HC5T9: Higher-Order Function to Transform a List
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

-- HC5T10: Combining Higher-Order Functions
anySquareGreater50 :: [Int] -> Bool
anySquareGreater50 = any (>50) . map (^2) . filter (>0)

-- MAIN FUNCTION TO TEST ALL TASKS
main :: IO ()
main = do
  putStrLn "===== HC5T1 - applyThrice ====="
  print $ applyThrice (+2) 5        -- Expect 11

  putStrLn "\n===== HC5T2 - oddNumbers ====="
  print oddNumbers                  -- Expect [1,3,5,...,29]

  putStrLn "\n===== HC5T3 - hasUppercaseWord ====="
  print $ hasUppercaseWord ["hello","World","hi"]  -- Expect True
  print $ hasUppercaseWord ["hello","world"]      -- Expect False

  putStrLn "\n===== HC5T4 - biggerThan10 ====="
  print $ biggerThan10 5             -- Expect False
  print $ biggerThan10 15            -- Expect True

  putStrLn "\n===== HC5T5 - multiplyByFive ====="
  print $ multiplyByFive 3           -- Expect 15

  putStrLn "\n===== HC5T6 - evenSquares ====="
  print $ evenSquares [1..10]        -- Expect [4,16,36,64,100]

  putStrLn "\n===== HC5T7 - result using $ ====="
  print result                       -- Expect 72

  putStrLn "\n===== HC5T8 - addFive (point-free) ====="
  print $ addFive 10                  -- Expect 15

  putStrLn "\n===== HC5T9 - transformList ====="
  print $ transformList (+1) [1,2,3] -- Expect [3,4,5]

  putStrLn "\n===== HC5T10 - anySquareGreater50 ====="
  print $ anySquareGreater50 [1..7]  -- Expect True
  print $ anySquareGreater50 [1..5]  -- Expect False
