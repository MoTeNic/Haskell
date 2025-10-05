-- ==========================================
-- HC4_Assignment.hs
-- HC4T1 – HC4T8
-- ==========================================

-- HC4T1 - Task 1: Define a weatherReport Function
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

-- HC4T2 - Task 2: Define a dayType Function
dayType :: String -> String
dayType "Saturday"  = "It's a weekend!"
dayType "Sunday"    = "It's a weekend!"
dayType "Monday"    = "It's a weekday."
dayType "Tuesday"   = "It's a weekday."
dayType "Wednesday" = "It's a weekday."
dayType "Thursday"  = "It's a weekday."
dayType "Friday"    = "It's a weekday."
dayType _           = "Invalid day"

-- HC4T3 - Task 3: Define a gradeComment Function
gradeComment :: Int -> String
gradeComment n
  | n >= 90 && n <= 100 = "Excellent!"
  | n >= 70 && n <= 89  = "Good job!"
  | n >= 50 && n <= 69  = "You passed."
  | n >= 0 && n <= 49   = "Better luck next time."
  | otherwise            = "Invalid grade"

-- HC4T4 & HC4T5 - Rewrite specialBirthday using Pattern Matching & Catch-All
specialBirthday :: Int -> String
specialBirthday 16 = "Sweet 16! Enjoy your day!"
specialBirthday 18 = "Congrats on turning 18!"
specialBirthday 21 = "Welcome to adulthood!"
specialBirthday age = "Happy " ++ show age ++ "th birthday!"

-- HC4T6 - Identify List Contents Using Pattern Matching
whatsInsideThisList :: [a] -> String
whatsInsideThisList []        = "The list is empty."
whatsInsideThisList [_]       = "The list has one element."
whatsInsideThisList [_, _]    = "The list has two elements."
whatsInsideThisList [_, _, _] = "The list has three elements."
whatsInsideThisList _         = "The list has more than three elements."

-- HC4T7 - Ignore Elements in a List
firstAndThird :: [a] -> (Maybe a, Maybe a)
firstAndThird (x:_:z:_) = (Just x, Just z)
firstAndThird (x:_:[])  = (Just x, Nothing)
firstAndThird (x:[])    = (Just x, Nothing)
firstAndThird []        = (Nothing, Nothing)

-- HC4T8 - Extract Values from Tuples
describeTuple :: (String, Int, Bool) -> String
describeTuple (name, age, status) =
  name ++ " is " ++ show age ++ " years old and the status is " ++ show status ++ "."

-- MAIN FUNCTION TO TEST ALL TASKS
main :: IO ()
main = do
  putStrLn "===== HC4T1 - Weather Report ====="
  putStrLn $ weatherReport "sunny"
  putStrLn $ weatherReport "rainy"
  putStrLn $ weatherReport "cloudy"
  putStrLn $ weatherReport "windy"

  putStrLn "\n===== HC4T2 - Day Type ====="
  putStrLn $ dayType "Monday"
  putStrLn $ dayType "Saturday"
  putStrLn $ dayType "Friday"
  putStrLn $ dayType "Funday"

  putStrLn "\n===== HC4T3 - Grade Comment ====="
  putStrLn $ gradeComment 95
  putStrLn $ gradeComment 75
  putStrLn $ gradeComment 55
  putStrLn $ gradeComment 30
  putStrLn $ gradeComment 120

  putStrLn "\n===== HC4T4 & HC4T5 - Special Birthday ====="
  putStrLn $ specialBirthday 16
  putStrLn $ specialBirthday 18
  putStrLn $ specialBirthday 21
  putStrLn $ specialBirthday 25

  putStrLn "\n===== HC4T6 - List Contents ====="
  putStrLn $ whatsInsideThisList []
  putStrLn $ whatsInsideThisList [1]
  putStrLn $ whatsInsideThisList [1,2]
  putStrLn $ whatsInsideThisList [1,2,3]
  putStrLn $ whatsInsideThisList [1,2,3,4,5]

  putStrLn "\n===== HC4T7 - First and Third Elements ====="
  print $ firstAndThird ([1,2,3,4] :: [Int])
  print $ firstAndThird ([10,20,30] :: [Int])
  print $ firstAndThird ([5,6] :: [Int])
  print $ firstAndThird ([7] :: [Int])
  print $ firstAndThird ([] :: [Int])

  putStrLn "\n===== HC4T8 - Describe Tuple ====="
  putStrLn $ describeTuple ("Alice", 30, True)
  putStrLn $ describeTuple ("Bob", 25, False)
