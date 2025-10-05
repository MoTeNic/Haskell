-- ==========================================
-- HC3_Assignment.hs
-- HC3T1 – HC3T10
-- ==========================================

import Text.Printf (printf)

-- HC3T1 - Check if a number is positive, negative, or zero
checkNumber :: Int -> String
checkNumber x =
  if x > 0 then "Positive"
  else if x < 0 then "Negative"
  else "Zero"

-- HC3T2 - Determine the grade based on a score using guards
grade :: Int -> String
grade score
  | score >= 90 = "A"
  | score >= 80 = "B"
  | score >= 70 = "C"
  | score >= 60 = "D"
  | otherwise   = "F"

-- HC3T3 - Convert RGB color to hex string using let bindings
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
  let
    hexR = printf "%02X" r
    hexG = printf "%02X" g
    hexB = printf "%02X" b
  in "#" ++ hexR ++ hexG ++ hexB

-- HC3T4 - Calculate the area of a triangle using Heron's formula
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
  let s = (a + b + c) / 2
  in sqrt (s * (s - a) * (s - b) * (s - c))

-- HC3T5 - Determine the type of a triangle using guards
triangleType :: Float -> Float -> Float -> String
triangleType a b c
  | a == b && b == c = "Equilateral"
  | a == b || b == c || a == c = "Isosceles"
  | otherwise = "Scalene"

-- HC3T6 - Check leap year using if-then-else
isLeapYear :: Int -> Bool
isLeapYear year =
  if year `mod` 400 == 0 then True
  else if year `mod` 100 == 0 then False
  else if year `mod` 4 == 0 then True
  else False

-- HC3T7 - Determine the season based on month using guards
season :: Int -> String
season month
  | month == 12 || month == 1 || month == 2 = "Winter"
  | month >= 3 && month <= 5 = "Spring"
  | month >= 6 && month <= 8 = "Summer"
  | month >= 9 && month <= 11 = "Autumn"
  | otherwise = "Invalid month"

-- HC3T8 - Calculate BMI and return category using where
bmiCategory :: Float -> Float -> String
bmiCategory weight height
  | bmi < 18.5 = "Underweight"
  | bmi < 25.0 = "Normal"
  | bmi < 30.0 = "Overweight"
  | otherwise  = "Obese"
  where
    bmi = weight / (height ^ 2)

-- HC3T9 - Find the maximum of three numbers using let
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c =
  let maxAB = max a b
      maxAll = max maxAB c
  in maxAll

-- HC3T10 - Check if a string is a palindrome using recursion and guards
isPalindrome :: String -> Bool
isPalindrome str
  | length str <= 1 = True
  | head str == last str = isPalindrome (init (tail str))
  | otherwise = False

-- MAIN FUNCTION TO TEST ALL TASKS
main :: IO ()
main = do
  putStrLn "===== HC3T1 - Check Number ====="
  print (checkNumber 5)
  print (checkNumber (-3))
  print (checkNumber 0)

  putStrLn "\n===== HC3T2 - Determine Grade ====="
  print (grade 95)
  print (grade 72)
  print (grade 50)

  putStrLn "\n===== HC3T3 - RGB to Hex ====="
  print (rgbToHex (255, 0, 127))
  print (rgbToHex (0, 255, 64))

  putStrLn "\n===== HC3T4 - Triangle Area ====="
  print (triangleArea 3 4 5)
  print (triangleArea 7 8 9)

  putStrLn "\n===== HC3T5 - Triangle Type ====="
  print (triangleType 3 3 3)
  print (triangleType 5 5 8)
  print (triangleType 6 7 8)

  putStrLn "\n===== HC3T6 - Leap Year Check ====="
  print (isLeapYear 2000)
  print (isLeapYear 1900)
  print (isLeapYear 2024)

  putStrLn "\n===== HC3T7 - Season by Month ====="
  print (season 3)
  print (season 7)
  print (season 11)

  putStrLn "\n===== HC3T8 - BMI Category ====="
  print (bmiCategory 70 1.75)
  print (bmiCategory 90 1.8)

  putStrLn "\n===== HC3T9 - Max of Three ====="
  print (maxOfThree 10 20 15)
  print (maxOfThree 5 25 10)

  putStrLn "\n===== HC3T10 - Palindrome Check ====="
  print (isPalindrome "racecar")
  print (isPalindrome "haskell")
  print (isPalindrome "madam")
