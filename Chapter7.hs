-- Haskell Chapter 7 Practical Tasks: Type Classes and Constraints
-- All 10 tasks combined in a single file

-------------------------------
-- HC7T1: Eq Instance for Color
-------------------------------
data Color = Red | Green | Blue deriving (Show, Eq, Ord, Enum, Bounded)

runHC7T1 :: IO ()
runHC7T1 = do
  putStrLn ("HC7T1: Red == Green ? " ++ show (Red == Green))
  putStrLn ("HC7T1: Blue == Blue ? " ++ show (Blue == Blue))

-------------------------------
-- HC7T2: Ord Instance for Color
-------------------------------
runHC7T2 :: IO ()
runHC7T2 = do
  putStrLn ("HC7T2: Red < Green ? " ++ show (Red < Green))
  putStrLn ("HC7T2: Blue > Green ? " ++ show (Blue > Green))

-------------------------------
-- HC7T3: Function Using Multiple Constraints
-------------------------------
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues a b = if a >= b then a else b

runHC7T3 :: IO ()
runHC7T3 = do
  putStrLn ("HC7T3: max of 5 and 10 = " ++ show (compareValues 5 10))
  putStrLn ("HC7T3: max of Red and Blue = " ++ show (compareValues Red Blue))

-------------------------------
-- HC7T4: Custom Type with Show and Read
-------------------------------
data Shape = Circle Double | Rectangle Double Double deriving (Show, Read)

runHC7T4 :: IO ()
runHC7T4 = do
  let c = Circle 3.0
      r = Rectangle 2.0 5.0
  putStrLn ("HC7T4: Show Circle = " ++ show c)
  putStrLn ("HC7T4: Show Rectangle = " ++ show r)
  putStrLn ("HC7T4: Read Circle = " ++ show (read "Circle 3.0" :: Shape))

-------------------------------
-- HC7T5: Function with Num Constraint
-------------------------------
squareArea :: Num a => a -> a
squareArea side = side * side

runHC7T5 :: IO ()
runHC7T5 = putStrLn ("HC7T5: Area of square with side 4 = " ++ show (squareArea 4))

-------------------------------
-- HC7T6: Using Integral and Floating Type Classes
-------------------------------
circleCircumference :: (Integral a, Floating b) => a -> b
circleCircumference r = 2 * pi * fromIntegral r

runHC7T6 :: IO ()
runHC7T6 = putStrLn ("HC7T6: Circumference of circle radius 5 = " ++ show (circleCircumference 5 :: Double))

-------------------------------
-- HC7T7: Bounded and Enum
-------------------------------
nextColor :: Color -> Color
nextColor c = if c == maxBound then minBound else succ c

runHC7T7 :: IO ()
runHC7T7 = do
  putStrLn ("HC7T7: Next after Red = " ++ show (nextColor Red))
  putStrLn ("HC7T7: Next after Blue = " ++ show (nextColor Blue))

-------------------------------
-- HC7T8: Parse a Value from a String Using Read
-------------------------------
parseShape :: String -> Maybe Shape
parseShape str = case reads str of
  [(s, "")] -> Just s
  _         -> Nothing

runHC7T8 :: IO ()
runHC7T8 = do
  putStrLn ("HC7T8: Parse \"Circle 3.0\" = " ++ show (parseShape "Circle 3.0"))
  putStrLn ("HC7T8: Parse \"Invalid\" = " ++ show (parseShape "Invalid"))

-------------------------------
-- HC7T9: Type Class with Multiple Instances
-------------------------------
class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True = "This is True"
  describe False = "This is False"

instance Describable Shape where
  describe (Circle r) = "Circle with radius " ++ show r
  describe (Rectangle w h) = "Rectangle " ++ show w ++ "x" ++ show h

runHC7T9 :: IO ()
runHC7T9 = do
  putStrLn ("HC7T9: Describe True = " ++ describe True)
  putStrLn ("HC7T9: Describe Rectangle = " ++ describe (Rectangle 2 5))

-------------------------------
-- HC7T10: Function with Multiple Type Class Constraints
-------------------------------
-- Make Shape comparable for area
instance Eq Shape where
  Circle r1 == Circle r2 = r1 == r2
  Rectangle w1 h1 == Rectangle w2 h2 = w1 * h1 == w2 * h2
  _ == _ = False

instance Ord Shape where
  compare (Circle r1) (Circle r2) = compare r1 r2
  compare (Rectangle w1 h1) (Rectangle w2 h2) = compare (w1*h1) (w2*h2)
  compare (Circle _) (Rectangle _ _) = LT
  compare (Rectangle _ _) (Circle _) = GT

describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y = describe (max x y)

runHC7T10 :: IO ()
runHC7T10 = do
  let s1 = Circle 3
      s2 = Rectangle 2 5
  putStrLn ("HC7T10: Describe larger shape = " ++ describeAndCompare s1 s2)

-------------------------------

-------------------------------
main :: IO ()
main = do
  runHC7T1
  runHC7T2
  runHC7T3
  runHC7T4
  runHC7T5
  runHC7T6
  runHC7T7
  runHC7T8
  runHC7T9
  runHC7T10
