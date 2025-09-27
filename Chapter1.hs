import Data.List (sortBy)

-- HC1T1 - Task 1: Function Composition

double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

-- HC1T2 - Task 2: Pure Function Example

circleArea :: Floating a => a -> a
circleArea r = pi * r * r

-- HC1T3 - Task 3: Checking if a Number is Greater than 18

greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

-- HC1T4 - Task 4: Composing a Function to Process Player Data

type Player = (String, Int)

extractPlayers :: [Player] -> [String]
extractPlayers players = map fst players

sortByScore :: [Player] -> [Player]
sortByScore players = reverse (sortBy (\(_, s1) (_, s2) -> compare s1 s2) players)

topThree :: [Player] -> [Player]
topThree = take 3

getTopThreePlayers :: [Player] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- HC1T5 - Task 5: Laziness in Haskell

infiniteNumbers :: [Integer]
infiniteNumbers = [1..]

firstN :: Int -> [Integer]
firstN n = take n infiniteNumbers

-- HC1T6 - Task 6: Using Type Signatures

addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- HC1T7 - Task 7: Converting Fahrenheit to Celsius

fToC :: Fractional a => a -> a
fToC f = (f - 32) * (5 / 9)

-- HC1T8 - Task 8: Higher-Order Functions

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Main Function to Demonstrate All Tasks

main :: IO ()
main = do
    putStrLn "== HC1T1 - Function Composition =="
    putStrLn $ "doubleThenIncrement 5 => " ++ show (doubleThenIncrement 5)

    putStrLn "\n== HC1T2 - Pure Function: circleArea =="
    putStrLn $ "circleArea 3 => " ++ show (circleArea 3)

    putStrLn "\n== HC1T3 - Greater Than 18 =="
    putStrLn $ "greaterThan18 20 => " ++ show (greaterThan18 20)
    putStrLn $ "greaterThan18 10 => " ++ show (greaterThan18 10)

    putStrLn "\n== HC1T4 - Top 3 Players =="
    let players = [("Alice", 50), ("Bob", 75), ("Charlie", 65), ("Dave", 80), ("Eve", 60)]
    putStrLn $ "getTopThreePlayers => " ++ show (getTopThreePlayers players)

    putStrLn "\n== HC1T5 - Infinite List Laziness =="
    putStrLn $ "firstN 5 => " ++ show (firstN 5)

    putStrLn "\n== HC1T6 - Add Numbers =="
    putStrLn $ "addNumbers 3 7 => " ++ show (addNumbers 3 7)

    putStrLn "\n== HC1T7 - Fahrenheit to Celsius =="
    putStrLn $ "fToC 100 => " ++ show (fToC 100)

    putStrLn "\n== HC1T8 - applyTwice =="
    putStrLn $ "applyTwice (+1) 5 => " ++ show (applyTwice (+1) 5)
    putStrLn $ "applyTwice (*2) 3 => " ++ show (applyTwice (*2) 3)
