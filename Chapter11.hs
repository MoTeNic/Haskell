import Data.Char (toUpper)

-- HC11T1: Greet the User
hc11t1 :: IO ()
hc11t1 = do
  let name = "Alice"
  putStrLn "=== HC11T1: Greet the User ==="
  putStrLn ("Hello, " ++ name ++ "!")

-- HC11T2: Count Characters in a Line
hc11t2 :: IO ()
hc11t2 = do
  let line = "Hello World"
  putStrLn "\n=== HC11T2: Count Characters in a Line ==="
  putStrLn ("Line: " ++ line)
  putStrLn ("Number of characters: " ++ show (length line))

-- HC11T3: Double a Number
hc11t3 :: IO ()
hc11t3 = do
  let num = 7
  putStrLn "\n=== HC11T3: Double a Number ==="
  putStrLn ("Number: " ++ show num)
  putStrLn ("Double: " ++ show (num * 2))

-- HC11T4: Concatenate Two Lines
hc11t4 :: IO ()
hc11t4 = do
  let line1 = "Hello "
      line2 = "World!"
  putStrLn "\n=== HC11T4: Concatenate Two Lines ==="
  putStrLn ("Concatenated: " ++ line1 ++ line2)

-- HC11T5: Repeat Until "quit"
hc11t5 :: IO ()
hc11t5 = do
  let inputs = ["Hi", "Test", "quit"]
  putStrLn "\n=== HC11T5: Repeat Until 'quit' ==="
  mapM_ (\line -> if line == "quit"
                  then putStrLn "Exiting..."
                  else putStrLn ("You typed: " ++ line)) inputs

-- HC11T6: Uppercase Converter
hc11t6 :: IO ()
hc11t6 = do
  let text = "hello haskell"
  putStrLn "\n=== HC11T6: Uppercase Converter ==="
  putStrLn ("Original: " ++ text)
  putStrLn ("Uppercase: " ++ map toUpper text)

-- HC11T7: User Options
hc11t7 :: IO ()
hc11t7 = do
  let option = "1"  -- Hardcoded option
  putStrLn "\n=== HC11T7: User Options ==="
  case option of
    "1" -> putStrLn "Hello, user!"
    "2" -> putStrLn "Goodbye!"
    _   -> putStrLn "Invalid choice"

-- HC11T8: Even or Odd Checker
hc11t8 :: IO ()
hc11t8 = do
  let num = 8
  putStrLn "\n=== HC11T8: Even or Odd Checker ==="
  putStrLn ("Number: " ++ show num)
  putStrLn (if even num then "Even" else "Odd")

-- HC11T9: Sum Two Numbers
hc11t9 :: IO ()
hc11t9 = do
  let n1 = 5
      n2 = 12
  putStrLn "\n=== HC11T9: Sum Two Numbers ==="
  putStrLn ("Numbers: " ++ show n1 ++ " and " ++ show n2)
  putStrLn ("Sum: " ++ show (n1 + n2))

-- HC11T10: Reverse User Input
hc11t10 :: IO ()
hc11t10 = do
  let text = "Haskell"
  putStrLn "\n=== HC11T10: Reverse User Input ==="
  putStrLn ("Original: " ++ text)
  putStrLn ("Reversed: " ++ reverse text)

-- MAIN: Run all tasks sequentially
main :: IO ()
main = do
  hc11t1
  hc11t2
  hc11t3
  hc11t4
  hc11t5
  hc11t6
  hc11t7
  hc11t8
  hc11t9
  hc11t10
