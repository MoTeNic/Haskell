{-# LANGUAGE FlexibleContexts #-}

-- Imports
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity
import Data.List

-- ============================================================
-- HC20T1: safeDivide with Maybe Monad
-- ============================================================
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- ============================================================
-- HC20T2: sequenceMaybe for List of Maybe
-- ============================================================
sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = sequence

-- ============================================================
-- HC20T3: Writer Monad Logging Calculator
-- ============================================================
add :: Double -> Double -> Writer [String] Double
add x y = writer (x + y, ["Added " ++ show x ++ " and " ++ show y])

mul :: Double -> Double -> Writer [String] Double
mul x y = writer (x * y, ["Multiplied " ++ show x ++ " and " ++ show y])

-- ============================================================
-- HC20T4: countChars with State Monad
-- ============================================================
countChars :: Char -> State String Int
countChars c = do
  s <- get
  return $ length $ filter (== c) s

-- ============================================================
-- HC20T5: Reader Monad for Configurable Greeting
-- ============================================================
type Config = String
greet :: Reader Config String
greet = do
  name <- ask
  return $ "Hello, " ++ name ++ "!"

-- ============================================================
-- HC20T6: doubleMonad Combining Maybe and List
-- ============================================================
doubleMonad :: Maybe a -> [a]
doubleMonad Nothing  = []
doubleMonad (Just x) = [x]

-- ============================================================
-- HC20T7: findFirst with Either Monad
-- ============================================================
findFirst :: (a -> Bool) -> [a] -> Either String a
findFirst _ [] = Left "No element found"
findFirst p xs = maybe (Left "No match") Right (find p xs)

-- ============================================================
-- HC20T8: Parser Monad for Simple Expressions
-- ============================================================
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f p = Parser $ \s -> do
    (x, rest) <- runParser p s
    return (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  pf <*> px = Parser $ \s -> do
    (f, s1) <- runParser pf s
    (x, s2) <- runParser px s1
    return (f x, s2)

instance Monad Parser where
  p >>= f = Parser $ \s -> do
    (x, s1) <- runParser p s
    runParser (f x) s1

charP :: Char -> Parser Char
charP c = Parser f
  where f (x:xs) | x == c    = Just (x, xs)
                  | otherwise = Nothing
        f [] = Nothing

-- ============================================================
-- HC20T9: replicateMonad with Identity Monad
-- ============================================================
replicateMonad :: Int -> Identity a -> Identity [a]
replicateMonad n x = replicateM n x

-- ============================================================
-- HC20T10: Nested StateT and MaybeT Transformer
-- ============================================================
type StateMaybe s a = StateT s Maybe a

incrementState :: StateMaybe Int Int
incrementState = do
  x <- get
  put (x + 1)
  return x

-- ============================================================
-- HC20T11
-- ============================================================
randomWalk :: State [Int] (Int, Int)
randomWalk = do
  xs <- get
  case xs of
    (dx:dy:rest) -> do
      put rest
      return (dx, dy)
    _ -> return (0, 0)

-- ============================================================
-- HC20T12
-- ============================================================
readFileLines :: FilePath -> IO ()
readFileLines _ = putStrLn "HC20T12 demo skipped (requires a real file)."

-- ============================================================
-- HC20T13
-- ============================================================
type FibState = State ([(Int, Int)]) Int

fibonacciMemo :: Int -> FibState
fibonacciMemo n = do
  memo <- get
  case lookup n memo of
    Just val -> return val
    Nothing -> do
      a <- if n <= 1 then return n else fibonacciMemo (n-1)
      b <- if n <= 1 then return 0 else fibonacciMemo (n-2)
      let val = a + b
      modify ((n,val):)
      return val

-- ============================================================
-- HC20T14
-- ============================================================
mapMFilter :: Monad m => (a -> m Bool) -> [a] -> m [a]
mapMFilter f xs = filterM f xs

-- ============================================================
-- HC20T15
-- ============================================================
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

treeSum :: Num a => Tree a -> a
treeSum Leaf = 0
treeSum (Node x l r) = x + treeSum l + treeSum r

-- ============================================================
-- HC20T16
-- ============================================================
retryIO :: Int -> IO Bool -> IO Bool
retryIO 0 action = action
retryIO n action = do
  res <- action
  if res then return True else retryIO (n-1) action

-- ============================================================
-- HC20T17
-- ============================================================
validatePassword :: String -> Either String String
validatePassword pwd
  | length pwd < 6 = Left "Too short"
  | not (any (`elem` ['0'..'9']) pwd) = Left "No digit"
  | otherwise = Right pwd

-- ============================================================
-- HC20T18
-- ============================================================
getMaybeInput :: MaybeT IO String
getMaybeInput = do
  lift $ putStrLn "Enter something:"
  input <- lift getLine
  guard (not $ null input)
  return input

-- ============================================================
-- HC20T19
-- ============================================================
logFunc :: Int -> Int -> Writer [String] Int
logFunc x y = writer (x + y, ["Added " ++ show x ++ " and " ++ show y])

-- ============================================================
-- HC20T20
-- ============================================================
batchProcessing :: IO ()
batchProcessing = do
  putStrLn "Processing 1"
  putStrLn "Processing 2"
  putStrLn "Processing 3"

-- ============================================================
-- MAIN
-- ============================================================
main :: IO ()
main = do
  putStrLn "--- HC20T1: safeDivide ---"
  print $ safeDivide 10 2
  print $ safeDivide 10 0

  putStrLn "\n--- HC20T2: sequenceMaybe ---"
  print $ sequenceMaybe [Just 1, Just 2, Just 3]
  print $ sequenceMaybe [Just 1, Nothing, Just 3]

  putStrLn "\n--- HC20T3: Writer Monad Calculator ---"
  let (res3, log3) = runWriter $ add 2 3 >>= \x -> mul x 4
  print res3
  mapM_ putStrLn log3

  putStrLn "\n--- HC20T4: countChars ---"
  print $ evalState (countChars 'a') "banana"

  putStrLn "\n--- HC20T5: Reader Monad Greeting ---"
  print $ runReader greet "Alice"

  putStrLn "\n--- HC20T6: doubleMonad ---"
  print $ doubleMonad (Just 42)
  print $ (doubleMonad Nothing :: [Int])

  putStrLn "\n--- HC20T7: findFirst ---"
  print $ findFirst even [1,3,4,5]
  print $ findFirst (>10) [1,3,4]

  putStrLn "\n--- HC20T8: Parser Monad ---"
  print $ runParser (charP 'a') "abc"
  print $ runParser (charP 'b') "abc"

  putStrLn "\n--- HC20T9: replicateMonad ---"
  print $ runIdentity $ replicateMonad 3 (Identity "Hi")

  putStrLn "\n--- HC20T10: Nested StateT + MaybeT ---"
  print $ runStateT incrementState 5

  putStrLn "\n--- HC20T11: randomWalk ---"
  let steps = [1,-1,0,1,0,-1]
  print $ runState randomWalk steps

  putStrLn "\n--- HC20T12: readFileLines ---"
  readFileLines "example.txt"

  putStrLn "\n--- HC20T13: fibonacciMemo ---"
  print $ evalState (fibonacciMemo 10) []

  putStrLn "\n--- HC20T14: mapMFilter ---"
  res14 <- mapMFilter (\x -> return (x `mod` 2 == 0)) [1..5]
  print res14

  putStrLn "\n--- HC20T15: treeSum ---"
  let t = Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)
  print $ treeSum t

  putStrLn "\n--- HC20T16: retryIO ---"
  let action = do { putStrLn "Trying..."; return False }
  _ <- retryIO 3 action
  putStrLn "Finished retrying."

  putStrLn "\n--- HC20T17: validatePassword ---"
  print $ validatePassword "abc"
  print $ validatePassword "abc123"

  putStrLn "\n--- HC20T18: MaybeT user input ---"
 
  putStrLn "HC20T18 skipped (requires user input)."

  putStrLn "\n--- HC20T19: Writer logging ---"
  let (val19, logs19) = runWriter $ logFunc 5 7
  print val19
  mapM_ putStrLn logs19

  putStrLn "\n--- HC20T20: batchProcessing ---"
  batchProcessing

 
