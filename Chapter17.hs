{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid
import Data.Semigroup hiding (Min, Max, Product) -- hide conflicting names
import GHC.Generics (Generic)

-- HC17T1: Severity Data Type and Semigroup Instance
data Severity = Low | Medium | High | Critical
  deriving (Show, Eq, Ord, Generic)

instance Semigroup Severity where
  s1 <> s2 = max s1 s2

-- HC17T2: MyMin and MyMax Newtypes with Semigroup
newtype MyMin a = MyMin { getMyMin :: a } deriving (Show, Eq, Ord)
newtype MyMax a = MyMax { getMyMax :: a } deriving (Show, Eq, Ord)

instance Ord a => Semigroup (MyMin a) where
  MyMin a <> MyMin b = MyMin (min a b)

instance Ord a => Semigroup (MyMax a) where
  MyMax a <> MyMax b = MyMax (max a b)

-- HC17T3: Monoid Instance for Severity
instance Monoid Severity where
  mempty = Low
  mappend = (<>)

-- Removed duplicate Monoid instance for Sum (already provided by Prelude)

-- HC17T5: combineLists Function
combineLists :: [Int] -> [Int] -> [Int]
combineLists = (<>)

-- HC17T6: maxSeverity Function
maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

-- HC17T7: MyProduct type and multiplyProducts Function
newtype MyProduct a = MyProduct { getMyProduct :: a } deriving (Show, Eq, Ord, Num)

instance Num a => Semigroup (MyProduct a) where
  MyProduct a <> MyProduct b = MyProduct (a * b)

instance Num a => Monoid (MyProduct a) where
  mempty = MyProduct 1
  mappend = (<>)

multiplyProducts :: Num a => [MyProduct a] -> MyProduct a
multiplyProducts = mconcat

-- HC17T8: foldWithSemigroup Function
foldWithSemigroup :: Semigroup a => [a] -> a
foldWithSemigroup = foldr1 (<>)

-- HC17T9: Config Data Type and Semigroup Instance
data Config = Config
  { loggingLevel :: Severity
  , timeout      :: Int
  , retries      :: Int
  } deriving (Show, Eq)

instance Semigroup Config where
  c1 <> c2 = Config
    { loggingLevel = max (loggingLevel c1) (loggingLevel c2)
    , timeout      = min (timeout c1) (timeout c2)
    , retries      = max (retries c1) (retries c2)
    }

-- HC17T10: Monoid Instance for Config
instance Monoid Config where
  mempty = Config { loggingLevel = Low, timeout = maxBound, retries = 0 }
  mappend = (<>)

-- Main function demonstrating all tasks
main :: IO ()
main = do
  putStrLn "=== HC17 Demonstrations ==="

  -- Severity examples
  putStrLn "\nTask 1 & 3: Severity Semigroup + Monoid"
  print $ Low <> High
  print $ mconcat [Low, Medium, Critical]

  -- MyMin and MyMax examples
  putStrLn "\nTask 2: MyMin and MyMax"
  print $ MyMin 3 <> MyMin 7
  print $ MyMax 3 <> MyMax 7

  -- combineLists
  putStrLn "\nTask 5: combineLists"
  print $ combineLists [1,2,3] [4,5]

  -- multiplyProducts
  putStrLn "\nTask 7: multiplyProducts"
  print $ multiplyProducts [MyProduct 2, MyProduct 3, MyProduct 4]

  -- foldWithSemigroup
  putStrLn "\nTask 8: foldWithSemigroup"
  print $ foldWithSemigroup [Low, High, Medium]

  -- Config combination
  putStrLn "\nTask 9 & 10: Config Semigroup + Monoid"
  let cfg1 = Config High 100 2
      cfg2 = Config Medium 50 5
  print $ cfg1 <> cfg2
  print $ mempty <> cfg1
