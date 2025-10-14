-- Haskell Chapter 10 Practical Tasks: Custom Type Classes and Instances

-- HC10T1: ShowSimple Type Class
class ShowSimple a where
  showSimple :: a -> String

data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show, Eq)

instance ShowSimple PaymentMethod where
  showSimple Cash = "Cash Payment"
  showSimple Card = "Card Payment"
  showSimple Cryptocurrency = "Crypto Payment"


-- HC10T2: Summable Type Class
class Summable a where
  sumUp :: [a] -> a

instance Summable Int where
  sumUp = sum


-- HC10T3: Comparable Type Class
data Blockchain = Bitcoin | Ethereum | Solana deriving (Show, Ord)

class Comparable a where
  compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
  compareWith = compare


-- HC10T4: Eq Instance for Box
data Box a = Empty | Has a deriving (Show)

instance Eq a => Eq (Box a) where
  Empty == Empty = True
  Has x == Has y = x == y
  _ == _         = False


-- HC10T5: ShowDetailed Type Class
data User = User { username :: String, email :: String } deriving (Show)

class ShowDetailed a where
  showDetailed :: a -> String

instance ShowDetailed User where
  showDetailed (User u e) = "User: " ++ u ++ " (" ++ e ++ ")"


-- HC10T6: Eq with Mutual Recursion for Blockchain
instance Eq Blockchain where
  Bitcoin == Bitcoin   = True
  Ethereum == Ethereum = True
  Solana == Solana     = True
  _ == _               = False
  a /= b               = not (a == b)


-- HC10T7: Convertible Type Class
class Convertible a b where
  convert :: a -> b

instance Convertible PaymentMethod String where
  convert Cash = "Cash"
  convert Card = "Card"
  convert Cryptocurrency = "Crypto"


-- HC10T8: AdvancedEq Subclass of Eq
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool
  compareEquality x y = x == y

instance AdvancedEq Int
instance AdvancedEq Bool


-- HC10T9: MinMax Type Class
class MinMax a where
  minValue :: a
  maxValue :: a

instance MinMax Int where
  minValue = minBound
  maxValue = maxBound


-- HC10T10: Concatenatable Type Class
class Concatenatable a where
  concatWith :: a -> a -> a

instance Concatenatable String where
  concatWith = (++)


-- MAIN FUNCTION
main :: IO ()
main = do
  putStrLn "=== HC10T1: ShowSimple Type Class ==="
  putStrLn (showSimple Cash)
  putStrLn (showSimple Cryptocurrency)

  putStrLn "\n=== HC10T2: Summable Type Class ==="
  print (sumUp [1, 2, 3, 4 :: Int])

  putStrLn "\n=== HC10T3: Comparable Type Class ==="
  print (compareWith Bitcoin Ethereum)

  putStrLn "\n=== HC10T4: Eq Instance for Box ==="
  print (Has 3 == Has 3)
  print (Has 3 == Has 4)
  print (Empty == (Has 3 :: Box Int))
  print ((Empty :: Box Int) == Empty)

  putStrLn "\n=== HC10T5: ShowDetailed Type Class ==="
  print (showDetailed (User "Alice" "alice@example.com"))

  putStrLn "\n=== HC10T6: Eq for Blockchain with Mutual Recursion ==="
  print (Bitcoin == Ethereum)
  print (Bitcoin /= Ethereum)
  print (Ethereum == Ethereum)

  putStrLn "\n=== HC10T7: Convertible Type Class ==="
  putStrLn (convert Card)
  putStrLn (convert Cryptocurrency)

  putStrLn "\n=== HC10T8: AdvancedEq Subclass ==="
  print (compareEquality (5 :: Int) 5)
  print (compareEquality True False)

  putStrLn "\n=== HC10T9: MinMax Type Class ==="
  print (minValue :: Int)
  print (maxValue :: Int)

  putStrLn "\n=== HC10T10: Concatenatable Type Class ==="
  putStrLn (concatWith "Hello, " "World!")
