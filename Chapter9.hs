-- ===========================================
-- CHAPTER 9 PRACTICAL TASKS (HC9T1 - HC9T10)
-- ===========================================

-- HC9T1: Define a Parametric Type Synonym
type Entity a = (String, a)

-- HC9T2: Implement a Parametric Data Type
data Box a = Empty | Has a deriving (Show)

-- HC9T3: Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN n Empty = Empty
addN n (Has x) = Has (x + n)

-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract def Empty = def
extract _ (Has x) = x

-- HC9T5: Parametric Data Type with Record Syntax
data Shape a
  = Circle { color :: a, radius :: Float }
  | Rectangle { color :: a, width :: Float, height :: Float }
  deriving (Show)

-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet
  { content :: String
  , likes :: Int
  , comments :: [Tweet]
  } deriving (Show)

-- HC9T7: Engagement Function for Tweets
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)

-- HC9T8: Recursive Sequence Data Type
data Sequence a = End | Node a (Sequence a) deriving (Show)

-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node y rest)
  | x == y    = True
  | otherwise = elemSeq x rest

-- HC9T10: Binary Search Tree Data Type
data BST a = Leaf | Branch a (BST a) (BST a) deriving (Show)

-- ===========================================
-- MAIN FUNCTION (Runs All Tasks Sequentially)
-- ===========================================

main :: IO ()
main = do
  putStrLn "=== HC9T1: Parametric Type Synonym ==="
  let person :: Entity String
      person = ("Address", "123 Main Street")
      company :: Entity Int
      company = ("Company ID", 42)
  print person
  print company

  putStrLn "\n=== HC9T2: Parametric Data Type ==="
  print (Empty :: Box Int)
  print (Has 42)

  putStrLn "\n=== HC9T3: Add Values in a Box ==="
  print (addN 5 (Has 10))
  print (addN 3 Empty)

  putStrLn "\n=== HC9T4: Extract a Value from a Box ==="
  print (extract 0 (Has 10))
  print (extract 0 Empty)

  putStrLn "\n=== HC9T5: Parametric Data Type with Record Syntax ==="
  let c = Circle { color = "Red", radius = 5.0 }
      r = Rectangle { color = "Blue", width = 10, height = 5 }
  print c
  print r

  putStrLn "\n=== HC9T6: Recursive Data Type for Tweets ==="
  let comment1 = Tweet "Nice post!" 2 []
      comment2 = Tweet "I agree!" 3 []
      mainTweet = Tweet "Hello world" 10 [comment1, comment2]
  print mainTweet

  putStrLn "\n=== HC9T7: Engagement Function for Tweets ==="
  putStrLn ("Engagement: " ++ show (engagement mainTweet))

  putStrLn "\n=== HC9T8: Recursive Sequence Data Type ==="
  let seq1 = Node 1 (Node 2 (Node 3 End))
  print seq1

  putStrLn "\n=== HC9T9: Check for Element in a Sequence ==="
  print (elemSeq 2 seq1)
  print (elemSeq 5 seq1)

  putStrLn "\n=== HC9T10: Binary Search Tree Data Type ==="
  let tree = Branch 10 (Branch 5 Leaf Leaf) (Branch 15 Leaf Leaf)
  print tree
