{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative
import Control.Monad

-- ============================================================
-- HC19T1
-- ============================================================
data Pair a = Pair a a deriving (Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

-- ============================================================
-- HC19T2
-- ============================================================
addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative a b c = (+) <$> a <*> ((+) <$> b <*> c)

-- ============================================================
-- HC19T3
-- ============================================================
safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = fmap product . sequenceA

-- ============================================================
-- HC19T4
-- ============================================================
liftAndMultiply :: Maybe Int -> Maybe Int -> Maybe Int
liftAndMultiply = liftA2 (*)

-- ============================================================
-- HC19T5
-- ============================================================
applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (a, b) = (+) <$> a <*> b

-- ============================================================
-- HC19T6
-- ============================================================
repeatEffect :: IO () -> IO ()
repeatEffect action = forever action

-- ============================================================
-- HC19T7
-- ============================================================
conditionalPrint :: Bool -> IO ()
conditionalPrint cond = when cond (putStrLn "Condition is True!")

-- ============================================================
-- HC19T8
-- ============================================================
discardSecond :: Applicative f => f a -> f b -> f a
discardSecond = (<*)

-- ============================================================
-- HC19T9
-- ============================================================
pureAndApply :: Maybe Int
pureAndApply = pure (+3) <*> Just 7

-- ============================================================
-- HC19T10
-- ============================================================
combineResults :: Either String Int -> Either String Int -> Either String Int
combineResults = liftA2 (+)

-- ============================================================
-- HC19T11
-- ============================================================
data Wrapper a = Wrapper a deriving (Show)

instance Functor Wrapper where
  fmap f (Wrapper x) = Wrapper (f x)

instance Applicative Wrapper where
  pure = Wrapper
  (Wrapper f) <*> (Wrapper x) = Wrapper (f x)

-- ============================================================
-- HC19T12
-- ============================================================
sumThreeApplicative :: Either String Int -> Either String Int -> Either String Int -> Either String Int
sumThreeApplicative a b c = (+) <$> a <*> ((+) <$> b <*> c)

-- ============================================================
-- HC19T13
-- ============================================================
whenApplicative :: Bool -> IO ()
whenApplicative cond = when cond (putStrLn "Action executed!")

-- ============================================================
-- HC19T14
-- ============================================================
replicateEffect :: Int -> IO () -> IO [()]
replicateEffect n action = replicateM n action

-- ============================================================
-- HC19T15
-- ============================================================
sequenceEffects :: [IO a] -> IO [a]
sequenceEffects = sequenceA

-- ============================================================
-- HC19T16
-- ============================================================
applyWithEffects :: IO (Int -> Int) -> IO Int -> IO Int
applyWithEffects f x = f <*> x

-- ============================================================
-- HC19T17
-- ============================================================
simulateMaybeEffect :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
simulateMaybeEffect = liftA3 (\x y z -> x + y + z)
  where liftA3 f a b c = f <$> a <*> b <*> c

-- ============================================================
-- HC19T18
-- ============================================================
combineEitherResults :: Either String Int -> Either String Int -> Either String Int -> Either String Int
combineEitherResults = liftA3 (\x y z -> x + y + z)
  where liftA3 f a b c = f <$> a <*> b <*> c

-- ============================================================
-- HC19T19
-- ============================================================
sequenceApplicative :: [Maybe a] -> Maybe [a]
sequenceApplicative = sequenceA

-- ============================================================
-- HC19T20
-- ============================================================
replicateForever :: IO () -> IO ()
replicateForever action = forever action

-- ============================================================
-- MAIN
-- ============================================================
main :: IO ()
main = do
  putStrLn "=== HC19T1: Applicative Instance for Pair ==="
  let p1 = Pair (+1) (*2)
      p2 = Pair 10 20
  print (p1 <*> p2)

  putStrLn "\n=== HC19T2: addThreeApplicative ==="
  print (addThreeApplicative (Just 2) (Just 3) (Just 5))
  print (addThreeApplicative (Just 2) Nothing (Just 5))

  putStrLn "\n=== HC19T3: safeProduct ==="
  print (safeProduct [Just 2, Just 3, Just 4])
  print (safeProduct [Just 2, Nothing, Just 4])

  putStrLn "\n=== HC19T4: liftAndMultiply ==="
  print (liftAndMultiply (Just 3) (Just 4))
  print (liftAndMultiply Nothing (Just 4))

  putStrLn "\n=== HC19T5: applyEffects ==="
  result5 <- applyEffects (return 5, return 10)
  print ("Sum is " ++ show result5)

  putStrLn "\n=== HC19T6: repeatEffect (SKIPPED to prevent infinite loop) ==="
  

  putStrLn "\n=== HC19T7: conditionalPrint ==="
  conditionalPrint True
  conditionalPrint False

  putStrLn "\n=== HC19T8: discardSecond ==="
  _ <- discardSecond (putStrLn "First") (putStrLn "Second")
  return ()

  putStrLn "\n=== HC19T9: pureAndApply ==="
  print pureAndApply

  putStrLn "\n=== HC19T10: combineResults ==="
  print (combineResults (Right 3) (Right 4))
  print (combineResults (Left "Error") (Right 4))

  putStrLn "\n=== HC19T11: Applicative for Wrapper ==="
  let a = Wrapper (*2)
      b = Wrapper 10
  print (a <*> b)

  putStrLn "\n=== HC19T12: sumThreeApplicative ==="
  print (sumThreeApplicative (Right 2) (Right 3) (Right 5))
  print (sumThreeApplicative (Left "Error") (Right 3) (Right 5))

  putStrLn "\n=== HC19T13: whenApplicative ==="
  whenApplicative True
  whenApplicative False

  putStrLn "\n=== HC19T14: replicateEffect ==="
  _ <- replicateEffect 3 (putStrLn "Hello")
  return ()

  putStrLn "\n=== HC19T15: sequenceEffects ==="
  results15 <- sequenceEffects [return 1, return 2, return 3]
  print results15

  putStrLn "\n=== HC19T16: applyWithEffects ==="
  result16 <- applyWithEffects (return (*2)) (return 5)
  print result16

  putStrLn "\n=== HC19T17: simulateMaybeEffect ==="
  print (simulateMaybeEffect (Just 1) (Just 2) (Just 3))
  print (simulateMaybeEffect (Just 1) Nothing (Just 3))

  putStrLn "\n=== HC19T18: combineEitherResults ==="
  print (combineEitherResults (Right 2) (Right 3) (Right 4))
  print (combineEitherResults (Left "Error") (Right 3) (Right 4))

  putStrLn "\n=== HC19T19: sequenceApplicative ==="
  print (sequenceApplicative [Just 1, Just 2, Just 3])
  print (sequenceApplicative [Just 1, Nothing, Just 3])

  putStrLn "\n=== HC19T20: replicateForever"
  

 
