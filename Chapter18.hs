-- Chapter 18 
import Data.Char (toLower)

--------------------------------------
-- HC18T1
--------------------------------------
mapToLower :: String -> String
mapToLower = fmap toLower

--------------------------------------
-- HC18T2
--------------------------------------
data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x left right) =
        Node (f x) (fmap f left) (fmap f right)

--------------------------------------
-- HC18T3
--------------------------------------
incrementTreeValues :: Num a => Tree a -> Tree a
incrementTreeValues = fmap (+1)

--------------------------------------
-- HC18T4
--------------------------------------
mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\b -> if b then '1' else '0')

--------------------------------------
-- HC18T5
--------------------------------------
data MyEither e a = MyLeft e | MyRight a
    deriving (Show, Eq)

instance Functor (MyEither e) where
    fmap _ (MyLeft e)  = MyLeft e
    fmap f (MyRight x) = MyRight (f x)

--------------------------------------
-- HC18T6
--------------------------------------
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

--------------------------------------
-- HC18T7
--------------------------------------
data MyTuple a b = MyTuple a b
    deriving (Show, Eq)

instance Functor (MyTuple a) where
    fmap f (MyTuple x y) = MyTuple x (f y)

fmapTuple :: (b -> c) -> MyTuple a b -> MyTuple a c
fmapTuple = fmap

--------------------------------------
-- HC18T8
--------------------------------------
identityLawCheck :: (Eq (f a), Functor f) => f a -> Bool
identityLawCheck x = fmap id x == x

--------------------------------------
-- HC18T9
--------------------------------------
compositionLawCheck :: (Eq (f c), Functor f)
                    => (b -> c) -> (a -> b) -> f a -> Bool
compositionLawCheck f g x =
    fmap (f . g) x == (fmap f . fmap g) x

--------------------------------------
-- HC18T10
--------------------------------------
nestedFmap :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
nestedFmap = fmap . fmap

--------------------------------------
-- Main
--------------------------------------
main :: IO ()
main = do
    putStrLn "HC18T1"
    putStrLn (mapToLower "HeLLo WoRLd!")

    putStrLn "\nHC18T2 & HC18T3"
    let tree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
    print tree
    print $ incrementTreeValues tree

    putStrLn "\nHC18T4"
    print $ mapToBits [True, False, True, True, False]

    putStrLn "\nHC18T5"
    print $ fmap (+1) (MyRight 10 :: MyEither String Int)
    print $ fmap (+1) (MyLeft "Error" :: MyEither String Int)

    putStrLn "\nHC18T6"
    print $ applyToMaybe (*2) (Just 5)
    print $ applyToMaybe (*2) Nothing

    putStrLn "\nHC18T7"
    print $ fmapTuple (++ "!") (MyTuple "Hi" "there")
    print $ fmapTuple (*2) (MyTuple "Value" 10)

    putStrLn "\nHC18T8"
    print $ identityLawCheck (Just 42)
    print $ identityLawCheck [1,2,3]

    putStrLn "\nHC18T9"
    print $ compositionLawCheck (+1) (*2) [1,2,3]
    print $ compositionLawCheck (++"!") reverse (Just "Hi")

    putStrLn "\nHC18T10"
    print $ nestedFmap (*2) (Just [1,2,3])
    print $ nestedFmap (++"!") (MyRight ["hi","there"] :: MyEither String [String])
