{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import System.Random (randomRIO)
import qualified Data.List as List
import qualified Data.Map as Map
import Utils

main :: IO ()
main = do
    putStrLn "HC14T1: Hello, Cabal!"

    -- HC14T2: Random number
    num <- randomRIO (1, 100 :: Int)
    putStrLn $ "HC14T2: Random number between 1 and 100: " ++ show num

    -- HC14T3: Numeric underscores
    let big1 = 1_000_000
        big2 = 2_500_000
    putStrLn $ "HC14T3: Numbers with underscores: " ++ show big1 ++ " and " ++ show big2

    -- HC14T4: TypeApplications
    putStrLn $ "HC14T4: String \"123\" to Int: " ++ show (strToInt "123")

    -- HC14T5: Custom data type Result and pattern matching
    let r1 = Success 42
        r2 = Failure "Something went wrong"
    putStrLn $ "HC14T5: " ++ describeResult r1
    putStrLn $ "HC14T5: " ++ describeResult r2

    -- HC14T8: Character frequency
    let text = "hello world"
    putStrLn $ "HC14T8: Char counts in \"" ++ text ++ "\": " ++ show (counts text)

    -- HC14T9: Partial type signature sum
    let nums = [1,2,3,4,5]
    putStrLn $ "HC14T9: Sum using partial type signature: " ++ show (sumAny nums)

    -- HC14T10: Demonstrate library usage
    putStrLn $ "HC14T10: Sorting numbers: " ++ show (List.sort [5,2,9,1])
    putStrLn $ "HC14T10: Map example: " ++ show (Map.fromList [("one",1),("two",2)])
