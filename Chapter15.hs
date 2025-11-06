{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Exception
import Data.Typeable
import Text.Read (readMaybe)

-- =========================================================
-- Custom Exception for Traffic Light (used in Task 3 and 4)
-- =========================================================
data TrafficException = InvalidLight String deriving (Show, Typeable)
instance Exception TrafficException

-- =========================================================
-- Task 1
-- =========================================================
hc15t1 :: IO ()
hc15t1 = do
    putStrLn "\n=============================="
    putStrLn "HC15T1: Handle Exceptions for File Reading and Velocity Calculation"
    putStrLn "=============================="
    let fileName = "example.txt"
    result <- try (readFile fileName) :: IO (Either IOError String)
    case result of
        Left err -> putStrLn ("Error reading file: " ++ show err)
        Right content -> do
            putStrLn ("File content preview: " ++ take 50 content ++ "...")
            let dStr = "100"
            let tStr = "5"
            case (readMaybe dStr :: Maybe Double, readMaybe tStr :: Maybe Double) of
                (Just d, Just t) ->
                    if t == 0
                        then putStrLn "Error: Division by zero!"
                        else putStrLn ("Velocity = " ++ show (d / t) ++ " m/s")
                _ -> putStrLn "Invalid numeric input!"

-- =========================================================
-- Task 2
-- =========================================================
hc15t2 :: IO ()
hc15t2 = do
    putStrLn "\n=============================="
    putStrLn "HC15T2: Self-Driving AI Car System"
    putStrLn "=============================="
    let colors = ["red", "yellow", "green", "blue"]
    mapM_ (\c -> putStrLn (c ++ " -> " ++ selfDrivingCar c)) colors

selfDrivingCar :: String -> String
selfDrivingCar "red"    = "Stop 🚦"
selfDrivingCar "yellow" = "Slow down ⚠️"
selfDrivingCar "green"  = "Go 🟢"
selfDrivingCar _        = "Invalid color ❌"

-- =========================================================
-- Task 3
-- =========================================================
hc15t3 :: IO ()
hc15t3 = do
    putStrLn "\n=============================="
    putStrLn "HC15T3: Custom Exception for Traffic Light Errors"
    putStrLn "=============================="
    let color = "purple"
    if color `elem` ["red", "yellow", "green"]
        then putStrLn "Traffic light accepted."
        else throwIO (InvalidLight color)

-- =========================================================
-- Task 4
-- =========================================================
hc15t4 :: IO ()
hc15t4 = handle handler $ do
    putStrLn "\n=============================="
    putStrLn "HC15T4: Exception Handler for Traffic Light"
    putStrLn "=============================="
    let color = "orange"
    if color `elem` ["red", "yellow", "green"]
        then putStrLn ("Action for " ++ color ++ ": valid")
        else throwIO (InvalidLight color)

handler :: TrafficException -> IO ()
handler (InvalidLight c) = putStrLn $ "Error: Invalid traffic light '" ++ c ++ "'"

-- =========================================================
-- Task 5
-- =========================================================
hc15t5 :: IO ()
hc15t5 = do
    putStrLn "\n=============================="
    putStrLn "HC15T5: Safe Division Using Maybe"
    putStrLn "=============================="
    print (safeDiv 10 2)
    print (safeDiv 5 0)

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

-- =========================================================
-- Task 6
-- =========================================================
hc15t6 :: IO ()
hc15t6 = do
    putStrLn "\n=============================="
    putStrLn "HC15T6: Safe Input Parsing with readMaybe"
    putStrLn "=============================="
    let inputs = ["42", "abc"]
    mapM_ (\i -> putStrLn (i ++ " -> " ++ show (readMaybe i :: Maybe Int))) inputs

-- =========================================================
-- Task 7
-- =========================================================
hc15t7 :: IO ()
hc15t7 = do
    putStrLn "\n=============================="
    putStrLn "HC15T7: Velocity Calculation with Optionals"
    putStrLn "=============================="
    let dStr = "120"
    let tStr = "4"
    case (readMaybe dStr :: Maybe Double, readMaybe tStr :: Maybe Double) of
        (Just d, Just t) ->
            case safeDiv d t of
                Just v  -> putStrLn ("Velocity = " ++ show v ++ " m/s")
                Nothing -> putStrLn "Error: Division by zero!"
        _ -> putStrLn "Invalid input!"

-- =========================================================
-- Task 8
-- =========================================================
hc15t8 :: IO ()
hc15t8 = do
    putStrLn "\n=============================="
    putStrLn "HC15T8: Division with Either for Detailed Errors"
    putStrLn "=============================="
    print (safeDivEither 10 2)
    print (safeDivEither 5 0)

safeDivEither :: Double -> Double -> Either String Double
safeDivEither _ 0 = Left "Error: Division by zero!"
safeDivEither x y = Right (x / y)

-- =========================================================
-- Task 9
-- =========================================================
hc15t9 :: IO ()
hc15t9 = do
    putStrLn "\n=============================="
    putStrLn "HC15T9: Try Function for File IO Exceptions"
    putStrLn "=============================="
    result <- try (readFile "nonexistent.txt") :: IO (Either IOError String)
    case result of
        Left err -> putStrLn ("File read error: " ++ show err)
        Right c  -> putStrLn ("File content preview: " ++ take 50 c ++ "...")

-- =========================================================
-- Task 10
-- =========================================================
hc15t10 :: IO ()
hc15t10 = do
    putStrLn "\n=============================="
    putStrLn "HC15T10: Hybrid Error Handling with Either and IO"
    putStrLn "=============================="
    let dStr = "200"
    let tStr = "0"
    case (readMaybe dStr :: Maybe Double, readMaybe tStr :: Maybe Double) of
        (Just d, Just t) ->
            case safeDivEither d t of
                Left err -> putStrLn err
                Right v  -> putStrLn ("Velocity = " ++ show v ++ " m/s")
        _ -> putStrLn "Invalid input!"

-- =========================================================
-- MAIN
-- =========================================================
main :: IO ()
main = do
    hc15t1
    hc15t2
    catch hc15t3 (\(InvalidLight c) -> putStrLn $ "Caught exception: " ++ c)
    hc15t4
    hc15t5
    hc15t6
    hc15t7
    hc15t8
    hc15t9
    hc15t10
 
