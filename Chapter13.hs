-- Main.hs
-- Haskell Chapter 13 Practical Tasks: Working with Modules and Directories

module Main where

import System.Directory
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Char as Char
import SumNonEmpty (sumNonEmpty)

-- HC13T6: Convert file names to a map
fileNamesToMap :: [FilePath] -> Map.Map Int FilePath
fileNamesToMap files = Map.fromList (zip [1..] files)

main :: IO ()
main = do
  putStrLn "========================================"
  putStrLn " HASKELL CHAPTER 13 PRACTICAL TASKS"
  putStrLn "========================================"

  -- HC13T1: List all files in the directory
  putStrLn "\n--- HC13T1: List Files in Directory ---"
  files <- listDirectory "."
  print files

  -- HC13T2: Filter files by substring
  putStrLn "\n--- HC13T2: Filter Files by Substring ---"
  let keyword = "hs"
  let filtered = filter (List.isInfixOf keyword) files
  putStrLn $ "Files containing '" ++ keyword ++ "':"
  print filtered

  -- HC13T3: Sort and return filtered files
  putStrLn "\n--- HC13T3: Sort and Return Filtered Files ---"
  let sortedFiltered = List.sort filtered
  print sortedFiltered

  -- HC13T4 + HC13T5: Using the SumNonEmpty module
  putStrLn "\n--- HC13T4 & HC13T5: SumNonEmpty Module ---"
  print (sumNonEmpty [1, 2, 3, 4, 5])

  -- HC13T6: File names to Map
  putStrLn "\n--- HC13T6: File Names to Map ---"
  print (fileNamesToMap files)

  -- HC13T7: Use Custom Module
  putStrLn "\n--- HC13T7: Use Custom Module in Main ---"
  let nums = [5, 10, 15]
  putStrLn $ "Sum of " ++ show nums ++ " = " ++ show (sumNonEmpty nums)

  -- HC13T8: Qualified imports to handle name conflicts
  putStrLn "\n--- HC13T8: Qualified Imports for Name Conflicts ---"
  let nums2 = [3, 1, 2]
  putStrLn $ "Sorted: " ++ show (List.sort nums2)
  putStrLn $ "Reversed: " ++ show (List.reverse nums2)

  -- HC13T9: Renaming module namespace
  putStrLn "\n--- HC13T9: Renaming Module Namespace ---"
  let word = "haskell"
  putStrLn $ "Uppercase: " ++ map Char.toUpper word
  putStrLn $ "Joined with dashes: " ++ List.intercalate "-" (map (:[]) word)

  -- HC13T10: Multi-module main function using System.Directory & Data.List
  putStrLn "\n--- HC13T10: Multi-Module Main Function ---"
  let sortedFiles = List.sort files
  putStrLn "Sorted files in current directory:"
  mapM_ putStrLn sortedFiles

 
