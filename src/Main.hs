module Main where

import           Aoc                (Problem (..), notImplemented)
import qualified Day5 (problemA, problemB)
import           System.Environment (getArgs)

-- Execute the problem to parse and find a solution
execute :: (Show a, Show b) => Int -> Problem a b -> IO ()
execute n problem = do
    rawData <- readFile $ show n <> ".dat"
    let input = parse problem $ init rawData
    -- putStrLn $ "Input=" <> show input
    putStrLn $ "Output=" <> show (solve problem input)

-- Write out the usage
usage::IO ()
usage = putStrLn "Usage: aoc2020 <1..24>"

-- Main
main :: IO ()
main = do
  args <- getArgs
  if not (null args)
    then case head args of
      "1" -> execute 1 notImplemented
      "2" -> execute 2 notImplemented
      "3" -> execute 3 notImplemented
      "4" -> execute 4 notImplemented
      "5" -> do
        execute 5 Day5.problemA
        execute 5 Day5.problemB
      "6" -> execute 6 notImplemented
      "7" -> execute 7 notImplemented
      "8" -> execute 8 notImplemented
      "9" -> execute 9 notImplemented
      "10" -> execute 10 notImplemented
      "11" -> execute 11 notImplemented
      "12" -> execute 12 notImplemented
      "13" -> execute 13 notImplemented
      "14" -> execute 14 notImplemented
      "15" -> execute 15 notImplemented
      "16" -> execute 16 notImplemented
      "17" -> execute 17 notImplemented
      "18" -> execute 18 notImplemented
      "19" -> execute 19 notImplemented
      "20" -> execute 20 notImplemented
      "21" -> execute 21 notImplemented
      "22" -> execute 22 notImplemented
      "23" -> execute 23 notImplemented
      "24" -> execute 24 notImplemented
      _   -> usage
  else usage
