{-# LANGUAGE OverloadedStrings #-}
module Day5 (problem) where

import           Aoc      (Problem (..))
import           Data.Set (Set (..), fromList, difference, toList)

allSeats::Set Int
allSeats = fromList [0..127*8+7]

seat::[Char]->Int
seat s = binary (take 7 s)*8 + binary (drop 7 s)

binary::[Char]->Int
binary = foldl digit 0
    where
        digit b 'B' = b * 2 + 1
        digit b 'F' = b * 2
        digit b 'L' = b * 2
        digit b 'R' = b * 2 + 1

problem::Problem [Int] [Int]
problem = Problem {parse = map seat . lines, solve = toList . difference allSeats . fromList }


