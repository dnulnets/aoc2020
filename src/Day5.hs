{-# LANGUAGE OverloadedStrings #-}
module Day5 (problemA, problemB) where

import           Aoc      (Problem (..))
import           Data.Set (Set (..), fromList, difference, toList)

-- Seat parser
seat::[Char]->Int
seat s = binary (take 7 s)*8 + binary (drop 7 s)
    where 
        binary = foldl addDigit 0
            where
                addDigit b 'B' = b * 2 + 1
                addDigit b 'F' = b * 2
                addDigit b 'L' = b * 2
                addDigit b 'R' = b * 2 + 1

-- Second part of day 5, using sets
problemB::Problem [Int] [Int]
problemB = Problem {parse = map seat . lines, solve = findSeat}
    where
        allSeats = fromList [0..127*8+7]
        findSeat l = toList $ difference (fromList $ freeSeats l) (neighbours $ freeSeats l)
        freeSeats = toList . difference allSeats . fromList
        neighbours = fromList . concatMap neighbour
            where
                neighbour n = [n-1, n+1]

-- First part of day 5, using maximum
problemA::Problem [Int] Int
problemA = Problem {parse = map seat . lines, solve = maximum }
