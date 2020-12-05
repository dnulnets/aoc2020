{-# LANGUAGE OverloadedStrings #-}
module Day5 (problemA, problemB) where

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

-- Return with the list of free sats on the plane
--freeSeats :: [Int] -> [Int]
--freeSeats = toList . difference allSeats . fromList

-- Return with a list of the neighbours given a seat
--neighbours::[Int] -> [Int]
--neighbours = concatMap neighbour
--    where
--        neighbour n = [n-1, n+1]

-- Second part of day 5
problemB::Problem [Int] [Int]
problemB = Problem {parse = map seat . lines, solve = findSeat}
    where
        findSeat l = toList (difference (fromList (freeSeats l)) (neighbours (freeSeats l)))
        freeSeats = toList . difference allSeats . fromList
        neighbours l = fromList $ concatMap neighbour l
            where
                neighbour n = [n-1, n+1]

-- First part of day 5
problemA::Problem [Int] Int
problemA = Problem {parse = map seat . lines, solve = maximum }
