{-# LANGUAGE OverloadedStrings #-}
module Day6 (problemA, problemB) where

import           Aoc       (Problem (..))
import           Data.List (groupBy, nub, intersect)


-- Rule for grouping of answers
group :: String->String->Bool
group a b = not (a=="" || b=="")

-- First part of day 6, using sets
--problemA::Problem [[String]] [[String]]
problemA :: Problem [String] Int
problemA = Problem {parse = filter (/= "") . map concat . groupBy group . lines, solve = sum . map (length . nub) }

-- Second part of day 6, using maximum
problemB :: Problem [[String]] Int
problemB = Problem {parse = groupBy group . lines, solve = sum . map (length . snitt) }
    where
        snitt = foldl intersect ['a'..'z']