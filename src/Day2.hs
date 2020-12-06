{-# LANGUAGE OverloadedStrings #-}
module Day2 (problemA, problemB) where

import           Aoc                  (Problem (..))
import           Data.Algebra.Boolean (xor)
import           Data.List            (elemIndex, elemIndices)
import           Data.List.Split      (splitOneOf)
import           Data.Maybe           (isJust)

password :: [String] -> (Int, Int, Char, String)
password (min:max:char:_:pwd:_) = (read min, read max, head char, pwd)

-- First part of day 2
problemA :: Problem [(Int, Int, Char, String)] Int
problemA = Problem {parse = map (password . splitOneOf "-: ") . lines, solve = sum . map test }
    where
        test (l, r, c, p) = if n >= l && n <= r then 1 else 0
            where
                n = length $ filter (c ==) p

-- Second part of day 2
problemB::Problem [(Int, Int, Char, String)] Int
problemB = Problem {parse = map (password . splitOneOf "-: ") . lines, solve = sum . map test}
    where
        test (l, r, c, p) = if xor (isJust (elemIndex (l-1) ix)) (isJust (elemIndex (r-1) ix)) then 1 else 0
            where
                ix = elemIndices c p
