{-# LANGUAGE OverloadedStrings #-}
module Day3 (problemA, problemB) where

import           Aoc                  (Problem (..))
import           Data.Matrix          (Matrix (..), getElem, fromLists)

start :: Num p => Int -> Int -> Matrix Char -> p
start sr sc m = _start m 0 0 sr sc
    where
        _start m r c sr sc
            | r >= nrows m  = 0
            | otherwise     = tree m r c + _start m (r+sr) (c+sc) sr sc

        tree m r c =
                if getElem (mod r nr+1) (mod c nc+1) m == '#' then 1 else 0
            where
                nc = ncols m
                nr = nrows m

-- First part of day 3
problemA :: Problem (Matrix Char) Int
problemA = Problem {parse = fromLists . lines, solve = start 1 3}

-- Second part of day 3
problemB :: Problem (Matrix Char) Int
problemB = Problem {parse = fromLists . lines, solve = multi}
    where
        multi m = start 1 1 m * start 1 3 m * start 1 5 m * start 1 7 m * start 2 1 m
