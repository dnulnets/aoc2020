{-# LANGUAGE OverloadedStrings #-}
module Day1 (problemA, problemB) where

import           Aoc      (Problem (..))

-- First part of day 1, using list comprehension
problemA::Problem [Int] [(Int,Int,Int)]
problemA = Problem {parse = map read . lines, solve = find2020 }
    where
        find2020 l = [(m,x,m*x) |  i <- [1..length l],
                                    let x = last $ take i l,
                                    m <- drop i l,
                                    m + x == 2020]

-- Second part of day 1, using list comprehension
problemB::Problem [Int] [(Int,Int,Int, Int)]
problemB = Problem {parse = map read . lines, solve = find2020}
    where
        find2020 l = [(n,m,x,m*x*n) |  i <- [1..length l],
                                    let n = last $ take i l,
                                    let ll = drop i l,
                                    j <- [1..length ll],
                                    let m = last $ take j ll,
                                    x <- drop j ll,
                                    m + n + x == 2020]
