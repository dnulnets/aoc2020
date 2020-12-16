{-# LANGUAGE OverloadedStrings #-}
module Day9 (problemA, problemB) where

import           Aoc       (Problem (..))


preamble::Int
preamble = 25

-- First part of day 9
problemA :: Problem [Int] [(Int, Bool)]
problemA = Problem {parse = map read . lines, solve = filter (not . snd) . whop }

    where

        whop :: [Int] -> [(Int, Bool)]
        whop l = map (doit l) [preamble..(length l-1)]

        --doit :: [Int]->Int->Bool
        doit :: [Int] -> Int -> (Int, Bool)
        doit l n = combos (l!!n) (drop (n - preamble) $ take n l)
            where
                combos :: Int -> [Int] -> (Int, Bool)
                combos s l = (s, s `elem` ([x + y | x<-l, y<-l, x /= y]))

-- Second part of day 6, using maximum
problemB :: Problem [Int] [(Int, Int)]
problemB = Problem {parse = map read . lines, solve = filter (\b -> fst b == 507622668) . (`sweep` 507622668) }

    where

        sweep :: [Int] -> Int -> [(Int,Int)]
        sweep (x:l) s = [(sum range, minimum range + maximum range)] <> sweep l s
            where
                range = eat (x:l) 0 s
        sweep [] _ = [(0,0)]

        eat::[Int]->Int->Int->[Int]
        eat (x:l) v s
            | x+v >= s = [x]
            | otherwise = [x] <> eat l (x+v) s