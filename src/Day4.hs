{-# LANGUAGE OverloadedStrings #-}
module Day4 (problemA, problemB) where

import           Aoc             (Problem (..))
import           Data.List       (groupBy)
import           Data.List.Split (splitOn)
import           Data.Maybe      (isNothing, isJust)

-- First part of day 4
--problemA :: Problem [[(String, String)]] [[(String, String)]]
problemA :: Problem [[(String, String)]] Int
problemA = Problem {parse = plopp, solve = length . filter id . map ppp}

plopp :: String -> [[(String, String)]]
plopp ml = passport $ map (map (tuplify . splitOn ":") . splitOn " ") $ lines ml

tuplify :: [String] -> (String, String)
tuplify [x,y] = (x,y)
tuplify _     = ("","")

passport :: [[(String, String)]] -> [[(String, String)]]
passport = filter (/= [("","")]) . map concat . groupBy pl
    where
        pl a b = not ((head a == ("","")) || (head b == ("","")))

--ppp::[(String, String)] -> [Maybe String]
--ppp :: [(String, String)] -> [Bool]
ppp :: [(String, String)] -> Bool
ppp l = all (\s -> isJust (lookup s l)) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- Second part of day 4
problemB :: Problem [[String]] [[String]]
problemB = Problem {parse = map (splitOn " ") . lines, solve = id}
