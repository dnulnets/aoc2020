{-# LANGUAGE OverloadedStrings #-}
module Day4 (problemA, problemB) where

import           Aoc                 (Problem (..))
import           Control.Applicative ((<*), (<|>))
import           Data.Either         (either)
import           Data.List           (groupBy)
import           Data.List.Split     (splitOn)
import           Data.Maybe          (isJust)
import qualified Text.Parsec         as Parsec

passports :: String -> [[(String, String)]]
passports ml = passport $ map (map (tuplify . splitOn ":") . splitOn " ") $ lines ml
    where

        tuplify [x,y] = (x,y)
        tuplify _     = ("","")

        passport = filter (/= [("","")]) . map concat . groupBy eop
            where
                eop a b = not ((head a == ("","")) || (head b == ("","")))

-- First part of day 4
problemA :: Problem [[(String, String)]] Int
problemA = Problem {parse = passports, solve = length . filter id . map valid}
    where
        valid l = all (\s -> isJust (lookup s l)) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- Second part of day 4
problemB :: Problem [[(String, String)]] Int
problemB = Problem {parse = passports, solve = length . filter id . map validate}
    where

        validate :: [(String, String)] -> Bool
        validate p = all (\(n,v)->Just True == (either (const False) id <$> (Parsec.parse v "day4" <$> lookup n p))) validators

            where

                validators :: [(String, Parsec.Parsec String () Bool)]
                validators = [("byr", byr), ("iyr", iyr), ("eyr", eyr), ("hgt", hgt),
                            ("hcl", hcl), ("ecl", ecl), ("pid", pid)]

                byr :: Parsec.Parsec String () Bool
                byr = range . read <$> Parsec.count 4 Parsec.digit <* Parsec.eof
                    where
                        range i = i>=1920 && i<=2002

                iyr :: Parsec.Parsec String () Bool
                iyr = range . read <$> Parsec.count 4 Parsec.digit <* Parsec.eof
                    where
                        range i = i>=2010 && i<=2020

                eyr :: Parsec.Parsec String () Bool
                eyr = range . read <$> Parsec.count 4 Parsec.digit <* Parsec.eof
                    where
                        range i = i>=2020 && i<=2030

                hgt :: Parsec.Parsec String () Bool
                hgt = range <$> ((,) <$> Parsec.many1 Parsec.digit <*> (Parsec.string "cm" <|> Parsec.string "in") <* Parsec.eof)
                    where
                        range (s, "cm") = i >=150 && i<=193
                            where
                                i = read s
                        range (s, "in") = i >=59 && i<=76
                            where
                                i = read s
                        range _ = False

                hcl :: Parsec.Parsec String () Bool
                hcl = range <$> (Parsec.char '#' *> Parsec.count 6 (Parsec.oneOf "0123456789abcdef") <* Parsec.eof)
                    where
                        range _ = True

                ecl :: Parsec.Parsec String () Bool
                ecl = range <$> (Parsec.string "amb" <|> (Parsec.char 'b' *> (Parsec.string "lu" <|> Parsec.string "rn"))
                    <|> (Parsec.string "gr" <* (Parsec.char 'y' <|> Parsec.char 'n')) <|> Parsec.string "hzl"
                    <|> Parsec.string "oth") <* Parsec.eof
                    where
                        range _ = True

                pid :: Parsec.Parsec String () Bool
                pid = range  <$> Parsec.count 9 Parsec.digit <* Parsec.eof
                    where
                        range _ = True
