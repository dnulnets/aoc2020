{-# LANGUAGE OverloadedStrings #-}
module Day7 (problemA, problemB) where

import           Aoc         (Problem (..))
import qualified Text.Parsec as Parsec
import Data.Either (rights)
import Data.List (find)

data Bag = Bag { name::String, count::Int, contains::[Bag] } deriving Show

-- Parser for: "light red bags contain 1 bright white bag, 2 muted yellow bags."
rule::Parsec.Parsec String () Bag
rule = do
    c <- bag
    Parsec.string " contain "
    l <- Parsec.sepBy1 inside $ Parsec.string ", "
    Parsec.char '.'
    Parsec.eof
    return Bag { name = c, count = 1, contains = concat l }
    where

        inside :: Parsec.Parsec String () [Bag]
        inside = do
            n <- Parsec.option "0" $ Parsec.many Parsec.digit <* Parsec.char ' '
            b <- bag
            let nn = read n::Int
            return $ [Bag { name = b, count = nn, contains = []} | nn>0]

        bag::Parsec.Parsec String () String
        bag = do
            bag1 <- Parsec.many Parsec.letter
            Parsec.char ' '
            bag2 <- Parsec.many Parsec.letter
            Parsec.char ' '
            Parsec.many Parsec.letter
            return $ bag1 <> " " <> bag2

findbag::String->[Bag]->Maybe Bag
findbag s = find (iseq s)

iseq::String->Bag->Bool
iseq s b = s == name b

-- First part of day 7
problemA :: Problem [Bag] Int
problemA = Problem {parse = rights . map (Parsec.parse rule "day7") . lines, solve = length . filter id . runbags "shiny gold" }
    where
        runbags s lob = map (\b->runbag s (Just b) lob) lob

        runbag s (Just b) mlob = any (\bb->iseq s bb || runbag s (findbag (name bb) mlob) mlob) (contains b)
        runbag _ Nothing _ = False

-- Second part of day 7
problemB :: Problem [Bag] Int
problemB = Problem {parse = rights . map (Parsec.parse rule "day7") . lines, solve = runbags "shiny gold" }
    where
        runbags s lob = runbag (findbag s lob) lob

        runbag (Just b) mlob = sumbag b + foldl (\ss bb-> ss + count bb * runbag (findbag (name bb) mlob) mlob) 0 (contains b)
            where
                sumbag b = foldl (\s bb -> s + count bb) 0 (contains b)
        runbag Nothing _ = 0
