{-# LANGUAGE OverloadedStrings #-}
module Day8 (problemA, problemB) where

import           Aoc         (Problem (..))
import qualified Text.Parsec as Parsec
import Data.Either (rights, fromRight)
import Data.Set (Set(..), insert, member)
import qualified Data.Vector as Vector

data Mnemonic = NOP | JMP | ACC | ERR deriving Show
data Instruction = Instruction {
        operation::Mnemonic,
        value::Int
    } deriving Show

-- Parser for: "light red bags contain 1 bright white bag, 2 muted yellow bags."

instruction::Parsec.Parsec String () Instruction
instruction = do
    m1 <- Parsec.many Parsec.letter
    let m2 = case m1 of
                "nop" -> NOP
                "jmp" -> JMP
                "acc" -> ACC
                _ -> ERR
    Parsec.char ' '
    s <- Parsec.oneOf "+-"
    n <- Parsec.many Parsec.digit
    let v = case s of
                '+' -> read n
                '-' -> - read n
                _ -> 0
    return Instruction {operation = m2, value = v}

-- First part of day 8
problemA :: Problem (Vector.Vector Instruction) Int
problemA = Problem {parse = Vector.map (fromRight (Instruction ERR 0) . Parsec.parse instruction "day8") . Vector.fromList . lines, solve = \p -> execute p (Vector.replicate (Vector.length p) False) 0 0 }

execute::Vector.Vector Instruction->Vector.Vector Bool->Int->Int->Int
execute prog mem pc acc = if mem Vector.! pc then
                            acc
                        else
                            case prog Vector.! pc of
                                Instruction ACC v -> execute prog (mem Vector.// [(pc,True)]) (pc+1) (acc+v)
                                Instruction JMP v -> execute prog (mem Vector.// [(pc,True)]) (pc+v) acc
                                Instruction NOP _ -> execute prog (mem Vector.// [(pc,True)]) (pc+1) acc
                                _ -> execute prog (mem Vector.// [(pc,True)]) (pc+1) acc

-- Second part of day 8
problemB :: Problem [Instruction] [Instruction]
problemB = Problem {parse = rights . map (Parsec.parse instruction "day8") . lines, solve = id }

