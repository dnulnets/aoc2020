{-# LANGUAGE OverloadedStrings #-}
module Day8 (problemA, problemB) where

import           Aoc         (Problem (..))
import qualified Text.Parsec as Parsec
import Data.Either (fromRight)
import qualified Data.Vector as Vector

data Mnemonic = NOP | JMP | ACC | ERR deriving (Eq, Show)
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

execute::Vector.Vector Instruction->Vector.Vector Bool->Int->Int->(Int, Bool)
execute prog mem pc acc = case mem Vector.!? pc of
                            Nothing -> (acc, True)
                            Just b -> if b then
                                        (acc, False)
                                    else
                                        case prog Vector.! pc of
                                            Instruction ACC v -> execute prog (mem Vector.// [(pc,True)]) (pc+1) (acc+v)
                                            Instruction JMP v -> execute prog (mem Vector.// [(pc,True)]) (pc+v) acc
                                            Instruction NOP _ -> execute prog (mem Vector.// [(pc,True)]) (pc+1) acc
                                            _ -> execute prog (mem Vector.// [(pc,True)]) (pc+1) acc

-- First part of day 8
problemA :: Problem (Vector.Vector Instruction) (Int, Bool)
problemA = Problem {parse = Vector.map (fromRight (Instruction ERR 0) . Parsec.parse instruction "day8") . Vector.fromList . lines, 
    solve = \p -> execute p (Vector.replicate (Vector.length p) False) 0 0 }

-- Second part of day 8
problemB :: Problem (Vector.Vector Instruction) (Vector.Vector (Int, (Int, Bool)))
problemB = Problem {parse = Vector.map (fromRight (Instruction ERR 0) . Parsec.parse instruction "day8") . Vector.fromList . lines, 
    solve = Vector.filter (\(_, (_, b))->b) . runAll }

    where

        runAll :: Vector.Vector Instruction -> Vector.Vector (Int, (Int, Bool))
        runAll p = Vector.map (\ix->(ix, runSwapped p ix)) (allPossibleSwaps p)

        runSwapped :: Vector.Vector Instruction -> Int -> (Int, Bool)
        runSwapped p ix = execute np (Vector.replicate (Vector.length np) False) 0 0
            where 
                np = swap p ix    

        swap :: Vector.Vector Instruction -> Int -> Vector.Vector Instruction
        swap p i = case p Vector.! i of
                    Instruction NOP v -> p Vector.// [(i, Instruction JMP v)]
                    Instruction JMP v -> p Vector.// [(i, Instruction NOP v)]
                    
        allPossibleSwaps :: Vector.Vector Instruction -> Vector.Vector Int
        allPossibleSwaps = Vector.findIndices (\i->operation i == NOP || operation i == JMP)
