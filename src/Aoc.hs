{-# LANGUAGE OverloadedStrings #-}
module Aoc (Problem(..), notImplemented) where

-- The problem datatype that contains a parser for the file and a solver for the problem
data Problem a b = Problem {
  parse::String->a
  , solve::a->b}

-- The NotImplemented problem, whenever I have no tsolved it yet
notImplemented::Problem String String
notImplemented = Problem {parse = const "", solve = const "Not implemented yet!"}
