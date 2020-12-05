{-# LANGUAGE OverloadedStrings #-}
module Aoc (Problem(..), notImplemented) where

data Problem a b = Problem {
  parse::String->a
  , solve::a->b}

notImplemented::Problem String String
notImplemented = Problem {parse = const "", solve = const "Not implemented yet!"}
