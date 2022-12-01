#!/usr/bin/env runghc

module Main where

import AoCUtils ( getLines )
import Data.List.Split ( splitOn )
import Data.Array ( listArray )


type Coded = [String]
type Input = [String]
type Line = (Coded, Input)


numbers :: [(Int, String)]
numbers = [ (0, "abcefg")
          , (1, "cf")
          , (2, "acdeg")
          , (3, "acdfg")
          , (4, "bcdf")
          , (5, "abdfg")
          , (6, "abdefg")
          , (7, "acf")
          , (8, "abcdefg")
          , (9, "abcdfg") ]


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    ls <- getLines fileIn
    pure ()


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    pure ()


main :: IO ()
main = do
    putStrLn "day 08"
    fstHalf "input1.txt"
    fstHalf "input2.txt"
    sndHalf "input1.txt"
    sndHalf "input2.txt"


