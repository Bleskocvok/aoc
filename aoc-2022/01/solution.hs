#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines, splitOn )

import Data.List ( sortOn )


getElves :: [Int] -> [[Int]]
getElves = splitOn (-1)


toInts :: String -> Int
toInts "" = -1
toInts str = read str


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    ls <- getLines fileIn
    let elves = getElves $ toInts `map` ls
        maxElf = maximum $ sum `map` elves
    print maxElf


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    let elves = getElves $ map toInts ls
        sorted = sortOn (* (-1)) $ sum `map` elves
        top = sum $ take 3 sorted
    print top
    pure ()


main :: IO ()
main = do
    putStrLn "day 01"
    putStrLn "\nfirst"
    fstHalf "input1.txt"
    putStrLn "\nsecond"
    sndHalf "input1.txt"
