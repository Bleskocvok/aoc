#!/usr/bin/env runghc

module Main where

import AoCUtils ( getLines )


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
    putStrLn "day 15"
    putStrLn "\nfirst"
    fstHalf "input1.txt"
    fstHalf "input2.txt"
    putStrLn "\nsecond"
    sndHalf "input1.txt"
    sndHalf "input2.txt"


