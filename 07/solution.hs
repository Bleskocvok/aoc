#!/usr/bin/env runghc

module Main where

import AoCUtils ( getCSVNums )
import Data.List ( sortOn )
import Text.Printf ( printf )


type FuelFunc = (Int -> Int -> Int)


diff :: FuelFunc
diff d = abs . (d -)


rising :: FuelFunc
rising d cur = n * (1 + n) `div` 2
    where
        n = abs (cur - d)
        beg = min d cur
        end = max d cur


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    vals <- getCSVNums fileIn
    let best = bestDepthOn diff vals
        fuel = fuelNeededOn diff vals best
    printf "%d: %d\n" best fuel
    pure ()


bestDepthOn :: FuelFunc -> [Int] -> Int
bestDepthOn f crabs = fst $ head $ sortOn snd allOptions
    where
        low = minimum crabs
        hig = maximum crabs
        allOptions = [(d, fuel) | d <- [low .. hig],
                                  let fuel = fuelNeededOn f crabs d]


fuelNeededOn :: FuelFunc -> [Int] -> Int -> Int
fuelNeededOn f lst d = sum $ map (f d) lst


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    vals <- getCSVNums fileIn
    let best = bestDepthOn rising vals
        fuel = fuelNeededOn rising vals best
    printf "%d: %d\n" best fuel
    pure ()


main :: IO ()
main = do
    putStrLn "day 07"
    fstHalf "input1.txt"
    fstHalf "input2.txt"
    sndHalf "input1.txt"
    sndHalf "input2.txt"



