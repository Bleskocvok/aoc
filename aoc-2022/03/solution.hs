#!/usr/bin/env -S runhaskell -i../

module Main where

import AoCUtils ( getLines )

import Data.Bifunctor ( bimap )
import Data.List ( sort, group )
import Data.Maybe ( fromJust, isJust )
import Data.Char ( ord )


calcScore :: Char -> Int
calcScore c
    | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
    | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 27
    | otherwise = error "invalid character"


dedup :: Ord a => [a] -> [a]
dedup = map head . group . sort


filterCommon :: Ord a => [a] -> [a] -> [a]
filterCommon (x : xs) (y : ys)
    | x == y = x : filterCommon xs ys
    | x < y = filterCommon xs (y : ys)
    | x > y = filterCommon (x : xs) ys
filterCommon _ _ = []


scoreLine :: String -> Int
scoreLine line =
    let
        halves = bimap dedup dedup (splitAt (length line `div` 2) line)
        common = uncurry filterCommon halves
        score = sum $ calcScore `map` common
    in
        score


scoreGroup3 :: [Char] -> [Char] -> [Char] -> Int
scoreGroup3 x y z =
    let
        (dx, dy, dz) = (dedup x, dedup y, dedup z)
        common = filterCommon dz (filterCommon dx dy)
    in
        calcScore $ head common


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    ls <- getLines fileIn
    let score = sum $ scoreLine `map` ls
    print score


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    let score (x : y : z : xs) = scoreGroup3 x y z + score xs
        score [] = 0
        score _ = error "sndHalf: invalid number of lines"
    print $ score ls


main :: IO ()
main = do
    putStrLn "day 03"

    putStrLn "\nfirst"
    fstHalf "input1.txt"

    putStrLn "\nsecond"
    sndHalf "input1.txt"


