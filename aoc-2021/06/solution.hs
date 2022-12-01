#!/usr/bin/env runghc

module Main where

import AoCUtils
import Data.List.Split
import Data.List ( foldl' )
import Data.Array


parseFish :: String -> [Int]
parseFish s = toInt `map` splitOn "," s
    where
        toInt :: String -> Int
        toInt = read


fishToList :: [Int] -> [Int]
fishToList fish = [
        length $ filter (== 0) fish,
        length $ filter (== 1) fish,
        length $ filter (== 2) fish,
        length $ filter (== 3) fish,
        length $ filter (== 4) fish,
        length $ filter (== 5) fish,
        length $ filter (== 6) fish,
        length $ filter (== 7) fish,
        length $ filter (== 8) fish
    ]


-- arrayedFished :: Array Int Int
-- arrayedFished = 


dayArr :: [Int] -> [Int]
dayArr (x : xs) = zipWith addTo6 updated [0 ..]
    where
        updated = xs ++ [x]
        addTo6 el i
            | i == 6 = el + x
            | otherwise = el


daysArr :: Int -> [Int] -> [Int]
daysArr ds fish = foldr (const dayArr) fish [1 .. ds]


day :: [Int] -> [Int]
day = foldl' parent []
    where
        parent lst n
            | n == 0 = 6 : 8 : lst
            | otherwise = n - 1 : lst


days :: Int -> [Int] -> [Int]
days days fish = foldr (const day) fish [1 .. days]


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    ls <- getLines fileIn
    -- let fish = parseFish $ head ls
    -- print $ length $ days 80 fish
    let fish = parseFish $ head ls
        arr = fishToList fish
    print $ sum $ daysArr 80 arr
    pure ()


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    let fish = parseFish $ head ls
        arr = fishToList fish
    print $ sum $ daysArr 256 arr
    pure ()


main :: IO ()
main = do
    putStrLn "day 06"
    fstHalf "input1.txt"
    fstHalf "input2.txt"
    sndHalf "input1.txt"
    sndHalf "input2.txt"


