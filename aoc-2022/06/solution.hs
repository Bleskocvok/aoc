#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines )

import Data.List ( sort, group, nub )
import Control.Monad ( mapM_ )


dedup :: Ord a => [a] -> [a]
dedup = map head . group . sort


allUnique :: Ord a => [a] -> Bool
allUnique xs = (length xs) == (length $ dedup xs)


markerIndex :: Int -> String -> Int
markerIndex i lst@(a : b : c : d : _)
    | allUnique [a, b, c, d] = i + 4
    | otherwise = markerIndex (i + 1) (tail lst)
markerIndex _ _ = error "markerIndex: invalid input"


-- lol
msgIdx :: Int -> String -> Int
msgIdx idx lst@(a : b : c : d : e : f : g : h : i : j : k : l : m : n : _)
    | allUnique [a, b, c, d, e, f, g, h, i, j, k, l, m, n] = idx + 14
    | otherwise = msgIdx (idx + 1) (tail lst)
msgIdx _ _ = error "messageIndex: invalid input"


fstHalf :: FilePath -> IO ()
fstHalf fileIn = getLines fileIn >>= mapM_ (print . markerIndex 0)


sndHalf :: FilePath -> IO ()
sndHalf fileIn = getLines fileIn >>= mapM_ (print . msgIdx 0)
-- sndHalf fileIn = do
--     ls <- getLines fileIn
--     mapM_ (\l -> do let i = msgIdx 0 l
--                     print i
--                     print $ l !! i
--                     print (drop (i - 14) l)) ls


main :: IO ()
main = do
    putStrLn "day 06"

    putStrLn "\nfirst"
    fstHalf "input1.txt"

    putStrLn "\nsecond"
    sndHalf "input1.txt"


