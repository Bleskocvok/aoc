#!/usr/bin/env runghc

module Main where

import Data.List ( foldl' )


data Instr = Forward Int | Down Int | Up Int
    deriving (Show)


follow :: [Instr] -> (Int, Int)
follow = foldl' apply (0, 0)
    where
        apply :: (Int, Int) -> Instr -> (Int, Int)
        apply (h, d) (Forward n) = (h + n, d)
        apply (h, d) (Down n)    = (h, d + n)
        apply (h, d) (Up n)      = (h, d - n)


fst3, snd3 :: (a, a, c) -> a
fst3 (x, _, _) = x
snd3 (_, x, _) = x


followEx :: [Instr] -> (Int, Int)
followEx lst =
    let
        res = foldl' apply (0, 0, 0) lst
    in
        (fst3 res, snd3 res)
    where
        apply :: (Int, Int, Int) -> Instr -> (Int, Int, Int)
        apply (h, d, a) (Forward n) = (h + n, d + a * n, a)
        apply (h, d, a) (Down n)    = (h, d, a + n)
        apply (h, d, a) (Up n)      = (h, d, a - n)


mult :: (Int, Int) -> Int
mult (x, y) = x * y


main :: IO ()
main = do
    printThing inputTest
    printThing input1

    printThingEx inputTest
    printThingEx input1

    where
        printThing a = do
            let res = follow a
            print res
            print $ mult res
        
        printThingEx a = do
            let res = followEx a
            print res
            print $ mult res


inputTest :: [Instr]
inputTest =
    [
        Forward 5,
        Down 5,
        Forward 8,
        Up 3,
        Down 8,
        Forward 2
    ]


input1 :: [Instr]
input1 = 
    [
        Forward 1, Down 3, Down 2, Up 1, Down 7, Down 8, Forward 6, Forward 1, Forward 1, Down 6, Up 3, Down 7, Down 1, Down 6, Forward 6, Down 6, Forward 3, Up 7, Forward 5, Down 4, Forward 6, Forward 1, Forward 6, Forward 9, Forward 3, Up 1, Forward 7, Down 9, Forward 2, Up 9, Down 5, Down 3, Up 1, Down 6, Down 7, Down 8, Down 7, Forward 7, Forward 3, Down 5, Down 2, Forward 4, Forward 7, Down 8, Down 4, Down 5, Down 1, Forward 4, Up 6, Forward 6, Forward 4, Forward 5, Up 3, Up 6, Down 4, Down 8, Forward 4, Forward 5, Down 3, Forward 1, Down 5, Down 5, Forward 8, Forward 9, Forward 1, Forward 8, Forward 5, Forward 6, Up 8, Down 3, Forward 8, Up 2, Down 3, Down 9, Up 9, Forward 5, Down 2, Forward 7, Forward 5, Forward 5, Down 2, Down 3, Forward 7, Forward 9, Down 9, Forward 3, Forward 3, Down 3, Forward 3, Up 6, Down 4, Forward 3, Forward 3, Forward 7, Down 4, Forward 8, Up 8, Down 3, Down 2, Forward 2, Down 6, Down 6, Up 7, Up 9, Down 4, Up 7, Up 8, Down 6, Down 1, Forward 6, Forward 9, Forward 1, Forward 8, Down 4, Up 3, Forward 6, Down 5, Forward 7, Forward 5, Down 2, Forward 7, Down 6, Forward 7, Up 2, Up 5, Forward 3, Down 6, Down 9, Down 9, Forward 9, Up 7, Down 6, Down 4, Down 8, Forward 6, Up 6, Down 2, Forward 5, Up 7, Forward 5, Down 1, Up 8, Forward 2, Forward 4, Down 5, Down 8, Up 7, Up 5, Down 2, Forward 8, Forward 4, Up 6, Forward 8, Down 3, Down 2, Down 2, Forward 4, Forward 2, Forward 9, Up 3, Up 7, Up 3, Up 5, Forward 5, Up 8, Forward 3, Forward 1, Down 3, Down 8, Forward 6, Forward 5, Up 5, Down 9, Down 1, Down 9, Forward 9, Up 3, Forward 7, Forward 6, Forward 1, Up 3, Forward 9, Forward 7, Down 9, Up 6, Forward 2, Up 9, Forward 6, Forward 4, Up 3, Forward 5, Down 4, Up 8, Up 4, Up 9, Down 1, Down 7, Forward 7, Forward 2, Down 9, Forward 7, Forward 2, Down 6, Down 9, Up 6, Down 7, Down 2, Down 7, Down 6, Down 4, Down 4, Forward 6, Forward 9, Forward 4, Down 7, Down 5, Up 6, Down 7, Forward 2, Down 3, Down 1, Forward 1, Forward 5, Down 4, Down 4, Down 1, Down 9, Forward 6, Down 3, Forward 5, Up 7, Forward 9, Down 8, Down 3, Up 2, Up 2, Forward 4, Down 7, Forward 2, Forward 5, Forward 4, Forward 3, Down 8, Down 6, Forward 9, Down 1, Forward 1, Down 1, Up 8, Down 2, Forward 2, Up 5, Down 7, Forward 8, Down 7, Down 4, Forward 2, Forward 6, Up 2, Forward 8, Forward 2, Forward 1, Up 5, Down 4, Down 8, Forward 4, Down 8, Up 8, Forward 3, Down 5, Forward 2, Forward 1, Up 3, Forward 9, Up 5, Forward 5, Forward 5, Up 8, Down 6, Forward 3, Down 4, Up 5, Forward 3, Up 6, Forward 6, Forward 9, Down 7, Down 7, Down 8, Down 4, Forward 4, Forward 3, Forward 3, Forward 5, Down 3, Forward 8, Up 5, Forward 1, Up 7, Down 5, Forward 7, Forward 3, Down 3, Forward 2, Forward 1, Forward 4, Up 2, Down 8, Forward 9, Forward 9, Down 6, Down 1, Down 5, Forward 4, Down 2, Forward 1, Up 7, Down 9, Forward 3, Down 5, Down 5, Up 6, Down 6, Forward 8, Down 1, Up 3, Down 2, Up 1, Forward 5, Down 4, Down 6, Forward 8, Down 4, Down 5, Down 2, Forward 5, Forward 8, Down 8, Up 4, Forward 2, Up 2, Down 9, Forward 6, Forward 1, Forward 5, Up 2, Down 1, Up 7, Up 3, Forward 3, Down 7, Forward 2, Forward 4, Up 7, Forward 4, Forward 6, Up 2, Forward 4, Forward 2, Down 6, Down 5, Down 5, Down 6, Forward 9, Up 4, Down 4, Down 7, Up 6, Up 9, Up 4, Down 4, Up 7, Down 9, Down 9, Forward 3, Down 7, Down 7, Down 7, Down 2, Up 2, Up 1, Up 6, Down 8, Up 7, Down 4, Forward 8, Down 7, Up 1, Down 5, Down 3, Forward 6, Up 1, Down 5, Forward 3, Forward 6, Forward 7, Forward 2, Down 3, Forward 1, Up 9, Down 5, Up 2, Up 9, Up 2, Up 2, Forward 1, Down 2, Forward 1, Down 7, Forward 1, Forward 8, Down 9, Down 1, Forward 9, Forward 7, Forward 7, Down 5, Down 5, Down 3, Forward 6, Down 7, Down 4, Forward 2, Up 6, Down 3, Up 4, Up 7, Forward 2, Forward 8, Forward 4, Down 7, Down 9, Up 1, Down 2, Up 8, Down 2, Up 6, Forward 6, Up 5, Down 2, Forward 5, Down 4, Forward 7, Down 3, Down 5, Forward 1, Down 7, Forward 1, Up 1, Forward 4, Up 4, Forward 4, Forward 5, Forward 7, Down 7, Down 2, Up 4, Down 2, Down 3, Forward 3, Forward 2, Forward 5, Forward 2, Up 8, Forward 3, Down 7, Down 9, Forward 3, Forward 6, Down 7, Down 5, Down 3, Up 3, Forward 5, Forward 2, Down 1, Up 1, Up 8, Down 6, Down 4, Down 1, Forward 1, Down 7, Up 8, Forward 2, Forward 8, Forward 8, Forward 7, Down 1, Forward 8, Down 8, Forward 4, Down 3, Up 9, Down 8, Forward 9, Down 7, Up 2, Forward 9, Down 4, Up 3, Up 4, Up 4, Forward 5, Up 2, Forward 3, Down 5, Forward 5, Down 5, Up 8, Down 4, Forward 6, Down 6, Forward 7, Forward 2, Down 2, Up 7, Down 5, Down 9, Down 8, Down 4, Up 3, Forward 4, Down 8, Down 8, Down 9, Down 7, Forward 2, Forward 8, Up 5, Forward 8, Down 9, Forward 6, Up 1, Down 6, Forward 1, Up 4, Down 3, Forward 3, Forward 2, Down 6, Forward 7, Up 6, Up 9, Down 1, Forward 3, Forward 4, Forward 2, Up 8, Forward 9, Up 7, Down 2, Forward 2, Up 7, Down 2, Up 6, Down 2, Forward 9, Forward 3, Down 6, Down 5, Down 3, Forward 9, Down 8, Down 8, Down 2, Down 7, Up 3, Forward 1, Down 7, Up 8, Up 8, Forward 5, Forward 5, Forward 1, Down 8, Down 6, Forward 2, Up 3, Forward 1, Forward 7, Forward 4, Forward 5, Forward 9, Forward 7, Forward 6, Forward 3, Forward 4, Down 8, Down 1, Forward 6, Forward 9, Forward 6, Forward 9, Forward 6, Up 3, Down 8, Forward 4, Forward 1, Down 4, Forward 9, Down 8, Down 3, Up 2, Forward 5, Forward 2, Forward 5, Down 6, Down 3, Up 1, Down 9, Up 5, Forward 6, Down 7, Forward 1, Forward 9, Down 2, Down 5, Forward 3, Forward 6, Down 4, Down 5, Up 4, Forward 7, Forward 5, Down 8, Forward 6, Down 5, Forward 2, Down 7, Forward 4, Forward 8, Down 8, Forward 2, Forward 8, Down 5, Forward 7, Down 8, Down 1, Forward 8, Down 4, Up 4, Down 7, Down 6, Up 5, Forward 4, Forward 1, Forward 4, Down 5, Forward 5, Forward 9, Down 1, Forward 3, Up 7, Down 1, Down 7, Forward 2, Down 5, Down 6, Forward 5, Up 2, Down 9, Forward 1, Up 5, Forward 6, Forward 9, Forward 4, Up 4, Down 6, Up 9, Up 5, Down 2, Up 9, Down 2, Down 4, Down 8, Down 2, Forward 2, Forward 2, Down 9, Up 5, Forward 2, Forward 8, Down 2, Down 2, Down 9, Down 3, Down 9, Up 9, Up 3, Down 1, Down 9, Down 2, Forward 7, Down 2, Up 3, Down 9, Up 2, Up 4, Forward 5, Forward 7, Down 7, Up 7, Up 5, Down 8, Up 2, Forward 2, Down 3, Down 5, Forward 2, Forward 3, Forward 3, Down 1, Down 1, Forward 9, Down 5, Down 7, Forward 7, Forward 5, Up 9, Forward 3, Up 4, Forward 1, Forward 3, Down 4, Forward 9, Down 5, Down 3, Down 5, Forward 6, Down 6, Forward 2, Up 4, Down 4, Forward 2, Down 8, Up 9, Forward 9, Forward 4, Down 8, Forward 2, Forward 5, Forward 1, Forward 5, Up 1, Forward 7, Forward 9, Down 5, Forward 6, Down 1, Forward 6, Down 2, Forward 9, Down 1, Forward 1, Down 4, Down 6, Down 2, Up 7, Up 5, Forward 8, Forward 1, Down 8, Forward 1, Forward 2, Down 8, Forward 7, Down 5, Forward 1, Down 2, Up 7, Forward 7, Down 4, Down 8, Up 6, Up 4, Forward 7, Down 3, Up 5, Down 5, Forward 7, Up 7, Down 6, Forward 8, Down 7, Down 2, Up 3, Down 9, Down 7, Down 8, Forward 4, Forward 3, Forward 9, Forward 6, Up 7, Forward 5, Down 4, Down 5, Forward 6, Up 9, Down 6, Down 7, Down 8, Down 9, Down 4, Up 5, Down 4, Forward 5, Forward 3, Down 3, Down 7, Up 8, Forward 5, Down 8, Down 1, Down 6, Down 9, Up 4, Up 1, Down 8, Down 3, Down 8, Up 4, Down 7, Down 6, Forward 7, Up 9, Down 4, Down 1, Down 6, Down 2, Forward 7, Down 2, Down 7, Forward 3, Forward 6, Up 2, Down 4, Up 1, Forward 4, Up 2, Down 4, Up 3, Down 8, Up 9, Forward 8, Down 5, Down 4, Forward 8, Down 1, Down 8, Forward 3, Down 4, Forward 5, Down 5, Up 9, Forward 1, Down 9, Down 1, Forward 4, Forward 9, Up 1, Forward 4, Forward 2, Down 9, Down 1, Forward 1, Down 2, Forward 2, Down 5, Up 4, Up 7, Down 8, Forward 3, Up 1, Down 4, Forward 5, Up 2, Forward 4, Forward 2, Down 1, Forward 4, Forward 1, Up 5, Forward 8, Forward 4, Forward 1, Down 6, Down 7, Up 4, Forward 9, Up 6, Forward 9, Forward 3, Forward 2, Down 2, Forward 7, Up 7, Forward 7, Down 4, Down 6, Forward 8, Up 8, Forward 1, Forward 3, Forward 3, Up 8, Down 1, Up 9, Forward 1, Up 1, Down 7, Forward 7, Up 5, Forward 5, Up 9, Up 3, Forward 2, Forward 6, Up 1, Forward 5, Up 1, Down 3, Down 5, Forward 8, Up 5, Forward 1, Down 8, Forward 4, Down 3, Down 1, Down 7, Forward 7, Forward 3, Down 4, Forward 9, Up 5, Down 2, Forward 4, Up 7, Up 5, Forward 3, Down 7, Forward 8, Up 4, Up 2, Forward 2, Down 9, Forward 4, Down 5, Forward 2, Down 9, Down 5, Up 7, Forward 3, Down 8, Forward 7, Forward 9, Down 5, Forward 2, Forward 9, Forward 5, Down 5, Up 4, Down 6, Down 6, Forward 8, Forward 5, Forward 4, Down 5, Forward 2, Down 8, Forward 9, Down 1, Down 8, Down 7, Up 9, Down 7, Down 1, Forward 7, Forward 9, Forward 8, Up 2, Forward 6, Down 3, Down 6, Up 4, Forward 4, Forward 5, Down 9, Down 5, Forward 1, Down 2, Forward 1, Down 1, Up 1, Up 7, Up 5, Forward 6, Forward 3
    ]
