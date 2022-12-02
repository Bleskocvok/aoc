#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines )


data RPS = Rock | Paper | Scissors deriving ( Eq )


parseRPS :: String -> (RPS, RPS)
parseRPS (a : ' ' : c : _) = (from a, to c)
    where
        from 'A' = Rock
        from 'B' = Paper
        from 'C' = Scissors
        from _ = error "parseRPS: invalid format"
        to 'X' = Rock
        to 'Y' = Paper
        to 'Z' = Scissors
        to _ = error "parseRPS: invalid format"
parseRPS _ = error "parseRPS: invalid format"


score :: RPS -> RPS -> Int
score a b = victory a b * 3 + 3
    + case b of
        Rock -> 1
        Paper -> 2
        Scissors -> 3


victory :: RPS -> RPS -> Int
victory a b
    | a == b = 0
    | otherwise = case (a, b) of
        (Rock,     Scissors) -> -1
        (Paper,    Rock)     -> -1
        (Scissors, Paper)    -> -1
        _                    ->  1


altScore :: RPS -> RPS -> Int
altScore a b = case b of
    -- lose
    Rock     -> score a (findPlay a (-1))
    -- draw
    Paper    -> score a (findPlay a 0)
    -- win
    Scissors -> score a (findPlay a 1)


findPlay :: RPS -> Int -> RPS
findPlay a i = foldr func Rock [Rock, Paper, Scissors]
    where
        func b prev
            | victory a b == i = b
            | otherwise        = prev


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    ls <- getLines fileIn
    let result = sum $ (uncurry score . parseRPS) `map` ls
    print result


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    let result2 = sum $ (uncurry altScore . parseRPS) `map` ls
    print result2


main :: IO ()
main = do
    putStrLn "day 02"
    
    putStrLn "\nfirst"
    fstHalf "input1.txt"
    
    putStrLn "\nsecond"
    sndHalf "input1.txt"


