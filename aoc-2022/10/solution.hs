#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines )

import Control.Monad ( forM_ )
import Data.Array ( (!), listArray, Array )


data Instruction = AddX Int | NoOp deriving ( Eq, Show )
type Cycles = Array Int Int


readInst :: String -> Instruction
readInst "noop" = NoOp
readInst ('a' : 'd' : 'd' : 'x' : ' ' : xs) = AddX $ read xs
readInst _ = error "readInst: invalid instruction"


eval :: [Instruction] -> Cycles
eval = mkArray . concat . eval1 1
    where
        mkArray xs = listArray (1, length xs) xs
        eval1 r (NoOp : xs) = [r] : eval1 r xs
        eval1 r (AddX x : xs) = [r, r + x] : eval1 (r + x) xs
        eval1 _ [] = []


getImportantCycles :: Cycles -> [Int] -> [Int]
getImportantCycles cycles = foldr ((:) . (cycles !) . (+ (-1))) []


visible :: Int -> Int -> Bool
visible row sprite = abs (row - sprite) <= 1


draw :: Cycles -> IO ()
draw cyc = forM_ [ [ c | x <- [0..39], let c = pixel x (sprite x y)
                   ] | y <- [0..5]
                 ] putStrLn
    where
        rows = 6
        cols = 40
        sprite x y = if x == 0 && y == 0 then 1 else (cyc ! (y * cols + x))
        pixel x = (".#" !!) . fromEnum . visible x


getCycles :: FilePath -> IO Cycles
getCycles f = eval . (readInst `map`) <$> getLines f



fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    cycles <- getCycles fileIn
    let times = [20, 60 .. 220]
        signal = getImportantCycles cycles times
        xs = zipWith (*) times signal
        result = sum xs
    print result


sndHalf :: FilePath -> IO ()
sndHalf fileIn = getCycles fileIn >>= draw


main :: IO ()
main = putStrLn "day 10" >> putStrLn "\nfirst"  >> fstHalf "example.txt"
                                                >> putStrLn ""
                                                >> fstHalf "input1.txt"
                         >> putStrLn "\nsecond" >> sndHalf "example.txt"
                                                >> putStrLn ""
                                                >> sndHalf "input1.txt"
