#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines )

import Data.Array
import Data.List

import Control.Monad

import Debug.Trace

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


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    inst <- (readInst `map`) <$> getLines fileIn
    let cycles = eval inst
        times = [20, 60 .. 220]
        signal = getImportantCycles cycles times
        xs = zipWith (*) times signal
        result = sum xs
    print result


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    pure ()


main :: IO ()
main = putStrLn "day 10" >> putStrLn "\nfirst"  >> fstHalf "input1.txt"
                                                >> fstHalf "input2.txt"
                         >> putStrLn "\nsecond" >> sndHalf "input1.txt"
                                                >> sndHalf "input2.txt"
