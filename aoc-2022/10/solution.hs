#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines )

import Data.Array
import Data.List

import Control.Monad

import Debug.Trace

data Instruction = AddX Int | NoOp deriving ( Eq, Show )

type Plan = [(Int, Int)] -- (t, ±delta)
type Cycles = Array Int Int


readInst :: String -> Instruction
readInst "noop" = NoOp
readInst ('a' : 'd' : 'd' : 'x' : ' ' : xs) = AddX $ read xs
readInst _ = error "readInst: invalid instruction"


makePlan :: [Instruction] -> Plan
makePlan = sortOn fst . zipWith func [1 .. 220] . cycle
    where
        func t NoOp = (t, 0)
        func t (AddX x) = (t + 1, x)


planToCycles :: Plan -> Cycles
planToCycles plan = listArray (1, length lst) lst
    where
        lst = mkList 1 1 plan
        mkList :: Int -> Int -> Plan -> [Int]
        mkList _ _ [] = []
        mkList t reg plan@((dt, delta) : xs)
            | t == dt = let nxt = reg + delta
                        in nxt : mkList t nxt xs
            | otherwise =  reg : mkList (t + 1) reg plan


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    inst <- (readInst `map`) <$> getLines fileIn
    let cycles = planToCycles $ makePlan inst
        times = [20, 60 .. 220]
        len = snd $ bounds cycles
        signal = foldr (\i acc -> cycles ! (i `mod` len) : acc) [] times
        xs = zipWith (*) times signal
        result = sum xs
    forM_ (makePlan inst) print
    print result
    -- forM_ (inst) print
    forM_ [20, 60, 100, 140, 180, 220] (print . (cycles !))
    -- forM_ cycles print
    -- putStrLn ""
    -- putStrLn ""


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    pure ()


main :: IO ()
main = putStrLn "day 10" >> putStrLn "\nfirst" -- >> fstHalf "input1.txt"
                                                >> fstHalf "input2.txt"
                         >> putStrLn "\nsecond" >> sndHalf "input1.txt"
                                                >> sndHalf "input2.txt"
