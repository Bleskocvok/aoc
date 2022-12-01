#!/usr/bin/env runghc

module Main where

import AoCUtils ( getLines )
import Data.Char ( digitToInt )
import Control.Monad.RWS ( RWS, get, when, MonadReader (ask, local), modify, evalRWS )


mostCommon :: [Char] -> Char
mostCommon xs
    | count '0' xs  > length xs `div` 2 = '1'
    | otherwise = '0'


getNthBits :: Int -> [String] -> [Char]
getNthBits n ls = flip (!!) n <$> ls


invert :: [Char] -> [Char]
invert = map inv
    where inv ch = case ch of
                    '0' -> '1'
                    '1' -> '0'


count :: Char -> String -> Int
count ch = length . filter (ch ==)


binToDec :: [Char] -> Int
binToDec =
    let
        btd n [] = 0
        btd n (x : xs) = x * 2^n + btd (n + 1) xs
    in
        btd 0 . reverse . map digitToInt


findBinNum :: [String] -> String
findBinNum ls =
    let
        len = length $ head ls
        func n = mostCommon . getNthBits n
    in
        foldr (\n -> (func n ls :)) "" [0 .. len - 1]


solve1 :: [String] -> (Int, Int)
solve1 ls = (binToDec str, binToDec $ invert str)
    where str = findBinNum ls


findRating :: (Int -> Int -> Bool)
           -> RWS [String]  -- read allowed bin numbers
                  ()   -- write nothing
                  Int  -- current bit
                  String  -- resulting number

findRating cmp = do

    ls <- ask
    bit <- get

    case ls of
        [x] -> pure x
        _ -> do
            let cur = getNthBits bit ls
                ones = count '1' cur
                zeroes = count '0' cur

            let crit = if cmp zeroes ones then '0' else '1'
                keepCrit = filter ((crit ==) . (!! bit))

            modify (+ 1)
            local keepCrit (findRating cmp)


main :: IO ()
main = do
    ls <- getLines "input1.txt"
    let (a, b) = solve1 ls
    print $ show a ++ " " ++ show b
    print $ a * b

    ls2 <- getLines "input2.txt"
    let (c, d) = solve1 ls2
    print $ show c ++ " " ++ show d
    print $ c * d

    let get cmp ll = binToDec $ fst $ evalRWS (findRating cmp) ll 0

    let ox1 = get (>) ls
        co1 = get (<=) ls

    print (ox1, co1)
    print $ ox1 * co1

    let ox2 = get (>) ls2
        co2 = get (<=) ls2

    print (ox2, co2)
    print $ ox2 * co2
