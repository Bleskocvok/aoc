#!/usr/bin/env runghc

module Main where

import AoCUtils ( getLines )
import Data.Text ( pack, unpack, splitOn )
import Text.Printf ( printf )
import Data.List ( foldl' )
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Array ( Array, listArray, elems, (!), (//), accum )
import Control.Monad ( when )


type Field = Array Int (Array Int Int)

type Line = (Int, Int, Int, Int)


oneLine :: String -> Line
oneLine l =
    let
        halves = splitOn (pack "->") (pack l)
        bits = unpack `map` concatMap (splitOn $ pack ",") halves
    in
        (read $ head bits, read $ bits !! 1,
         read $ bits !! 2, read $ bits !! 3)


parseLines :: [String] -> [Line]
parseLines = reverse . foldl' (\a -> (: a) . oneLine) []


filterAligned :: [Line] -> [Line]
filterAligned = filter (\(x1, y1, x2, y2) -> x1 == x2 || y1 == y2)


fieldSize :: [Line] -> (Int, Int)
fieldSize = bimap (+ 1) (+ 1) . foldr getMaxs (0, 0)
    where
        getMaxs (x1, y1, x2, y2) = bimap (max x1 . max x2)
                                         (max y1 . max y2)


mkField :: (Int, Int) -> Field
mkField (w, h) = listArray (0, h - 1) $ replicate h line
    where line = listArray (0, w - 1) $ replicate w 0


pprintField :: Field -> String
pprintField = concat . elems . (printLine <$>)
    where
        printLine = (++ "\n") . concat . elems . (printOne <$>)
        printOne 0 = " ."
        printOne n = printf "%2d" n


allCoords :: Line -> [(Int, Int)]
allCoords (x1, y1, x2, y2)
    | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
    | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
    | abs (x1 - x2) == abs (y1 - y2) = 
                 [(x1 + dx * i, y1 + dy * i) | let dx = signum $ x2 - x1,
                                               let dy = signum $ y2 - y1,
                                               i <- [0 .. abs $ x1 - x2]]
    | otherwise = error "not implemented"


addLine :: Line -> Field -> Field
addLine l f = foldr putAt f (allCoords l)
    where
        -- okay, this is horrible
        putAt (x, y) fld = fld // [ (y, fld ! y // [(x, fld ! y ! x + 1)]) ]


addAll :: Field -> [Line] -> Field
addAll = foldr addLine


countOverlap :: Field -> Int
countOverlap = length . filter (>= 2) . concat . (elems <$>) . elems


fstHalf :: FilePath -> Bool -> IO ()
fstHalf fileIn showFld = do
    ls <- getLines fileIn
    let lines = parseLines ls
        hv = filterAligned lines
        field = mkField $ fieldSize hv
        withLines = addAll field hv
        overlap = countOverlap withLines
    when showFld $ putStrLn $ pprintField withLines
    print overlap
    pure ()


sndHalf :: FilePath -> Bool -> IO ()
sndHalf fileIn showFld = do
    ls <- getLines fileIn
    let lines = parseLines ls
        hv = lines -- no filtering here
        field = mkField $ fieldSize hv
        withLines = addAll field hv
        overlap = countOverlap withLines
    when showFld $ putStrLn $ pprintField withLines
    print overlap
    pure ()


main :: IO ()
main = do
    putStrLn "day 05"
    fstHalf "input1.txt" True
    fstHalf "input2.txt" False
    sndHalf "input1.txt" True
    sndHalf "input2.txt" False


