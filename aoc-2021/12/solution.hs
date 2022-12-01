#!/usr/bin/env runghc

module Main where

import AoCUtils ( getLines )
import Data.List.Split ( splitOn )
import Data.List ( foldl', group, sort )
import Data.Char ( isUpper, isLower )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Maybe ( fromJust )


type CaveGraph = Map.Map String [String]
type Path = [String]


parseLine :: String -> CaveGraph -> CaveGraph
parseLine line = insertPath b a . insertPath a b
    where
        split = splitOn "-" line
        (a : b : _) = split


insertPath :: String -> String -> CaveGraph -> CaveGraph
insertPath a b = Map.insertWith (++) a [b]


parseCave :: [String] -> CaveGraph
parseCave = foldl' (flip parseLine) Map.empty


smallCave :: String -> Bool
smallCave = all isLower


bigCave :: String -> Bool
bigCave = all isUpper


getPaths :: (String -> Path -> Bool)
        -> String
        -> String
        -> Path
        -> CaveGraph
        -> State (Set.Set Path) ()
getPaths pred curr end acc cave
    | end == curr = modify' $ Set.insert (curr : acc)
    | otherwise = do
        if pred curr acc
        then do
            let nxtLst = fromJust $ Map.lookup curr cave
            forM_ nxtLst (\c -> getPaths pred c end (curr : acc) cave)
        else
            pure ()


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    ls <- getLines fileIn
    let cave = parseCave ls
        pred curr path = bigCave curr || curr `notElem` path
        paths = execState (getPaths pred "start" "end" [] cave) Set.empty
    -- print paths
    print $ Set.size paths
    pure ()


count :: (a -> Bool) -> [a] -> Int
count p = length . filter p


sndPred :: String -> Path -> Bool
sndPred curr path = bigCave curr
                    || count (curr ==) path == 0
                    || (noSmallTwice path && count (curr ==) path == 1)
                    && curr /= "start"


noSmallTwice :: Path -> Bool
noSmallTwice p = length (noDups small) == length small
    where
        noDups = map head . group . sort
        small = filter smallCave p


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    let cave = parseCave ls
        paths = execState (getPaths sndPred "start" "end" [] cave) Set.empty
    print $ Set.size paths
    pure ()


main :: IO ()
main = do
    putStrLn "day 12"
    putStrLn "\nfirst"
    fstHalf "input1.txt"
    fstHalf "input2.txt"
    putStrLn "\nsecond"
    sndHalf "input1.txt"
    sndHalf "input2.txt"


