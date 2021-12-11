#!/usr/bin/env runghc

module Main where

import AoCUtils ( getLines )
import Data.Array ( Array, (!), assocs, bounds, listArray )
import qualified Data.Set as S
import Control.Monad.State.Lazy ( forM_, modify, execState, State, get )
import Data.List ( foldl', sortOn )
import qualified Data.Ord


type HeightMap = Array (Int, Int) Int
data LowPoint = LowPoint { getX :: Int,
                           getY :: Int,
                           getH :: Int }
                deriving ( Show, Eq )


parseMap :: [String] -> HeightMap
parseMap ls = listArray ((0, 0), (h - 1, w - 1)) numlst
    where
        numlst = concatMap mapLine ls
        mapLine = map (read . (: []) :: Char -> Int)
        w = length $ head ls
        h = length ls


neighbors :: HeightMap -> (Int, Int) -> [(Int, Int)]
neighbors hm (y, x) = filterOut hm
                        [ (y - 1, x),
                          (y + 1, x),
                          (y,     x - 1),
                          (y,     x + 1) ]

filterOut :: HeightMap -> [(Int, Int)] -> [(Int, Int)]
filterOut hm = filter (\(y', x') -> x' >= lx
                            && y' >= ly
                            && x' <= hx
                            && y' <= hy)
    where
        ((ly, lx), (hy, hx)) = bounds hm


findLowPoints :: HeightMap -> [LowPoint]
findLowPoints hm = foldr accumLs [] (assocs hm)
    where
        accumLs ((y, x), d) acc
            | null $ neighbors hm (y, x) = acc
            | d < lowest = LowPoint x y d : acc
            | otherwise = acc
            where
                lowest = minimum $ (hm !) `map` neighbors hm (y, x)


basinFinder :: HeightMap -> (Int, Int) -> State (S.Set (Int, Int)) ()
basinFinder hm (y, x) = do
    visited <- get
    if hm ! (y, x) == 9 || (y, x) `S.member` visited then
        pure ()
    else do
        modify $ S.insert (y, x)
        let around = filterOut hm $ neighbors hm (y, x)
        forM_ around (basinFinder hm)

findBasin :: LowPoint -> HeightMap -> [(Int, Int)]
findBasin lp hm = S.toList $ execState (basinFinder hm $ lpToPair lp) S.empty
    where
        lpToPair p = (getY p, getX p)


sumRisk :: [LowPoint] -> Int
sumRisk = sum . map ((1 + ) . getH)


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    ls <- getLines fileIn
    let hm = parseMap ls
        lps = findLowPoints hm
    print $ sumRisk lps
    pure ()


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    let hm = parseMap ls
        lps = findLowPoints hm
        basins = foldl' (\a -> (: a) . flip findBasin hm) [] lps
        top3 = take 3 $ sortOn (Data.Ord.Down . length) basins
    print $ map length top3
    print $ product $ length <$> top3
    pure ()


main :: IO ()
main = do
    putStrLn "day 09"
    fstHalf "input1.txt"
    fstHalf "input2.txt"
    sndHalf "input1.txt"
    sndHalf "input2.txt"


