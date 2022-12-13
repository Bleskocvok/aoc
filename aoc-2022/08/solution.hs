#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines )

import Data.Array ( (!), (//), bounds, indices, listArray, Array )


type Map = Array (Int, Int) Int


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    m <- readMap fileIn
    let hm = calculateMaxima m
    printMap $ transform m hm
    print $ length $ filter id $ collect (isVisible hm) m


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    m <- readMap fileIn
    let hm = calculateMaxima m
    print $ maximum $ collect viewScore m


main :: IO ()
main = putStrLn "day 08" >> putStrLn "\nfirst"  >> fstHalf "input1.txt"
                         >> putStrLn "\nsecond" >> sndHalf "input1.txt"


-- SECOND HALF


viewScore :: Map -> (Int, Int) -> Int
viewScore m (x, y) = foldr (*) 1 [visibleLine (0, 1), visibleLine (0, -1),
                                  visibleLine (1, 0), visibleLine (-1, 0)]
    where
        visibleLine d@(dx, dy) =
            let
                (_, c, _) = foldTreeMap m (x + dx, y + dy) d (m ! (x, y), 0, True) count
            in
                c
        count _ h acc@(maxH, c, cont)
            | cont = (max h maxH,
                      if h <= maxH then c + 1 else c,
                      cont && h < maxH)
            | otherwise = acc


-- FIRST HALF


calculateMaxima :: Map -> (Map, Map, Map, Map)
calculateMaxima m = (m // a, m // b, m // c, m // d)
    where
        ((lx, ly), (hx, hy)) = bounds m
        (_, a) = foldOver ( 1,  0) [ (lx,  y) | y <- [ly..hy] ]
        (_, b) = foldOver (-1,  0) [ (hx,  y) | y <- [ly..hy] ]
        (_, c) = foldOver ( 0,  1) [ ( x, ly) | x <- [lx..hx] ]
        (_, d) = foldOver ( 0, -1) [ ( x, hy) | x <- [lx..hx] ]
        foldOver dir = foldr
                (\xy (_, acc) -> (foldTreeMap m xy dir (-1, acc) foldMaximum))
                (0, [])


foldMaximum :: (Int, Int) -> Int -> (Int, [((Int, Int), Int)])
                                 -> (Int, [((Int, Int), Int)])
foldMaximum (x, y) h (maxH, acc) = (max h maxH, ((x, y), maxH) : acc)


isVisible :: (Map, Map, Map, Map) -> Map -> (Int, Int) -> Bool
isVisible (a, b, c, d) m ix = m ! ix > minimum [a ! ix, b ! ix, c ! ix, d ! ix]


transform :: Map -> (Map, Map, Map, Map) -> Map
transform m hm = m // foldr (\i -> ((i, -1 + fromEnum (isVisible hm m i)) :))
                            []
                            (indices m)


-- UTILS


linesToMap :: [[Int]] -> Map
linesToMap xs = listArray ((0, 0), (w-1, h-1)) $ concat xs
    where
        w = if null xs then 0 else length $ xs !! 0
        h = length xs


readMap :: FilePath -> IO Map
readMap f = linesToMap . map (map readChar) <$> getLines f
    where
        readChar :: Char -> Int
        readChar c = read [c]


-- “fold” over a line in the 2d array
foldTreeMap :: Map                              -- the map
            -> (Int, Int)                       -- position
            -> (Int, Int)                       -- vector
            -> a                                -- beginning value
            -> ((Int, Int) -> Int -> a -> a)    -- folding function
            -> a                                -- result
foldTreeMap m (x, y) (dx, dy) acc f
    | inBounds (bounds m) = foldTreeMap m (x + dx, y + dy) (dx, dy) next f
    | otherwise = acc
    where
        inBounds ((lx, ly), (hx, hy)) = not (x < lx || y < ly ||
                                             x > hx || y > hy)
        next = f (x, y) (m ! (x, y)) acc


printMap :: Map -> IO ()
printMap m = putStrLn $ concat [ digit ++ end | y <- [ly..hy],
                                                x <- [lx..hx],
                                  let digit = if m ! (x, y) < 0
                                              then "."
                                              else show (m ! (x, y)),
                                  let end = if x == hx
                                            then "\n"
                                            else ""]
    where
        ((lx, ly), (hx, hy)) = bounds m


collect :: (Map -> (Int, Int) -> a) -> Map -> [a]
collect f m = foldr (\i acc -> f m i : acc) [] (indices m)
