#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines )

import Data.Array
import Data.List
import Data.Maybe
import Data.Bool
import Data.Bifunctor
import Control.Monad
import qualified Data.Set as S

import Debug.Trace


data Blizzard = X | N | S | W | E  -- blocked, up, down, left, right
              deriving ( Eq )

type Blizzards = Array (Int, Int) [Blizzard]

data Map = Map { blizzards :: Blizzards
               , from :: (Int, Int)
               , dest :: (Int, Int) }

type Visited = Array (Int, Int) Bool


instance Show Blizzard where
    show b = case b of X -> "#"; N -> "^"; S -> "v"; W -> "<"; E -> ">"


instance Show Map where
    show = showMap . blizzards


showMap :: Blizzards -> String
showMap m = foldl showLine [] [ly..hy]
    where
        ((lx, ly), (hx, hy)) = bounds m
        showLine str y = str ++ map (chrs !) (zip [lx..hx] (repeat y)) ++ "\n"
        chrs = showElem <$> m
        showElem []  = '.'
        showElem [x] = head . show $ x
        showElem xs  = head . show . length $ xs


showArray2d :: Show a => Array (Int, Int) a -> String
showArray2d a = foldl showLine [] [ly..hy]
    where
        ((lx, ly), (hx, hy)) = bounds a
        showLine str y = str ++ map (chrs !) (zip [lx..hx] (repeat y)) ++ "\n"
        chrs = head . show <$> a


parseMap :: [String] -> Map
parseMap lines = case lines of
    [] -> error "parseMap: empty input"
    ls ->
        let
            start = (findDot . head $ ls, 0)
            end   = (findDot . last $ ls, snd $ getBounds xs)
            xs = map (map parseOne) ls
            blizz = listArray ((0, 0), getBounds xs) . concat . transpose $ xs
        in
            Map blizz start end
    where
        findDot = fromJust . elemIndex '.'
        parseOne c = case c of '.' -> []
                               '#' -> [X]
                               '^' -> [N]
                               'v' -> [S]
                               '<' -> [W]
                               '>' -> [E]
        getBounds xs = (length (head xs) - 1, length xs - 1)


apply :: (Blizzards -> Blizzards) -> Map -> Map
apply f (Map blizz a b) = Map (f blizz) a b


next :: Blizzards -> Blizzards
next bs = accum oneNext bs zixs
    where
        ixs = indices bs
        zixs = zip ixs ixs

        oneNext _ ix | bs ! ix == [X] = [X]
                     | otherwise = concat [ getDir ix ( 0,  1) N
                                          , getDir ix ( 0, -1) S
                                          , getDir ix ( 1,  0) W
                                          , getDir ix (-1,  0) E ]

        get ix b = filter (b ==) $ bs ! wrapIdx ix

        getDir (x, y) (dx, dy) b
            | bs ! moved == [X] = getDir moved (dx, dy) b
            | otherwise         = get moved b
            where
                moved = wrapIdx (x + dx, y + dy)

        wrapIdx (x, y) = (wrap' 0 hx x, wrap' 0 hy y)
            where
                wrap' v0 vmax v | v > vmax  = v0
                                | v < v0    = vmax
                                | otherwise = v
                (_, (hx, hy)) = bounds bs


bfs :: Int -> Map -> Visited -> Int
bfs i m vis | visited ! dest m = i - 1
            | otherwise = bfs (i + 1) (next `apply` m) nextVis
    where
        visited = vis
        -- visited = trace (showArray2d $ fromEnum <$> vis) vis

        (_, (hx, hy)) = bounds . blizzards $ m
        isVisited (x, y) | x < 0  || y < 0  = False
                         | x > hx || y > hy = False
                         | otherwise        = vis ! (x, y)
        nextVis = accum accFun vis (zip ix ix)
        ix = indices vis
        accFun el (x, y) = null (blizzards m ! (x, y))
                        && (el || isVisited (x - 1, y)
                               || isVisited (x + 1, y)
                               || isVisited (x, y + 1)
                               || isVisited (x, y - 1))


runBfs :: Map -> Int
runBfs m = bfs 0 m $ (False <$ blizzards m) // [(from m, True)]


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    m <- parseMap <$> getLines fileIn
    let result = runBfs m
    print m
    print result


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    m <- parseMap <$> getLines fileIn
    let there = runBfs m
        m'    = switch . nexts (there + 1) $ m
        back  = runBfs m'
        m''   = switch . nexts (back + 1)  $ m'
        thereAgain = runBfs m''
    print [there, back, thereAgain]
    print $ there + back + thereAgain
    pure ()
    where
        switch (Map bs fr to) = Map bs to fr
        nexts n = last . take n . iterate (apply next)


main :: IO ()
main = putStrLn "day 24" >> putStrLn "\nfirst"  >> fstHalf "example.txt"
                                                >> fstHalf "input1.txt"
                         >> putStrLn "\nsecond" >> sndHalf "example.txt"
                                                >> sndHalf "input1.txt"
