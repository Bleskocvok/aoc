#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines )

import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Sequence ( ViewL( (:<) ), (|>) )
import Data.Bifunctor ( Bifunctor( first, second ) )
import Control.Monad.State ( State, forM_, gets, modify, execState )
import Data.Array ( Ix( inRange ), Array, ( ! ), bounds, indices, listArray )
import Data.Char ( ord, chr )
import Data.List ( find, elemIndex, transpose )
import Data.Maybe ( isJust, fromJust )


type Point = (Int, Int)
data Map = Map { heights :: Array Point Int
               , start   :: Point
               , end     :: Point }
               deriving ( Show )


at :: Map -> Point -> Int
at = (!) . heights


inBounds :: Map -> Point -> Bool
inBounds = inRange . bounds . heights


draw :: Map -> IO ()
draw m = forM_ lines putStrLn
    where
        maxW = fst . snd . bounds . heights $ m
        maxH = snd . snd . bounds . heights $ m
        lines = [ [ (drawC (x, y)) | x <- [0 .. maxW] ] | y <- [0 .. maxH] ]
        drawC (x, y)
            | (x, y) == start m = 'S'
            | (x, y) == end   m = 'E'
            | otherwise = chr $ m `at` (x, y) + ord 'a'


parseMap :: [String] -> Map
parseMap ls = Map { heights = listArray ixs (concat $ transpose hs)
                  , start = coordsOf 'S' ls
                  , end   = coordsOf 'E' ls }
    where
        ixs = ((0, 0), (w-1, h-1))
        hs = parseHeights ls
        h = length hs
        w = length $ hs !! 0
        parseHeights = map (map cToH)
        cToH c
            | c >= 'a' && c <= 'z' = ord c - ord 'a'
            | c == 'S' = 0
            | c == 'E' = ord 'z' - ord 'a'
            | otherwise = error $ "invalid character '" ++ show c ++ "'"
        coordsOf c ls =
            let
                mbyIdx = find (isJust . snd) . zip [0..] . map (elemIndex c)
            in
                case mbyIdx ls of
                    Just (y, Just x) -> (x, y)
                    _                -> error $ "'" ++ show c ++ "' not found"


bfs :: Map -> State (S.Seq (Point, Int), M.Map Point Int) ()
bfs m = do
    seq <- gets $ S.viewl . fst
    case seq of
        S.EmptyL -> pure ()
        (nxt, step) :< rest -> do
            modify $ first $ const rest
            let curHeight = m `at` nxt
            forM_ (neighbors nxt) (visit step curHeight)
            bfs m
    where
        neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        visit step h (x, y) = do
            visited <- gets $ M.member (x, y) . snd
            if not visited
                && inBounds m (x, y)
                && (m `at` (x, y) + 1) >= h
            then do
                modify $ second $ M.insert   (x, y) (step + 1)
                modify $ first  $ flip (|>) ((x, y), step + 1)
            else
                pure ()


readMap :: FilePath -> IO Map
readMap f = parseMap <$> getLines f


paths :: Point -> Map -> M.Map Point Int
paths to m = snd $ execState (bfs m) (S.singleton (to, 0),
                                      M.singleton  to  0)


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    m <- readMap fileIn
    let len = paths (end m) m M.! start m
    draw m
    print len


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    m <- readMap fileIn
    let notEndAndZero ix = 0 == m `at` ix && ix /= end m
        starts = filter notEndAndZero (indices . heights $ m)
        ps = paths (end m) m
        lens = map fromJust $ filter isJust $ map (ps M.!?) starts
    print $ minimum lens


main :: IO ()
main = putStrLn "day 12" >> putStrLn "\nfirst"  >> fstHalf "example.txt"
                                                >> fstHalf "input2.txt"
                         >> putStrLn "\nsecond" >> sndHalf "example.txt"
                                                >> sndHalf "input2.txt"


