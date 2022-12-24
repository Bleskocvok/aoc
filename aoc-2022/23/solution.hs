#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines )

import Data.Array ( Ix( inRange ), Array, elems, (!), (//), bounds, listArray )
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Semigroup ( Max( Max, getMax ), Min( Min, getMin ) )
import Data.List ( find, findIndex, group, sort )
import Data.Bool ( bool )
import Control.Monad ( liftM, (<$!>) )
import Data.Functor ( (<&>) )


type Point     = (Int, Int)
type Direction = (Int, Int)
type Elves     = [Point]
type Rectangle = Array (Int, Int) Bool


liftPair :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
liftPair op (a, b) = bimap (op a) (op b)

add, sub, mul :: (Int, Int) -> (Int, Int) -> (Int, Int)
add = liftPair (+)
sub = liftPair (-)
mul = liftPair (*)


makeRect :: Elves -> Rectangle
makeRect es = listArray ((lx, ly), (hx, hy)) (repeat False)
              // zip es (repeat True)
    where
        get f g = bimap f f . foldMap (bimap g g)
        (!lx, !ly) = get getMin Min es
        (!hx, !hy) = get getMax Max es


showRect :: Rectangle -> String
showRect ar = foldl showLine [] [ly..hy]
    where
        ((lx, ly), (hx, hy)) = bounds ar
        chrs = ((".#" !!) . fromEnum) <$> ar
        showLine str y = str ++ map (chrs !) (zip [lx..hx] (repeat y)) ++ "\n"


parseLine :: Int -> String -> Elves
parseLine y = map (\(x, _) -> (x, y)) . filter snd . zip [0..] . map ('#' ==)


parseElves :: [String] -> Elves
parseElves = mconcat . zipWith parseLine [0..]


occupied :: Rectangle -> Point -> Bool
occupied rect ix | inRange (bounds rect) ix = rect ! ix
                 | otherwise = False


allowed :: Rectangle -> Point -> Direction -> Bool
allowed rect pt (dx, dy) = not ( occupied rect (coord (-1))
                              || occupied rect (coord   0)
                              || occupied rect (coord   1) )
    where
        coord v = pt `add` (dx, dy) `add` nxt v
        nxt x = (x, x) `mul` ( (1, 1) `sub` (abs dx, abs dy) )


runRound :: Elves -> [Direction] -> Elves
runRound es dirs = zipWith (\m o -> bool m o $ occupied blocked m) moves origs
    where
        rect = makeRect es

        origs = es
        moves = map (elfMove dirs) es

        targets = map head . filter ((>= 2) . length) . group . sort $ moves

        blocked = rect // (zip targets (repeat True))

        elfMove dirs pt = case find (allowed rect pt) dirs of
            Nothing  -> pt
            Just dir -> if all (allowed rect pt) dirs
                        then pt
                        else pt `add` dir


foldRounds :: (Elves -> a -> a) -> a -> Int -> Elves -> [Direction] -> (Elves,a)
foldRounds _ acc 0 es _    = (es, acc)
foldRounds f acc i es dirs = foldRounds f (f next acc) (i-1) next nextDirs
    where
        (next, nextDirs) = fullRound es dirs


fullRound :: Elves -> [Direction] -> (Elves, [Direction])
fullRound es dirs = (runRound es dirs, rotate dirs)
    where
        rotate (x : xs) = xs ++ [x]
        rotate [] = error "rotate: empty list"


converges :: Int -> Elves -> [Direction]-> Int
converges i es dirs | es == next = i
                    | otherwise = converges (i + 1) next nextDirs
    where
        (!next, !nextDirs) = fullRound es dirs


getStates :: Int -> Elves -> [Elves]
getStates i es = let dirs = [ (0, -1), (0, 1), (-1, 0), (1, 0) ] 
                 in  reverse $ snd $ foldRounds (:) [] i es dirs


fstHalf :: FilePath -> IO ()
fstHalf f = do
    es <- parseElves <$> getLines f

    let printRect = putStr . showRect . makeRect
        sts = getStates 10 es
        spaces = length . filter not . elems . makeRect . last $ sts

    -- forM_ sts (\state -> putStrLn "" >> printRect state)
    print spaces


sndHalf :: FilePath -> IO ()
sndHalf f = (parseElves <$!> getLines f)
    >>= (print . flip (converges 2) [ (0, -1), (0, 1), (-1, 0), (1, 0) ] )


main :: IO ()
main = putStrLn "day 23" >> putStrLn "\nfirst"  >> fstHalf "example.txt"
                                                >> fstHalf "input1.txt"
                         >> putStrLn "\nsecond" >> sndHalf "example.txt"
                                                >> sndHalf "long.txt"

