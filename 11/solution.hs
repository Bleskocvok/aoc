#!/usr/bin/env runghc

module Main where

import AoCUtils ( getLines )
import Data.Array
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad
import Data.List


type Grid = Array (Int, Int) Int


parseGrid :: [String] -> Grid
parseGrid = listArray ((0, 0), (9, 9)) . concatMap parseLine
    where
        parseLine = map (read . (: "") :: Char -> Int)


(👉) :: Monad m => m a -> (a -> m b) -> m b
(👉) = (>>=)


propagate :: (Int, Int) -> Grid -> State Int Grid
propagate (y, x) grid
    | y > w' || x > h' || x < 0 || y < 0 = pure grid
    | curr == 9 = do
        used <- get
        modify (+ 1)
        allDirections new
    | otherwise = pure new
    where
        (w', h') = snd $ bounds grid
        curr = grid ! (y, x)
        new = grid // [((y, x), curr + 1)]
        allDirections s =
               propagate (y,     x + 1) s
            👉 propagate (y,     x - 1)
            👉 propagate (y + 1, x)
            👉 propagate (y - 1, x)
            👉 propagate (y + 1, x + 1)
            👉 propagate (y + 1, x - 1)
            👉 propagate (y - 1, x + 1)
            👉 propagate (y - 1, x - 1)


oneStep :: Grid -> (Int, Grid)
oneStep grid = (snd res, zeroed)
    where
        apply = foldM (flip propagate) grid (indices grid)
        res = runState apply 0
        zeroed = wrap <$> fst res
        wrap x | x > 9 = 0
               | otherwise = x


moreSteps :: Int -> Grid -> (Int, Grid)
moreSteps 0 g = (0, g)
moreSteps i g = (n + acc, fin)
    where
        (n, new) = oneStep g
        (acc, fin) = moreSteps (i - 1) new


firstFlash :: Grid -> (Int, Grid)
firstFlash = firstFlash' 0
    where
        firstFlash' acc g
            | all (0 ==) $ elems g = (,) acc g
            | otherwise = firstFlash' (acc + 1) $ snd $ oneStep g


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    ls <- getLines fileIn
    let grid = parseGrid ls
    print grid
    print $ fst $ moreSteps 100 $ parseGrid ls
    pure ()


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    let grid = parseGrid ls
    print $ firstFlash grid
    pure ()


main :: IO ()
main = do
    putStrLn "day 11"
    fstHalf "input1.txt"
    fstHalf "input2.txt"
    sndHalf "input1.txt"
    sndHalf "input2.txt"


