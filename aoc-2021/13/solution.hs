#!/usr/bin/env runghc

module Main where

import AoCUtils ( getLines )
import Data.Array ( Array, (!), (//), bounds, listArray )
import Data.List.Split ( splitOn )
import Data.List ( foldl', group, sort )
import Data.Tuple ( swap )


data Origami = Origami
                { getPts :: [(Int, Int)]
                , getFolds :: [Fold] }
                deriving ( Show )


data Fold = AlongX Int | AlongY Int
    deriving ( Show, Eq )


parseFolds :: [String] -> [Fold]
parseFolds = foldr (flip parseOne) []
    where
        parseOne acc ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ': c :'=': num)
            = (case c of
                'x' -> AlongX
                'y' -> AlongY
                _ -> error "parseFolds: invalid coordinate")
              (read num) : acc
        parseOne _ _ = error "parseFolds: invalid format"


foldPt :: (Int, Int) -> Fold -> (Int, Int)
foldPt (x, y) f =
    case f of
        AlongY v -> swap $ foldPt (y, x) (AlongX v)
        AlongX v -> if x > v
                    then (2 * v - x, y)
                    else (x, y)


applyFolds :: (Int, Int) -> [Fold] -> (Int, Int)
applyFolds = foldl' foldPt


foldPoints :: [(Int, Int)] -> [Fold] -> [(Int, Int)]
foldPoints pts flds = map (`applyFolds` flds) pts


parsePts :: [String] -> [(Int, Int)]
parsePts [] = []
parsePts (l : ls)
    | ',' `elem` l = do
        let bs = splitOn "," l
            (x : y : _) = bs
        (read x, read y) : parsePts ls
    | otherwise = []


parseOrigami :: [String] -> Origami
parseOrigami ls = do
    let ptLs = takeWhile (not . null) ls
        foldLs = tail $ dropWhile (not . null) ls
        folds = parseFolds foldLs
        pts = parsePts ptLs
    Origami pts folds


dedup :: Ord a => [a] -> [a]
dedup = (head `map`) . group . sort


mkTable :: [(Int, Int)] -> Array (Int, Int) Bool
mkTable pts = square w h // map (flip (,) True . swap) pts
    where
        w = (maximum . map fst) pts + 1
        h = (maximum . map snd) pts + 1


square :: Int -> Int -> Array (Int, Int) Bool
square w h = listArray ((0, 0), (h - 1, w - 1)) (replicate (h * w) False)


showTable :: Array (Int, Int) Bool -> String
showTable tbl = concat [ ch ++ del |
                    y <- [0 .. h'],
                    x <- [0 .. w'],
                    let ch = if tbl ! (y, x)
                             then "#"
                             else ".",
                    let del = if x == w'
                             then "\n"
                             else " "]
    where ((_, _), (h', w')) = bounds tbl


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    ls <- getLines fileIn
    let orig = parseOrigami ls
        folded = foldPoints (getPts orig) (take 1 $ getFolds orig)
        table = mkTable folded
    print $ length $ dedup folded
    pure ()


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    ls <- getLines fileIn
    let orig = parseOrigami ls
        folded = foldPoints (getPts orig) (getFolds orig)
        table = mkTable folded
    print $ length $ dedup folded
    putStrLn $ showTable table
    pure ()


main :: IO ()
main = do
    putStrLn "day 13"
    putStrLn "\nfirst"
    fstHalf "input1.txt"
    fstHalf "input2.txt"
    putStrLn "\nsecond"
    sndHalf "input1.txt"
    sndHalf "input2.txt"


