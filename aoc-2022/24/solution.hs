#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines )

import Data.Array
import Data.List
import Data.Maybe
import Data.Bool
import Data.Bifunctor

import qualified Data.Set as S


data Blizzard = N | S | W | E  -- up, down, left, right
              deriving ( Eq )

type Blizzards = Array (Int, Int) [Blizzard]

data Map = Map { blizzards :: Blizzards
               , from :: Int
               , dest :: Int }

type Visited = Array (Int, Int) Bool


instance Show Blizzard where
    show b = case b of N -> "^"; S -> "v"; W -> "<"; E -> ">"


instance Show Map where
    show (Map bs beg end) = border beg ++ "\n"
                            ++ showMap bs
                            ++ border end ++ "\n"
        where
            border i = [ bool '#' '.' (x == i) | x <- [0 .. maxX] ]
            maxX = fst . snd . bounds $ bs


showMap :: Blizzards -> String
showMap m = foldl showLine [] [ly..hy]
    where
        ((lx, ly), (hx, hy)) = bounds m
        showLine str y = str ++ map (chrs !) (zip [lx..hx] (repeat y)) ++ "\n"
        chrs = showElem <$> m
        showElem []  = '.'
        showElem [x] = head . show $ x
        showElem xs  = head . show . length $ xs


parseMap :: [String] -> Map
parseMap lines = case lines of
    []       -> error "parseMap: empty input"
    (l : ls) ->
        let
            start = findDot l
            end   = findDot . last $ ls
            xs = map (parseLine) . init $ ls
            blizz = listArray ((0, 0), getBounds xs) . concat . transpose $ xs
        in
            Map blizz start end
    where
        findDot = ((-1) +) . fromJust . elemIndex '.'
        parseLine = map parseOne . init . tail
        parseOne c = case c of '.' -> []
                               '^' -> [N]
                               'v' -> [S]
                               '<' -> [W]
                               '>' -> [E]
        getBounds xs = (length (head xs) - 1, length xs - 1)


apply :: (Blizzards -> Blizzards) -> Map -> Map
apply f (Map blizz a b) = (Map (f blizz) a b)


next :: Blizzards -> Blizzards
next bs = accum oneNext bs (zip ixs ixs)
    where
        ixs = indices bs

        oneNext _ (x, y) = concat [ get (x, y + 1) N
                                  , get (x, y - 1) S
                                  , get (x + 1, y) W
                                  , get (x - 1, y) E ]
            where
                get ix b = filter (b ==) $ bs ! (wrapIdx ix)

        wrapIdx (x, y) = (wrap' 0 hx x, wrap' 0 hy y)
            where
                wrap' v0 vmax v | v > vmax  = v0
                                | v < v0    = vmax
                                | otherwise = v
                (_, (hx, hy)) = bounds bs


bfs :: Int -> Map -> Visited -> Int
bfs i m vis | vis ! (dest m, hy) = i
            | otherwise = bfs (i + 1) (next `apply` m) nextVis
    where
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
runBfs m = bfs 0 m ((const False <$> blizzards m) // [((from m, 0), True)])


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    m <- parseMap <$> getLines fileIn
    let result = runBfs m
    print m
    print $ next `apply` m
    print $ (next . next) `apply` m
    print result
    pure ()


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    pure ()


main :: IO ()
main = putStrLn "day 24" >> putStrLn "\nfirst"  >> fstHalf "example.txt"
                                                >> fstHalf "input1.txt"
                         >> putStrLn "\nsecond" >> sndHalf "example.txt"
                                                >> sndHalf "input1.txt"
