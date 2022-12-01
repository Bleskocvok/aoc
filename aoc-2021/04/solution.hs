#!/usr/bin/env runghc

{-# LANGUAGE TupleSections #-}

module Main where

import AoCUtils
import Data.Array
import Data.Foldable ( foldl' )
import Control.Monad.State
import Data.List (find)
import Debug.Trace


type Board = Array Int (Array Int (Int, Bool))


data Game = Game { draws :: [Int], boards :: [Board] }


(<$$>) :: ((Int, Bool) -> (Int, Bool)) -> Board -> Board
fun <$$> b = appline <$> b
    where
        appline arr = fun <$> arr


pprint :: Board -> String
pprint = concatMap printLine . elems
    where
        printLine = (++ "\n") . concatMap ((++ " ") . show . fst) . elems


makeDraw :: Int -> Board -> Board
makeDraw i b = change <$$> b
    where
        change (n, bool)
            | n == i = (n, True)
            | otherwise = (n, bool)


checkBingo :: Board -> Bool
checkBingo b = or [checkH b i || checkV b i | i <- [0 .. 4]]
    where
        checkH brd i = all snd (brd ! i)
        checkV brd i = foldr chkOne True brd
            where
                chkOne :: Array Int (Int, Bool) -> Bool -> Bool
                chkOne l a = snd (l ! i) && a


parseBoard :: [String] -> Board
parseBoard = listArray (0, 4) . foldr forLine []
    where
        forLine l c = listArray (0, 4) (map wordToField $ words l) : c
        wordToField :: String -> (Int, Bool)
        wordToField = (flip (,) False) . read


parseLines :: State [String] [Board]
parseLines = do
    let getter = drop 1 . take 6
    lines <- gets getter
    if null lines then
        pure []
    else do
        let b = parseBoard lines
        modify (drop 6)
        bs <- parseLines
        pure $ b : bs


clean :: Board -> Board
clean = (cleaner <$$>)
    where
        cleaner (n, _) = (n, False)


bempty :: Board
bempty = listArray (0, 0) [listArray (0, 0) [(0, False)]]


playGame :: State Game (Int, Board)
playGame = do
    dr <- gets draws
    bs <- gets boards
    if null dr then
        pure (-1, bempty)
    else do
        let new = makeDraw (head dr) `map` bs
        let mbyWin = find checkBingo new
        case mbyWin of
            Just board -> pure (head dr, board)
            Nothing -> do
                put $ Game (tail dr) new
                playGame


getOrder :: Game -> [(Int, Board)]
getOrder (Game _ []) = []
getOrder gam = (i, winbrd) : getOrder (Game (draws gam) rem)
    where
        (i, winbrd) = evalState playGame gam
        rem = filter (clean winbrd /=) (boards gam)


-- lastWin :: State Game (Int, Board)
-- lastWin = do
--     dr <- gets draws
--     bs <- gets boards
    
--     if null dr then
--         pure (-1, bempty)
--     else do
--         let new = makeDraw (head dr) `map` bs
--         let mbyWin = find checkBingo new
--         case mbyWin of
--             Nothing -> do
--                 put $ Game (tail dr) new
--                 lastWin
--             Just board -> do
--                 case new of
--                     [one] -> pure (head dr, one)
--                     brds -> do
--                         let removed = filter (not . checkBingo) brds
--                         if length removed <= 1 then
--                             pure (head dr, last removed)
--                         else do
--                             put $ Game (tail dr) removed
--                             lastWin


score :: Board -> Int
score = sum . map fst . filter ((False ==) . snd) . concat . (elems <$>) . elems


main2 :: IO ()
main2 = do
    putStrLn "fst solution"
    ls <- getLines "input2.txt"

    let boards = evalState parseLines (drop 1 ls)
        game1 = Game draws2 boards

    let result = evalState playGame game1
        last = fst result
        board = snd result
        scr = score board
    putStrLn $ pprint board
    print last
    print scr
    print $ scr * last


main3 :: IO ()
main3 = do
    putStrLn "snd test"
    ls <- getLines "input1.txt"
    let draws = draws1

    let boards = evalState parseLines (drop 1 ls)
        game1 = Game draws boards

    let result = last $ getOrder game1
        lst = fst result
        board = snd result
        scr = score board
    putStrLn $ pprint board
    print lst
    print scr
    print $ scr * lst


main4 :: IO ()
main4 = do
    putStrLn "snd main"
    ls <- getLines "input2.txt"
    let draws = draws2

    let boards = evalState parseLines (drop 1 ls)
        game1 = Game draws boards

    let result = last $ getOrder game1
        lst = fst result
        board = snd result
        scr = score board
    putStrLn $ pprint board
    print lst
    print scr
    print $ scr * lst


main :: IO ()
main = do
    ls <- getLines "input1.txt"

    let boards = evalState parseLines (drop 1 ls)
        game1 = Game draws1 boards

    let result = evalState playGame game1
        last = fst result
        board = snd result
        scr = score board
    putStrLn $ pprint board
    print last
    print scr
    print $ scr * last

    main2
    main3
    main4


draws1 :: [Int]
draws1 = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]


draws2 :: [Int]
draws2 =
    [
        13,47,64,52,60,69,80,85,57,1,2,6,30,81,86,40,27,26,97,77,70,92,43,94,8,
        78,3,88,93,17,55,49,32,59,51,28,33,41,83,67,11,91,53,36,96,7,34,79,98,
        72,39,56,31,75,82,62,99,66,29,58,9,50,54,12,45,68,4,46,38,21,24,18,44,
        48,16,61,19,0,90,35,65,37,73,20,22,89,42,23,15,87,74,10,71,25,14,76,84,
        5,63,95
    ]
