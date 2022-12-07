#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines )

-- mtl
import Control.Monad.State
        ( liftM, liftM2, when, gets, modify, evalState, State )

import Data.Array ( (!), (//), elems, listArray, Array )
import Data.Char ( isDigit )
import Data.Maybe ( fromJust, isNothing )


type Move = (Int, Int, Int) -- move from to
type Cargo = Array Int [Char]


parseCargoLine :: String -> [Maybe Char]
parseCargoLine ('[' : c   : ']' : xs) = Just c  : parseCargoLine xs
parseCargoLine (' ' : ' ' : ' ' : xs) = Nothing : parseCargoLine xs
parseCargoLine (' ' :             xs) =           parseCargoLine xs
parseCargoLine [] = []
parseCargoLine _ = error "parseCargoLine: invalid format"


mkArray :: [[Maybe Char]] -> Cargo
mkArray [] = error "mkArray: empty"
mkArray mat = listArray (1, size)
                        [cleanup (mkList i mat) | i <- [0 .. size-1] ]
    where
        -- size = length $ head mat
        size = minimum $ length `map` mat
        cleanup = map fromJust . dropWhile isNothing
        mkList i mat = case mat of [] -> []
                                   (lst : xs) -> (lst !! i) : mkList i xs

eats :: (s -> a) -> State [s] a
eats f = do
    res <- gets $ f . head
    modify tail
    pure res


parseCargo :: State [String] [[Maybe Char]]
parseCargo = do
    let endCargo (Just (' ' : '1' : _)) = True
        endCargo (Just []) = True
        endCargo Nothing = True
        endCargo _ = False

    end <- gets $ endCargo . safeHead

    if end then
        pure []
    else do
        liftM2 (:) (eats parseCargoLine) parseCargo


parseMoves :: State [String] [Move]
parseMoves = do
    ll <- gets safeHead
    case ll :: Maybe String of
        Nothing -> pure []
        Just l -> do
            modify tail
            let move = evalState parseMoveLine l
            liftM (move :) parseMoves
            -- :)
    where
        parseMoveLine :: State String Move
        parseMoveLine = do
            eatStr "move "
            a <- readNum
            eatStr " from "
            b <- readNum
            eatStr " to "
            c <- readNum
            pure (a, b, c)


parseInput :: State [String] (Cargo, [Move])
parseInput = do
    cargo <- mkArray <$> parseCargo
    modify $ drop 2
    moves <- parseMoves
    pure (cargo, moves)


performMove :: Move -> Cargo -> Cargo
performMove (0, _, _) c = c
performMove (m, from, to) c = performMove (m - 1, from, to) $
    c // [ (from, tail $ c ! from),
           (to, head (c ! from) : c ! to) ]


performMove9001 :: Move -> Cargo -> Cargo
performMove9001 (0, _, _) c = c
performMove9001 (m, from, to) c =
    c // [ (from, drop m $ c ! from),
           (to,   take m  (c ! from) ++ c ! to) ]


getInputFromFile :: FilePath -> IO (Cargo, [Move])
getInputFromFile f = evalState parseInput <$> getLines f


solution :: (Move -> Cargo -> Cargo) -> FilePath -> IO ()
solution perform fileIn = do
    (c, m) <- getInputFromFile fileIn
    -- performMovesDebug c m
    print $ map head (elems $ performMoves m c)

    where
        performMoves [] c = c
        -- performMoves (m' : ms) c = performMoves ms (perform m' c)
        
        -- hmmmmmmmmmmmmmmmm
        -- why does    foldl (flip perform)    work
        -- but         foldr perform           doesn't????
        -- I'll have to think about it later, it's too late now
        performMoves ms c = foldl (flip perform) c ms

        performMovesDebug c [] = putStrLn "END" >> (print $ map head (elems c))
        performMovesDebug c (m : ms) = do
            let after = perform m c
            putStrLn $ show after
            performMovesDebug after ms


fstHalf :: FilePath -> IO ()
fstHalf = solution performMove


sndHalf :: FilePath -> IO ()
sndHalf = solution performMove9001


main :: IO ()
main = do
    putStrLn "day 05"

    putStrLn "\nfirst"
    fstHalf "input1.txt"

    putStrLn "\nsecond"
    sndHalf "input1.txt"



safeHead :: [a] -> Maybe a
safeHead xs
    | null xs = Nothing
    | otherwise = (Just . head) xs


readNum :: State String Int
readNum = do
    c <- gets (read . (:[]) . head :: String -> Int)
    modify $ drop 1
    nxt <- gets safeHead
    case nxt of
        Nothing -> pure c
        Just n ->  if isDigit n
                   then readNum >>= pure . ((10 * c) +)
                   else pure c


eatChar :: Char -> State String ()
eatChar c = do
    empty <- gets null
    when empty (error "eatChar: empty")
    c' <- gets head
    when (c' /= c) (error $ "invalid format '" ++ [c'] ++ "' /= '" ++ [c] ++ "'")
    modify $ drop 1


eatStr :: String -> State String ()
eatStr = mapM_ eatChar
