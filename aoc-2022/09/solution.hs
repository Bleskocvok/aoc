#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines )

import qualified Data.Set as S
import Data.Bifunctor ( Bifunctor(first, second) )
import Control.Monad.State ( gets, modify, execState, State )


data Move = MUp | MDown | MLeft | MRight

type Rope = [(Int, Int)]


moveDir :: Move -> (Int, Int)
moveDir m = case m of
    MUp    -> ( 0,  1)
    MDown  -> ( 0, -1)
    MLeft  -> (-1,  0)
    MRight -> ( 1,  0)


readMove :: String -> [Move]
readMove (d : ' ' : xs) =
    replicate (read xs) (case lookup d [('U', MUp), ('D', MDown),
                                        ('L', MLeft), ('R', MRight)] of
                        Nothing -> error "readMove: invalid direction"
                        Just d -> d)
readMove _ = error "readMove: invalid move"


liftPair :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
liftPair op (a, b) = first (op a) . second (op b)

add, sub :: (Int, Int) -> (Int, Int) -> (Int, Int)
add = liftPair (+)
sub = liftPair (-)


moveHead :: Move -> Rope -> Rope
moveHead m (x : xs) = add (moveDir m) x : xs


moveTail :: Rope -> Rope
moveTail (h : t : xs) =
    let
        (x, y) = sub h t
    in
        h : moveTail ((if abs x >= 2 || abs y >= 2
                       then add t (signum x, signum y)
                       else t) : xs)
moveTail x = x


getTail :: Rope -> (Int, Int)
getTail = last


play :: [Move] -> State (S.Set (Int, Int), Rope) ()
play ms =
    case ms of
        [] -> pure ()
        (m : ms) -> do
            modify (second $ moveHead m)
            modify (second moveTail)
            coord <- gets (getTail . snd)
            modify (first $ S.insert coord)
            play ms


simulate :: Int -> FilePath -> IO ()
simulate len fileIn = do
    moves <- (readMove `concatMap`) <$> getLines fileIn
    let rope = replicate len (0, 0)
        (set, _) = execState (play moves)
                             (S.singleton $ getTail rope, rope)
    print $ S.size set


fstHalf :: FilePath -> IO ()
fstHalf = simulate 2


sndHalf :: FilePath -> IO ()
sndHalf = simulate 10


main :: IO ()
main = putStrLn "day 09" >>  putStrLn "\nfirst"  >> fstHalf "input1.txt"
                         >>  putStrLn "\nsecond" >> sndHalf "input1.txt"


