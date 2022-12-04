#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines )

-- mtl
import Control.Monad.State

import Data.Char ( isDigit )
import Control.Monad ( liftM )


type Range = (Int, Int)


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
    c' <- gets head
    when (c' /= c) (error "invalid format")
    modify $ drop 1


parsePair :: State String (Range, Range)
parsePair = do
    a <- readNum
    eatChar '-'
    b <- readNum

    eatChar ','

    c <- readNum
    eatChar '-'
    d <- readNum

    pure ((a, b), (c, d))


getRanges :: FilePath -> IO [(Range, Range)]
getRanges f = ((evalState parsePair) <$>) <$> getLines f


includes :: Range -> Range -> Bool
includes a b = let sndInFst (a, b) (a', b') = a <= a' && b >= b'
                in sndInFst a b || sndInFst b a


overlaps :: Range -> Range -> Bool
overlaps (a, b) (a', b') = not (b < a' || b' < a)


filterFile :: (Range -> Range -> Bool) -> FilePath -> IO ()
filterFile p f = filter (uncurry p) <$> getRanges f >>= print . length


fstHalf :: FilePath -> IO ()
fstHalf = filterFile includes


sndHalf :: FilePath -> IO ()
sndHalf = filterFile overlaps


main :: IO ()
main = do
    putStrLn "day 04"

    putStrLn "\nfirst"
    fstHalf "input1.txt"

    putStrLn "\nsecond"
    sndHalf "input1.txt"
