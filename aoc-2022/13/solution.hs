#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines, eatChar, safeHead, readNum )

import Control.Monad.State ( State, liftM, liftM2, when, gets, evalState )
import Data.List ( elemIndex, sortBy )


data Packet = Val Int | List [Packet] deriving ( Eq )

newtype Sep = Sep { fromSep :: String } deriving ( Eq, Show )


instance Monoid Sep where
    mempty = Sep ""

instance Semigroup Sep where
    (<>) (Sep a) (Sep b) = Sep $ a ++ sep ++ b
        where
            sep = if null a || null b then "" else ","

instance Show Packet where
    show = showPacket


showPacket :: Packet -> String
showPacket (Val n) = show n
showPacket (List lst) = "[" ++ fromSep (foldMap Sep (map showPacket lst)) ++ "]"


parsePackets :: State String [Packet]
parsePackets = eatChar '[' *> between <* eatChar ']'
    where
        between :: State String [Packet]
        between = do
            nxt <- gets safeHead
            case nxt of
                Nothing  -> error "unexpected end of input"
                Just ']' -> pure []
                Just c   -> do
                    val <- if c == '['
                           then List <$> parsePackets
                           else Val <$> readNum
                    nxt' <- gets safeHead
                    when (nxt' == Just ',') (eatChar ',')
                    liftM (val :) between


parseOne :: String -> Packet
parseOne = List . evalState parsePackets


parsePairs :: [String] -> [(Packet, Packet)]
parsePairs lst = case lst of
    (a : b : "" : xs) -> (parseOne a, parseOne b) : parsePairs xs
    (a : b : [])      -> (parseOne a, parseOne b) : []
    []                -> []
    _                 -> error "invalid input"


process :: (Packet -> Packet -> b) -> (b -> a -> a) -> a
        -> [(Packet, Packet)] -> a
process _     _     base []       = base
process packF combF base (x : xs) = combF (uncurry packF x)
                                          (process packF combF base xs)


areSame :: Packet -> Packet -> Bool
areSame   (Val   x)    (Val   y)     = x == y
areSame a@(List  _)  b@(Val  y)      = areSame a (List [b])
areSame a@(Val   x)  b@(List _)      = areSame (List [a]) b
areSame   (List xs)    (List ys)     = and (zipWith areSame xs ys)
                                       && length xs == length ys


areRight :: Packet -> Packet -> Bool
areRight   (Val   x)   (Val   y)      = x < y
areRight a@(List  _)  b@(Val  y)      = areRight a (List [b])
areRight a@(Val   x)  b@(List _)      = areRight (List [a]) b
areRight   (List xs)    (List ys)     =
    case dropWhile (uncurry areSame) (zip xs ys) of
        []           -> length xs <= length ys
        ((x, y) : _) -> areRight x y


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    pairs <- parsePairs <$> getLines fileIn
    let okay = process areRight ((:) . fromEnum) [] pairs
        summed = sum $ zipWith (*) okay [1..]
    print okay
    print summed


sndHalf :: FilePath -> IO ()
sndHalf fileIn =
  do
    packets <- map parseOne . filter (not . null) <$> getLines fileIn

    let sortation a b | areSame  a b = EQ
                      | areRight a b = LT
                      | otherwise    = GT
        divA = List [Val 2]
        divB = List [Val 6]
        addDivs = (divA :) . (divB :)

        sorted = sortBy sortation (addDivs packets)

        (ixA, ixB) = ( divA `elemIndex` sorted
                     , divB `elemIndex` sorted )

        result = liftM2 (\a b -> (a + 1) * (b + 1)) ixA ixB
    print result


main :: IO ()
main = putStrLn "day 13" >> putStrLn "\nfirst"  >> fstHalf "example.txt"
                                                >> fstHalf "input1.txt"
                         >> putStrLn "\nsecond" >> sndHalf "example.txt"
                                                >> sndHalf "input1.txt"
