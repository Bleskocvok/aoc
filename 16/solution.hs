#!/usr/bin/env runghc

module Main where

import AoCUtils ( getLines )
import Control.Monad.State
    ( MonadState(get),
      gets,
      liftM2,
      when,
      forM_,
      modify,
      modify',
      evalState,
      runState,
      withState,
      State )
import Data.Functor ()


hexChar :: Char -> Bits
hexChar c = case c of
    '0' -> [f, f, f, f]
    '1' -> [f, f, f, t]
    '2' -> [f, f, t, f]
    '3' -> [f, f, t, t]
    '4' -> [f, t, f, f]
    '5' -> [f, t, f, t]
    '6' -> [f, t, t, f]
    '7' -> [f, t, t, t]
    '8' -> [t, f, f, f]
    '9' -> [t, f, f, t]
    'A' -> [t, f, t, f]
    'B' -> [t, f, t, t]
    'C' -> [t, t, f, f]
    'D' -> [t, t, f, t]
    'E' -> [t, t, t, f]
    'F' -> [t, t, t, t]
    e   -> error $ "parseBits: invalid hex char " ++ show e
    where
        t, f :: Bool
        t = True
        f = False


data PacType = Literal Int
             | LenBits [Packet]
             | NumSub [Packet]
             deriving ( Show )


data Packet = Packet { version :: Int
                     , tid     :: Int
                     , pacType :: PacType }
                     deriving ( Show )


type Bits = [Bool]


parseBits :: String -> Bits
parseBits = concatMap hexChar


binToDec :: Bits -> Int
binToDec = sum . zipWith f [0 ..] . reverse
    where
        f p n = bit n * 2 ^ p
        bit True = 1
        bit _    = 0


subPacketsMax :: Int -> State Bits [Packet]
subPacketsMax n
    | n == 0 = pure []
    | otherwise = liftM2 (:) parsePacket (subPacketsMax (n - 1))


subPacketsBits :: Int -> State Bits [Packet]
subPacketsBits n = do
    st <- get
    let (pkt, rest) = runState (withState (take n) parsePacket) st
        count = length (take n st) - length rest
    modify $ (rest ++) . drop n
    (pkt :) <$>
        if count >= n then
            pure []
        else
            subPacketsBits (n - count)


literal :: Int -> Int -> Bits -> State Bits Packet
literal v i acc = do
    nxt <- gets $ take 5
    modify $ drop 5
    when (length nxt /= 5) $ error $ "literal: cannot get 5 bits, only "
                                      ++ show (length nxt)
    let bits = acc ++ tail nxt
    if not $ head nxt then do
        pure $ Packet v i (Literal $ binToDec bits)
    else
        literal v i bits


parsePacket :: State Bits Packet
parsePacket = do
    v <- parseInt 3
    i <- parseInt 3
    if i == 4 then
        literal v i []
    else do
        l <- parseInt 1
        Packet v i <$> case l of
            0 -> LenBits <$> (parseInt 15 >>= subPacketsBits)
            1 -> NumSub <$> (parseInt 11 >>= subPacketsMax)
            _ -> error "parsePacket: invalid length type id"


parseInt :: Int -> State Bits Int
parseInt n = do
    bits <- gets $ take n
    when (length bits /= n) $ error $ "parseInt: cannot parse "
                                      ++ show n ++ " bits, only "
                                      ++ show (length bits)
    modify' $ drop n
    pure $ binToDec bits


verSum :: Packet -> Int
verSum (Packet v _ ptype) =
    v + case ptype of
        LenBits pkts -> s pkts
        NumSub  pkts -> s pkts
        Literal _    -> 0
    where s = sum . map verSum


fstHalf :: FilePath -> IO ()
fstHalf fileIn =
    let
        solveOne input = do
            let bitvec = parseBits input
                pkt = evalState parsePacket bitvec
                res = verSum pkt
            putStrLn ""
            print input
            -- print pkt
            print res
    in do
        ls <- getLines fileIn
        forM_ ls solveOne


evalPacket :: Packet -> Int
evalPacket (Packet _ tid dat) =
    let
        oper i = case i of
            0 -> sum
            1 -> product
            2 -> minimum
            3 -> maximum
            5 -> fromEnum . onTwo (>)
            6 -> fromEnum . onTwo (<)
            7 -> fromEnum . onTwo (==)
            _ -> error "evalPacket: invalid operator"
        onTwo op lst = a `op` b
            where [a, b] = take 2 lst
    in
        case (tid, dat) of
            (4, Literal n) -> n
            (4, _) -> error "evalPacket: tid=4, expected Literal"
            (op, NumSub pkts)  -> oper op (evalPacket `map` pkts)
            (op, LenBits pkts) -> oper op (evalPacket `map` pkts)
            (_, _) -> error "evalPacket: invalid packet"


sndHalf :: FilePath -> IO ()
sndHalf fileIn =
    let
        solveOne input = do
            let bitvec = parseBits input
                pkt = evalState parsePacket bitvec
                evald = evalPacket pkt
            putStrLn ""
            print input
            print evald
    in do
        ls <- getLines fileIn
        forM_ ls solveOne


main :: IO ()
main = do
    putStrLn "day 16"
    putStrLn "\nfirst"
    fstHalf "input1.txt"
    fstHalf "input2.txt"
    putStrLn "\nsecond"
    sndHalf "input1.txt"
    sndHalf "input2.txt"


