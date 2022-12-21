#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines, splitBy )

import qualified Data.Map as M
import Control.Monad ( liftM2 )


data Operator = Add | Sub | Mul | Div deriving ( Eq, Show )
data Expr = Val Int | Op Operator String String


runOp :: Operator -> (Int -> Int -> Int)
runOp op = case op of Add -> (+)
                      Sub -> (-)
                      Mul -> (*)
                      Div -> (div)


trim :: String -> String
trim = let trimPref = dropWhile (' ' ==)
        in reverse . trimPref . reverse . trimPref


parseMonkey :: String -> (String, Expr)
parseMonkey xs = case splitBy (`elem` ":+-*/") xs of
    [name, a, b] -> (trim name, Op digOp (trim a) (trim b))
    [name, val]  -> (trim name, Val . read . trim $ val)
    _            -> error "invalid monkey format"
    where
        digOp | '+' `elem` xs = Add
              | '-' `elem` xs = Sub
              | '*' `elem` xs = Mul
              | '/' `elem` xs = Div
              | otherwise = error "invalid monkey op"


parseMonkeys :: [String] -> M.Map String Expr
parseMonkeys = foldMap (uncurry M.singleton . parseMonkey)


-- naive cause it's in O(2^n * log n) (times log n from map lookup)
-- where n is size of the map
-- but apparently it's sufficient for the task
naiveEval :: String -> M.Map String Expr -> Maybe Int
naiveEval name ms = ms M.!? name >>= eval
    where
        eval (Val n) = Just n
        eval (Op op a b) = liftM2 (runOp op) (naiveEval a ms) (naiveEval b ms)


data TreeExpr = TreeX | TreeVal Int | TreeOp Operator TreeExpr TreeExpr
              deriving ( Eq, Show )


-- same complexity, but it's fine for given input
makeTree :: M.Map String Expr -> String -> TreeExpr
makeTree ms name =
    case ms M.!? name of
        Nothing          -> TreeX
        Just (Val n)     -> TreeVal n
        Just (Op op a b) -> case (makeTree ms a, makeTree ms b) of
            (TreeVal x, TreeVal y) -> TreeVal $ (runOp op) x y
            (l, r)                 -> TreeOp op l r


solveTreeX :: TreeExpr -> Int -> Maybe Int
solveTreeX expr n = case expr of
    TreeX      -> Just n
    TreeVal n' -> if n' == n then Just 0 else Nothing  -- inf or 0 solutions
    TreeOp Add a (TreeVal vb) -> solveTreeX a (n - vb)
    TreeOp Mul a (TreeVal vb) -> solveTreeX a (n `div` vb)
    TreeOp Sub a (TreeVal vb) -> solveTreeX a (n + vb)
    TreeOp Div a (TreeVal vb) -> solveTreeX a (n * vb)
    -- non trivial cases
    TreeOp Sub (TreeVal va) b -> solveTreeX b ((n - va) * (-1))
    TreeOp Div (TreeVal va) b -> solveTreeX b (va `div` n)
    -- the rest are swapped cases for commutative operators
    TreeOp op l@(TreeVal _) r -> solveTreeX (TreeOp op r l) n
    TreeOp _  _             _ -> Nothing


fstHalf :: FilePath -> IO ()
fstHalf fileIn = parseMonkeys <$> getLines fileIn >>= print . naiveEval "root"


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ms <- M.delete "humn" . parseMonkeys <$> getLines fileIn
    let (Op _ l r) = ms M.! "root"
        sol = case (makeTree ms l, makeTree ms r) of
            (TreeVal x, rt) -> solveTreeX rt x
            (lt, TreeVal x) -> solveTreeX lt x
            _               -> error "cannot solve"
    print sol


main :: IO ()
main = putStrLn "day 21" >> putStrLn "\nfirst"  >> fstHalf "example.txt"
                                                >> fstHalf "input1.txt"
                         >> putStrLn "\nsecond" >> sndHalf "example.txt"
                                                >> sndHalf "input1.txt"
