#!/usr/bin/env runghc

module Main where

import AoCUtils ( getLines )
import Data.List
import qualified Data.Map as M
import Control.Monad.State


-- parsing


type Rule = ((Char, Char), Char)


-- ewwww, factory
data PolymerFactory = PolymerFactory
                    { getStart :: String
                    , getRules :: [Rule] }
                    deriving ( Show )


parseRule :: String -> Rule
parseRule (a : b :' ':'-':'>':' ': r : _) = ((a, b), r)
parseRule _ = error "parseRule: invalid format"


parseRules :: [String] -> [Rule]
parseRules = (parseRule `map`)


-- this is so disgusting
parsePolymerFactory :: [String] -> PolymerFactory
parsePolymerFactory (s : _ : rest) = PolymerFactory s (parseRules rest)
parsePolymerFactory _ = error "parsePolymerFactory: invalid format"


-- bad complexity


step :: [Rule] -> String -> String
step _ [] = ""
step _ [a] = [a]
step rules (a : b : xs) =
    case found of
        Nothing -> step rules (b : xs)
        Just c -> a : c : step rules (b : xs)
    where
        found = lookup (a, b) rules


stepsN :: Int -> [Rule] -> String -> String
stepsN i rules st = foldr (\_ a -> step rules a) st [1 .. i]


minMax :: String -> (Int, Int)
minMax = foldr1 mm . map ((\x -> (x, x)) . length) . group . sort
    where
        mm (x, _) (lo', hi') = (min lo' x, max hi' x)


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    ls <- getLines fileIn
    let factory = parsePolymerFactory ls
        str = getStart factory
        rules = getRules factory
        (lst, mst) = minMax $ stepsN 10 rules str
    print $ step rules str
    print (mst, lst)
    print (mst - lst)
    pure ()


-- second half will need to have better complexity


data Side = Mid | L | R
    deriving ( Show, Eq, Ord )

type Key = ((Char, Char), Side)

type Pairs = M.Map Key Int


getPairs' :: String -> [(Key, Int)]
getPairs' (a : b : xs) = (((a, b), L), 1) : result
    where
        result = init mid ++ [toR (last mid)]
        mid = impl (b : xs)
        toR ((p, _), i) = ((p, R), i)
        impl (x : y : xs) = (((x, y), Mid), 1) : impl (y : xs)
        impl _ = []
getPairs' _ = []


mkPairs :: String -> Pairs
mkPairs = M.fromList . getPairs'


-- getPairs :: String -> [(Char, Char)]
-- getPairs (x : y : xs) = (x, y) : getPairs xs
-- getPairs _ = []


-- mkPairs :: String -> Pairs
-- mkPairs = foldl' (\m x -> M.insertWith add x (1, Mid) m) M.empty . getPairs


-- add :: Element -> Element -> Element
-- add (i, p) (j, p') = if p == p'
--     then (i + j, p)
--     else error "add: sides not matching"


-- step' :: [Rule] -> Pairs -> Pairs
-- step' rules pairs = foldr apply [] rules
--     where
--         apply rul acc = case found of
--             Nothing -> acc
--             Just x -> addToList
--             where
--                 found = M.lookup (fst rul) pairs
--         addToList (a, b) i lst
--             | (a, b) `elem` lst = map (madd (a, b), i) lst
--             | otherwise = ((a, b), i)
--         madd x@(_, i') (a, b) i
--             | fst x == (a, b) = ((a, b), i' + i)
--             | otherwise = x


step' :: [Rule] -> Pairs -> State Pairs ()
step' rules pairs = do
    forM_ rules addPairs
    foldM_ addNotIncluded () (M.toList pairs)
    where
        addNotIncluded :: () -> (Key, Int) -> State Pairs ()
        addNotIncluded _ (((a, b), s), i)
            | (a, b) `notElem` map fst rules = modify' $ M.insert ((a, b), s) i
            | otherwise = pure ()
        addPairs :: Rule -> State Pairs ()
        addPairs rul = do
            addBySide L rul
            addBySide R rul
            addBySide Mid rul
        addBySide :: Side -> Rule -> State Pairs ()
        addBySide side rul =
            let
                ((a, b), c) = rul
                found = M.lookup (fst rul, side) pairs
            in
                case found of
                    Just i -> do
                        case side of
                            L -> do
                                modify' $ M.insertWith (+) ((a, c), L) i
                                modify' $ M.insertWith (+) ((c, b), Mid) i
                            R -> do
                                modify' $ M.insertWith (+) ((a, c), Mid) i
                                modify' $ M.insertWith (+) ((c, b), R) i
                            Mid -> do
                                modify' $ M.insertWith (+) ((a, c), Mid) i
                                modify' $ M.insertWith (+) ((c, b), Mid) i
                    Nothing -> pure ()


stepsN' :: Int -> [Rule] -> Pairs -> Pairs
stepsN' i rules st = foldr (\_ a -> execState (step' rules a) M.empty) st [1 .. i]


countElems :: Pairs -> M.Map Char Int
countElems pairs = (`div` 2) <$> foldl' (flip adda) M.empty (M.toList pairs)
    where
        adda (((a, b), side), i) =
            let
                i' = sideToInt side i
            in
                M.insertWith (+) a i'
              . M.insertWith (+) b i'
        sideToInt L   i = i * 2
        sideToInt R   i = i * 2
        sideToInt Mid i = i


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    let factory = parsePolymerFactory ls
        pairs = mkPairs $ getStart factory
        rules = getRules factory
        res = stepsN' 40 rules pairs
        count = countElems res
        resNumber = (minimum count, maximum count)
    print count
    print $ snd resNumber - fst resNumber


main :: IO ()
main = do
    putStrLn "day 14"
    putStrLn "\nfirst"
    fstHalf "input1.txt"
    fstHalf "input2.txt"
    putStrLn "\nsecond"
    sndHalf "input1.txt"
    sndHalf "input2.txt"


