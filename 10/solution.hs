#!/usr/bin/env runghc

module Main where

import AoCUtils ( getLines )
import Data.List ( find, foldl', sort )
import Data.Maybe ( isJust, fromJust, isNothing )
import Control.Monad ( forM_, msum )
import Control.Monad.State.Lazy ( MonadState(get), modify, evalState, State )


chunks :: [((Char, Char), Int)]
chunks = [ (('(', ')'), 3)
         , (('[', ']'), 57)
         , (('{', '}'), 1197)
         , (('<', '>'), 25137) ]


mScore :: Char -> Int
mScore ')' = 1
mScore ']' = 2
mScore '}' = 3
mScore '>' = 4
mScore _ = error "mScore: invalid char"


scoreMissing :: [Char] -> Int
scoreMissing = foldl' (\a x -> a * 5 + mScore x) 0


opening :: Char -> Bool
opening c = (c `elem`) $ map (fst . fst) chunks


closing :: Char -> Bool
closing c = (c `elem`) $ map (snd . fst) chunks


findClose :: Char -> Maybe ((Char, Char), Int)
findClose c = find ((c ==) . snd . fst) chunks


findOpen :: Char -> Maybe ((Char, Char), Int)
findOpen c = find ((c ==) . fst . fst) chunks


score :: Char -> Int
score c = snd $ fromJust $ findClose c


inverse :: Char -> Char
inverse c
    | isJust op = snd . fst $ fromJust op
    | isJust cl = fst . fst $ fromJust cl
    | otherwise = error "invalid thing"
    where
        op = findOpen c
        cl = findClose c


-- lineReader :: Maybe Char -> State String (Maybe Int)
-- lineReader ch = do
--     xs <- get
--     if null xs then
--         pure Nothing
--     else do
--         modify $ drop 1
--         let h = head xs
--         if opening h then do
--             r <- lineReader $ Just h
--             rest <- lineReader Nothing
--             pure $ msum [ r, rest ]
--         else if closing h then do
--             rest <- lineReader Nothing
--             pure $ msum [ mismatch ch h, rest ]
--         else do
--             pure $ Just $ score h
--     where
--         mismatch Nothing cl = Just $ score cl
--         mismatch (Just op) cl
--             | cl == inverse op = Nothing
--             | otherwise = Just $ score cl
    


-- checkLine :: String -> Maybe Int
-- checkLine = evalState (lineReader Nothing)


missing :: [Char] -> String -> [Char]
missing acc [] = inverse `map` acc
missing acc (h : xs)
    | opening h = missing (h : acc) xs
    | closing h = case acc of
                    [] -> missing acc xs
                    (a : as) -> if a == inverse h then
                            missing as xs
                        else
                            error "invalid closing bracket '" ++ show h ++ "'"
    | otherwise = error "not accounted for"


checkLine' :: [Char] -> String -> Maybe Int
checkLine' _ [] = Nothing
checkLine' acc (h : xs)
    | opening h = checkLine' (h : acc) xs
    | closing h = case acc of
                    [] -> checkLine' acc xs
                    (a : as) -> if a == inverse h then
                            checkLine' as xs
                        else
                            Just $ score h
    | otherwise = error "not accounted for"


toInt :: Maybe Int -> Int
toInt Nothing = 0
toInt (Just n) = n


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    ls <- getLines fileIn
    let total = sum $ toInt . checkLine' [] <$> ls 
    -- forM_ (show . checkLine' [] <$> ls) putStrLn
    print total


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    let incompl = filter (isNothing . checkLine' []) ls
        finished = missing [] `map` incompl
        scored = map scoreMissing finished
        sorted = sort scored
        middle = sorted !! (length sorted `div` 2)
    print middle
    pure ()


main :: IO ()
main = do
    putStrLn "day 10"
    fstHalf "input1.txt"
    fstHalf "input2.txt"
    sndHalf "input1.txt"
    sndHalf "input2.txt"


