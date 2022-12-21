
module AoCUtils where


import Data.Functor ( (<&>) )
import Control.Monad.State ( when, gets, modify, State )
import Data.Char ( isDigit )


getLines :: FilePath -> IO [String]
getLines file = readFile file <&> lines


getNums :: FilePath -> IO [Int]
getNums file = do
    ls <- getLines file
    pure $ read <$> ls :: IO [Int]


getCSVNums :: FilePath -> IO [Int]
getCSVNums file = getLines file <&> map toInt . splitOn ',' . head
    where
        toInt :: String -> Int
        toInt = read


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = splitAcc []
    where
        splitAcc acc _ [] = [reverse acc]
        splitAcc acc c (x : xs)
            | x == c    = (reverse acc) : splitAcc [] c xs
            | otherwise = splitAcc (x : acc) c xs


eats :: (s -> a) -> State [s] a
eats f = do
    res <- gets $ f . head
    modify tail
    pure res


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
    when empty (error $ "eatChar: empty, expected '" ++ show c ++ "'")
    c' <- gets head
    when (c' /= c) (error $ "invalid format '" ++ [c'] ++ "' /= '" ++ [c] ++ "'")
    modify $ drop 1


eatStr :: String -> State String ()
eatStr = mapM_ eatChar
