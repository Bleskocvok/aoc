
module AoCUtils where

import Data.Functor ( (<&>) )
import Data.List.Split ( splitOn )


getLines :: FilePath -> IO [String]
getLines file = readFile file <&> lines


getNums :: FilePath -> IO [Int]
getNums file = do
    ls <- getLines file
    pure $ read <$> ls :: IO [Int]


getCSVNums :: FilePath -> IO [Int]
getCSVNums file = getLines file <&> map toInt . splitOn "," . head
    where
        toInt :: String -> Int
        toInt = read

