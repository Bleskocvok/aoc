
module AoCUtils where


import Data.Functor ( (<&>) )


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
