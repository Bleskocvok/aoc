
module AoCUtils where

import Data.Functor ( (<&>) )


getLines :: FilePath -> IO [String]
getLines file = readFile file <&> lines


getNums :: FilePath -> IO [Int]
getNums file = do
    ls <- getLines file
    pure $ read <$> ls :: IO [Int]
