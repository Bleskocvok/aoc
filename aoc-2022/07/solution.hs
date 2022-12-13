#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines, splitOn )

import Control.Monad ( liftM2, when, forM_ )
import Data.Functor ( ($>) )
import Control.Monad.State ( MonadState(get), gets, modify, evalState, State )
import qualified Data.Map.Strict as M


data FileTree = Dir (M.Map String FileTree)
              | File Int
              deriving ( Show )

data Cmd = Cd String | Ls | EchoDir String | EchoFile Int String
    deriving ( Show )


readCmd :: String -> Cmd
readCmd ('$' : ' ' : 'c' : 'd' : ' ' : xs) = Cd xs
readCmd ('$' : ' ' : 'l' : 's' : []) = Ls
readCmd ('d' : 'i' : 'r' : ' ' : xs) = EchoDir xs
readCmd xs = let [size, name] = splitOn ' ' xs
             in  EchoFile (read size) name


tokenize :: [String] -> [Cmd]
tokenize = map readCmd


parseDir :: State [Cmd] FileTree
parseDir = do

    -- first two lines should be cd and ls
    pre <- gets $ take 2
    case pre of
        []  -> pure $ Dir M.empty
        [_] -> pure $ Dir M.empty
        [cd, ls] -> do
            when (not $ isCd cd) (error $ "expected cd: " ++ show cd)
            when (not $ isLs ls) (error $ "expected ls: " ++ show ls)
            modify $ drop 2

            -- eat directory contents
            ents <- gets $ (toEntry `map`) . takeWhile isContent
            modify $ drop $ length ents
            let files = M.fromList ents

            subdirs <- parseCds

            let replace (k, d) f = M.adjust (const d) k f
                files' = foldr replace files subdirs

            pure $ Dir files'

    where
        parseCds :: State [Cmd] [(String, FileTree)]
        parseCds = do
            cmds <- get
            case cmds of
                []            -> pure []
                (Cd ".." : _) -> modify tail $> []
                (Cd chld : _) -> liftM2 ((:) . ((,) chld)) parseDir parseCds
        -- “Cds nuts” hehe


        isCd x = case x of (Cd _) -> True; _ -> False
        isLs x = case x of Ls     -> True; _ -> False

        isContent x = case x of (EchoDir  _)   -> True
                                (EchoFile _ _) -> True
                                _ -> False

        toEntry x = case x of (EchoDir    n) -> (n, Dir M.empty)
                              (EchoFile i n) -> (n, File i)
                              _ -> error "toFile: invalid argument"

        addToDir xs (Dir ys) = Dir $ xs `M.union` ys
        addToDir _ _ = error "joinDirs: invalid arguments"


sillySumLargeDirs :: Int -> FileTree -> Int
sillySumLargeDirs _ (File i) = 0
sillySumLargeDirs thres dir@(Dir sub) =
    foldr ((+) . sillySumLargeDirs thres) 0 sub
    + let total = size dir in if total <= thres then total else 0


sumLargeDirs :: Int -> FileTree -> Int
sumLargeDirs _ (File i) = 0
sumLargeDirs thres dir@(Dir sub) = let total = size dir in
    if total <= thres
    then total
    else foldr ((+) . sumLargeDirs thres) 0 sub


getLargeDirs :: Int -> FileTree -> Int
getLargeDirs thr (File i) = maxBound
getLargeDirs thr dir@(Dir sub) =
    let
        bigger = filter ((thr <=) . size) (M.elems sub)
        cur = minimum $ (maxBound :) . map size $ bigger
    in
        foldr (min . getLargeDirs thr) cur bigger


foldDirSizes :: (Int -> b -> b) -> b -> FileTree -> b
foldDirSizes _ b (File _) = b
foldDirSizes f b (Dir sub) = foldr (flip (foldDirSizes f))
                                   (foldr f b (map size dirs))
                                   dirs
    where
        dirs = filter isDir $ M.elems sub
        isDir x = case x of (Dir _) -> True; _ -> False


size :: FileTree -> Int
size (Dir files) = foldr ((+) . size) 0 files
size (File s) = s


perform :: Show a => (FileTree -> a) -> FilePath -> IO ()
perform f fileIn = getLines fileIn >>= print . f . evalState parseDir . tokenize


fstHalf :: FilePath -> IO ()
fstHalf = perform (sillySumLargeDirs 100000)


sndHalf :: FilePath -> IO ()
sndHalf = perform freeSpace
    where
        freeSpace t = let needed = (30000000 - (70000000 - size t))
                      in minimum $ filter (needed <=) $ foldDirSizes (:) [] t


main :: IO ()
main = putStrLn "day 07" >> putStrLn "\nfirst"  >> fstHalf "example.txt"
                                                >> fstHalf "input1.txt"
                         >> putStrLn "\nsecond" >> sndHalf "example.txt"
                                                >> sndHalf "input1.txt"
