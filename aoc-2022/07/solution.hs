#!/usr/bin/env -S runhaskell -i../

module Main where


import AoCUtils ( getLines, splitOn )

import Control.Monad
import Data.Functor
import Control.Monad.State
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
    let (cd, ls) = (pre !! 0, pre !! 1)
    when (not $ isCd cd) (error $ "expected cd: " ++ show cd)
    when (not $ isLs ls) (error $ "expected ls: " ++ show ls)
    modify $ drop 2
    -- eat directory contents
    ents <- gets $ (toEntry `map`) . takeWhile isContent
    modify $ drop $ length ents
    let ment = M.fromList ents
    -- continue
    cmds <- get
    case cmds of
        []            -> pure $ Dir ment
        (Cd ".." : _) -> modify tail $> Dir ment
        (Cd chld : _) -> do dir <- parseDir
                            let ment' = M.adjust (const dir) chld ment
                            cmds' <- get
                            case cmds' of
                                []            -> pure $ Dir ment'
                                (Cd ".." : _) -> modify tail $> Dir ment'
                                _ -> addToDir ment' <$> parseDir
        (e : _) -> error $ "parseDir: unexpected " ++ show e
    where
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


fstHalf :: FilePath -> IO ()
fstHalf fileIn = do
    ls <- getLines fileIn
    let cmds = tokenize ls
        tree = evalState parseDir cmds
    forM_ cmds print
    print tree
    pure ()


sndHalf :: FilePath -> IO ()
sndHalf fileIn = do
    ls <- getLines fileIn
    pure ()


main :: IO ()
main = putStrLn "day 07" >> putStrLn "\nfirst"  >> fstHalf "example.txt"
                                                >> fstHalf "input1.txt"
                         >> putStrLn "\nsecond" >> sndHalf "example.txt"
                                                >> sndHalf "input1.txt"
