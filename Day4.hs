{-# LANGUAGE TupleSections #-}

module Day4 where

import qualified Data.IntSet as IS
import Data.List (partition, transpose)
import Data.Traversable (mapAccumL)

type Board = [[Int]]

day4 :: FilePath -> IO ()
day4 inputFile = do
  content <- readFile inputFile
  let inputLines = lines content
  let bingoes = fmap ri $ splitWhen (== ',') $ head inputLines
  let boards = splitWhen (== []) $ fmap (fmap ri . words) $ tail inputLines
  let solns = solve bingoes boards
  print $ head solns
  print $ last solns

ri :: String -> Int
ri = read

splitWhen :: (x -> Bool) -> [x] -> [[x]]
splitWhen p s = case dropWhile p s of
  [] -> []
  s' -> w : splitWhen p s''
    where
      (w, s'') = break p s'

solve :: [Int] -> [Board] -> [Int]
solve [] boards = []
solve (b:bg) boards = fmap (\x -> b * unmarkedSum x) winners <> solve bg losers
  where
    (winners, losers) = partition isBingoWinner (map (wipeBoard b) boards)

wipeBoard :: Int -> Board -> Board
wipeBoard c = fmap (fmap (\x -> if x == c then -1 else x))

isBingoWinner :: Board -> Bool
isBingoWinner b = any match b || any match (transpose b)
  where
    match = all (== -1)

unmarkedSum :: Board -> Int
unmarkedSum = sum . filter (/= -1). mconcat