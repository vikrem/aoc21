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
  print $ solve bingoes boards

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
solve bingoes boards = solveBingo bingoSetList boards
  where
    (final, accum) = mapAccumL (\mp x -> (IS.insert x mp, mp)) IS.empty bingoes
    bingoSetList = zip bingoes $ tail $ accum <> [final]

solveBingo :: [(Int, IS.IntSet)] -> [Board] -> [Int]
solveBingo [] bss = []
solveBingo ((call, is) : iss) bss =
  let (winners, losers) = partition (isBingoWinner is) bss
   in map (\w -> call * unmarkedSum is w) winners <> solveBingo iss losers

isBingoWinner :: IS.IntSet -> Board -> Bool
isBingoWinner is b = any match b || any match (transpose b)
  where
    match row = IS.isSubsetOf (IS.fromList row) is

unmarkedSum :: IS.IntSet -> Board -> Int
unmarkedSum is b = sum $ IS.toList $ IS.difference (IS.fromList $ mconcat b) is

winnersPerBingo :: [(Int, IS.IntSet)] -> [Board] -> [(Int, IS.IntSet, [Board])]
winnersPerBingo calls bs = fmap (\(c, is) -> (c,is,) $ filter (isBingoWinner is) bs) calls