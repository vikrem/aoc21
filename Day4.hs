{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Day4 where

import Data.Foldable (find)
import qualified Data.IntSet as IS
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Traversable (mapAccumR, mapAccumL)

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

--solve :: [Int] -> [Board] -> [IS.IntSet]
solve :: [Int] -> [Board] -> Int
solve bingoes boards = unmarkedSum is winner * (bingoes !! (idx - 1))
  where
    (final, accum) = mapAccumL (\mp x -> (IS.insert x mp, mp)) IS.empty bingoes
    bingoSetList = accum <> [final]
    bingoSetListIdxed = zip [0..] bingoSetList
    (idx, is, winner) = head . catMaybes $ fmap (\(idx, is) -> fmap (idx, is, ) $ find (isBingoWinner is) boards) bingoSetListIdxed 

isBingoWinner :: IS.IntSet -> Board -> Bool
isBingoWinner is b = any match b || any match (transpose b)
  where
    match row = IS.isSubsetOf (IS.fromList row) is

unmarkedSum :: IS.IntSet -> Board -> Int
unmarkedSum is b = sum $ IS.toList $ IS.difference (IS.fromList $ mconcat b) is

isPred :: (a -> Bool) -> [a] -> [Maybe a]
isPred p = fmap (\x -> if p x then Just x else Nothing)