{-# LANGUAGE TupleSections #-}

module Day5 where

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.List (group, groupBy, sort)
import Text.Parsec hiding (Line)
import Text.Parsec.String

type Line = ((Int, Int), (Int, Int))

type Point = (Int, Int)

day5 :: FilePath -> IO ()
day5 inputFile = do
  content <- readFile inputFile
  let inputLines = lines content
  let parses = parse lineParser "day5" <$> inputLines
  let (Right parsed) = sequenceA parses
  print $ lineToPoints True <$> parsed
  print $ solve (lineToPoints False) parsed
  print $ solve (lineToPoints True) parsed

lineParser :: Parser Line
lineParser = do
  x1 <- many1 digit
  char ','
  y1 <- many1 digit
  spaces
  string "->"
  spaces
  x2 <- many1 digit
  char ','
  y2 <- many1 digit
  pure ((dtoi x1, dtoi y1), (dtoi x2, dtoi y2))

dtoi :: [Char] -> Int
dtoi = foldl' (\a x -> a * 10 + digitToInt x) 0

walk :: Int -> Int -> [Int]
walk a b
  | a <= b = [a .. b]
  | otherwise = [a, a -1 .. b]

lineToPoints :: Bool -> Line -> [Point]
lineToPoints diag ((x1, y1), (x2, y2))
  | x1 == x2 = (x1,) <$> walk y1 y2
  | y1 == y2 = (,y1) <$> walk x1 x2
  | diag = getZipList $ (,) <$> ZipList (walk x1 x2) <*> ZipList (walk y1 y2)
  | otherwise = []

solve :: (Line -> [Point]) -> [Line] -> Int
solve f = length . filter (>= 2) . fmap length . group . sort . concatMap f