{-# LANGUAGE RecordWildCards #-}

module Day3 where

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.Foldable (Foldable (foldl'))

day3 :: FilePath -> IO ()
day3 inputFile = do
  content <- readFile inputFile
  let xs = fmap (== '1') <$> lines content
  print $ solve xs
  print $ solve2 xs

solve :: [[Bool]] -> Int
solve xxs = bitsToInt a * bitsToInt b
  where
    tsp = transpose xxs
    n = length xxs
    countedBits = fmap countSet tsp
    a = fmap (isMaj n) countedBits
    b = fmap (not . isMaj n) countedBits

solve2 :: [[Bool]] -> Int
solve2 xxs =
  product $
    fmap
      bitsToInt
      [ cycler 0 isMaj xxs,
        cycler 0 ((not .) . isMaj) xxs
      ]

transpose :: [[b]] -> [[b]]
transpose = getZipList . traverse ZipList

cycler :: Int -> (Int -> Int -> Bool) -> [[Bool]] -> [Bool]
cycler idx f xxs = case filter (\xs -> (xs !! idx) == desired) xxs of
  [] -> error "wtf"
  [x] -> x
  xxs' -> cycler (idx + 1) f xxs'
  where
    n = length xxs
    desired = f n . countSet $ (transpose xxs !! idx)

countSet :: [Bool] -> Int
countSet = length . filter id

isMaj :: (Ord a, Num a) => a -> a -> Bool
isMaj h n = n * 2 >= h

bitsToInt :: [Bool] -> Int
bitsToInt = foldl' (\acc d -> acc * 2 + d) 0 . fmap btd
  where
    btd True = 1
    btd _ = 0