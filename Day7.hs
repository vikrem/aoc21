{-# LANGUAGE TypeApplications #-}

module Day7 where

import Data.List (sort)
import Util (splitWhen)

day7 :: FilePath -> IO ()
day7 inputFile = do
  content <- readFile inputFile
  let input = fmap (read @Int) $ splitWhen (== ',') content
  print $ solve input
  print $ solve2 input

solve :: (Num a, Ord a) => [a] -> a
solve xs = sum $ fmap (\x -> abs $ x - median xs) xs

solve2 :: (Foldable t, Functor t, Integral a) => t a -> a
solve2 xs = minimum $ fmap (\target -> sum $ fmap (\x -> rangeSum $ abs $ x - target) xs) [minimum xs .. maximum xs]

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

rangeSum :: Integral a => a -> a
rangeSum n = n * (n + 1) `div` 2