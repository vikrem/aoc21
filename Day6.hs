{-# LANGUAGE TypeApplications #-}

module Day6 where

import Data.Array.Unboxed
  ( UArray,
    accumArray,
    array,
    assocs,
    elems,
  )
import Data.List (group, sort)
import Util (splitWhen)

-- Array maps from fish timer to number of fish alive
type FishArray = UArray Int Int

day6 :: FilePath -> IO ()
day6 inputFile = do
  content <- readFile inputFile
  let input = fmap (read @Int) $ splitWhen (== ',') content
  let inpArray = array @UArray (0, 8) $ fmap (\k -> (head k, length k)) $ group $ sort input
  print $ solve 80 inpArray
  print $ solve 256 inpArray

solve :: Int -> FishArray -> Int
solve k = sum . elems . (!! k) . iterate fish

fish :: FishArray -> FishArray
fish = accumArray (+) 0 (0, 8) . concatMap live . assocs
  where
    live (0, k) = [(6, k), (8, k)]
    live (n, k) = [(n -1, k)]