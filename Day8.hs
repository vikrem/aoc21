module Day8 where

import Control.Monad (guard)
import Data.List (foldl', permutations, sort)
import qualified Data.Map.Strict as M
import Util (splitWhen)

day8 :: FilePath -> IO ()
day8 inputFile = do
  content <- readFile inputFile
  let linedOutput = fmap (fmap words . splitWhen (== '|')) $ lines content
  print $ solve linedOutput
  let p2 = solve2 <$> linedOutput
  print p2
  print $ sum . concat $ p2

solve :: [[[String]]] -> Int
solve line =
  sum $
    fmap
      ( \[lhs, rhs] ->
          length $ filter (include . length) rhs
      )
      line
  where
    include = (`elem` [2, 3, 4, 7])

baseMap :: M.Map String Int
baseMap =
  M.fromList $
    zip
      [ "abcefg",
        "cf",
        "acdeg",
        "acdfg",
        "bcdf",
        "abdfg",
        "abdefg",
        "acf",
        "abcdefg",
        "abcdfg"
      ]
      [0 .. 9]

allMappings :: [M.Map Char Char]
allMappings = fmap (M.fromList . zip "abcdefg") $ permutations "abcdefg"

dtoi :: [Int] -> Int
dtoi = foldl' (\acc x -> acc * 10 + x) 0

solve2 :: [[String]] -> [Int]
solve2 [lhs, rhs] = do
  tryMapping <- allMappings
  let mapped = fmap $ sort . fmap (tryMapping M.!)
  let lhs' = mapped lhs
  let rhs' = mapped rhs
  guard $ all (`M.member` baseMap) lhs'
  pure $ dtoi $ (baseMap M.!) <$> rhs'
solve2 _ = [0]
