{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Day2 where

import Data.Monoid ( Dual(Dual, getDual), Endo(Endo, appEndo) )

data Submarine = Submarine
  { position :: Int
  , depth :: Int
  , aim :: Int
  }
  deriving (Eq, Ord, Show)

initSubmarine :: Submarine
initSubmarine = Submarine 0 0 0

day2 :: FilePath -> IO ()
day2 inputFile = do
  content <- readFile inputFile
  let xs = map words $ lines content
  let p1 = solve match1 xs initSubmarine
  print p1
  print $ position p1 * depth p1
  let p2 = solve match2 xs initSubmarine
  print p2
  print $ position p2 * depth p2

-- Maps data to a list of fn transformations, then applies them left-to-right
-- Uses the Endo and Dual monoids to collapse transformations in the right order
solve :: (a -> b -> b) -> [a] -> b -> b
solve match xs = appEndo . getDual $ mconcat $ map (Dual . Endo . match) xs

ri :: String -> Int
ri = read

-- Transforms for part 1
match1 :: [String] -> Submarine -> Submarine
match1 ["forward", n] s@Submarine {..} =
  s { position = position + ri n }
match1 ["down", n] s@Submarine {..} =
  s { depth = depth + ri n }
match1 ["up", n] s@Submarine {..} =
  s { depth = depth - ri n }
match1 _ s = s

-- Transforms for part 2
match2 :: [String] -> Submarine -> Submarine
match2 ["forward", n] s@Submarine {..} =
  let i = ri n
   in s { position = position + i , depth = depth + aim * i}
match2 ["down", n] s@Submarine {..} =
  s { aim = aim + ri n }
match2 ["up", n] s@Submarine {..} =
  s { aim = aim - ri n }
match2 _ s = s