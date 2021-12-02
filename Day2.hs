{-# LANGUAGE RecordWildCards #-}

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

printAns :: Submarine -> IO ()
printAns s = print s >> print (position s * depth s)

day2 :: FilePath -> IO ()
day2 inputFile = do
  content <- readFile inputFile
  let xs = map words $ lines content
  printAns $ solve match1 xs initSubmarine
  printAns $ solve match2 xs initSubmarine

-- Maps data to a list of fn transformations, then applies them left-to-right
-- Uses the Endo and Dual monoids to collapse transformations in the right order
solve :: (a -> b -> b) -> [a] -> b -> b
solve match xs = appEndo . getDual $ mconcat $ map (Dual . Endo . match) xs

ri :: String -> Int
ri = read

-- Transforms for part 1
match1 :: [String] -> Submarine -> Submarine
match1 ["forward", n] s@Submarine {..} = s { position = position + ri n }
match1 ["down", n] s@Submarine {..} = s { depth = depth + ri n }
match1 ["up", n] s@Submarine {..} = s { depth = depth - ri n }
match1 _ s = s

-- Transforms for part 2
match2 :: [String] -> Submarine -> Submarine
match2 ["forward", n] s@Submarine {..} = s { position = position + ri n , depth = depth + aim * ri n}
match2 ["down", n] s@Submarine {..} = s { aim = aim + ri n }
match2 ["up", n] s@Submarine {..} = s { aim = aim - ri n }
match2 _ s = s