module Day8 where

import Util (splitWhen)

day8 :: FilePath -> IO ()
day8 inputFile = do
  content <- readFile inputFile
  let linedOutput = fmap (fmap words . splitWhen (== '|')) $ lines content
  print $ solve linedOutput

solve :: [[[String]]] -> Int
solve line =
  sum $
    fmap
      ( \[lhs, rhs] ->
          length $ filter (include . length) rhs
      )
      line
  where
    include 2 = True
    include 3 = True
    include 4 = True
    include 7 = True
    include _ = False