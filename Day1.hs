{-# LANGUAGE TypeApplications #-}
module Day1 where


day1 :: FilePath -> IO ()
day1 inputFile = do
    content <- readFile inputFile
    let xs = map (read @Int) $ lines content
    print $ solve 1 xs
    print $ solve 3 xs

solve :: Ord a => Int -> [a] -> Int
solve n xs = length . filter id $ zipWith (<) xs $ drop n xs