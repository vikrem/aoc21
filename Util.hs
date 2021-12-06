module Util
  ( splitWhen,
  )
where

splitWhen :: (x -> Bool) -> [x] -> [[x]]
splitWhen p s = case dropWhile p s of
  [] -> []
  s' -> w : splitWhen p s''
    where
      (w, s'') = break p s'