module Day2 ( day2 ) where

import Data.Either (rights)
import Data.List.Split (splitOn)

day2 = do
  content <- readFile "input/d2"
  let raw = lines content
      xs = map f raw
  print $ length $ filter valid xs
  print $ length $ filter valid' xs

  where
    f x = let [y1, y2, y3] = words x
              [p,q] = map read $ splitOn "-" y1
          in ((p,q), head y2, y3)

    valid ((p,q), c, pw) =
      let n = length $ filter (== c) pw in p <= n && n <= q

    valid' ((p,q), c, pw) = (g p' == c || g q' == c) && (g p' /= c || g q' /= c)
      where p' = p-1; q' = q-1; g = (pw!!)
