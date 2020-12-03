module Day2
  ( day2
  ) where

import Data.Text (pack, splitOn, unpack)

day2 = do
  content <- readFile "input/d2"
  let raw = lines content
      xs = map f raw
  print $ length $ filter valid xs
  print $ length $ filter valid' xs

  where
    f x = let [y1, y2, y3] = words x
              [p,q] = map (read . unpack) $ splitOn (pack "-") $ pack y1
          in ((p,q), y2!!0, y3)

    valid ((p,q), c, pw) =
      let n = length $ filter (== c) pw
      in p <= n && n <= q

    valid' ((p,q), c, pw) = (pw!!(p-1) == c || pw!!(q-1) == c)
      && not (pw!!(p-1) == c && pw!!(q-1) == c)
