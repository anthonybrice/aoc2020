module Lib
    ( someFunc
    ) where

import Data.Text (pack, splitOn, unpack)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

day1 = do
  content <- readFile "input/d1"
  let xs' = words content
      xs = map read xs'
  print $ d1p1 xs
  print $ d1p2 xs

d1p1 xs = case [ x * y | x <- xs, y <- xs, x + y == 2020 ] of (x:_) -> x; _ -> 0

d1p2 xs = case [ x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020 ]
          of x:_ -> x
             _ -> 0

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
