module Day3 ( day3 ) where

import Data.List (tails, mapAccumR)
import Data.List.HT (sieve)

day3 = do
  content <- readFile "input/d3"
  let raw = lines content
      xs = map cycle raw
  print $ p1 xs
  print $ p2 xs

p1 = toboggan 3 1

toboggan r d xs = length $ filter (== '#') ys where
   ys' = tail $ sieve d xs
   (_, ys) = mapAccumR f (r * length ys') ys'
   f r' e = (r'-r, e!!fromIntegral r')

p2 xs = product $
  map (\(r,d) -> toboggan r d xs) [(1,1), (3,1), (5,1), (7,1), (1,2)]