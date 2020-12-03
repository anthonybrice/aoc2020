module Day1
  ( day1
  ) where

import Data.List (tails)

day1 = do
  content <- readFile "input/d1"
  let xs' = words content
      xs = map read xs'
  print $ d1p1 xs
  print $ d1p2 xs

d1p1 xs =
  case [ x * y | x:ys <- tails xs, y <- ys, x + y == 2020 ] of x:_ -> x; _ -> 0

d1p2 xs =
  case [ x * y * z
       | x:ys <- tails xs, y:zs <- tails ys, z <- zs, x + y + z == 2020 ]
  of x:_ -> x
     _ -> 0
