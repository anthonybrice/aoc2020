module Day9
  ( day9
  ) where

import Data.List (tails, inits)

day9 = do
  content <- readFile "input/d9"
  let xs = read <$> lines content
      n = xmas (take 25 xs) (drop 25 xs)

  print n
  print $ xmas' n xs

xmas :: [Integer] -> [Integer] -> Integer
xmas ps (q:qs)
  | null [ p + p' | p <- ps, p' <- ps, q == p + p', p /= p' ] = q
  | otherwise = xmas (tail ps ++ [q]) qs

xmas' :: Integer -> [Integer] -> Integer
xmas' n xs = minimum ys + maximum ys where
  ys:_ = [ ys' | ys <- tails xs, ys' <- inits ys, sum ys' == n ]
