module Day9
  ( day9
  ) where

day9 = do
  content <- readFile "input/d9"
  let xs = read <$> lines content

  print $ xmas (take 25 xs) (drop 25 xs)

-- >>> day9

xmas :: [Integer] -> [Integer] -> Integer
xmas ps (q:qs)
  | null [ p + p' | p <- ps, p' <- ps, q == p + p', p /= p' ] = q
  | otherwise = xmas (tail ps ++ [q]) qs
