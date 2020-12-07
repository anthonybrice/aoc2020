module Day6
  ( day6
  ) where

import Data.List (groupBy, nub, delete)

day6 = do
  content <- readFile "input/d6"
  let raw = lines content
      xs = map (delete "") $ groupBy (\_ y -> y /= "") raw

  print $ sum $ map (length . collate) xs
  print $ sum $ map (length . collate') xs

collate :: Eq a => [[a]] -> [a]
collate = nub . concat

collate' :: Eq a => [[a]] -> [a]
collate' xss = collate $ map (\xs -> [x | x <- xs, all (x `elem`) xss]) xss
