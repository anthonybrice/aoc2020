module Day6
  ( day6
  ) where

import Data.List (groupBy, nub, delete)
import Data.Maybe (mapMaybe)

day6 = do
  content <- readFile "input/d6"
  let raw = lines content
      xs = map (delete "") $ groupBy (\_ y -> y /= "") raw

  print $ sum $ map (length . collate) xs
  print $ sum $ map (length . collate') xs

collate :: (Eq a) => [[a]] -> [a]
collate = nub . concat

collate' :: (Eq a) => [[a]] -> [a]
collate' xss = collate $ map f xss where
  f xs = mapMaybe (\x -> if all (x `elem`) xss then Just x else Nothing) xs
