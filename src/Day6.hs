module Day6
  ( day6
  ) where

import Data.List (groupBy, nub, delete, foldl')
import Data.Maybe (mapMaybe)
import Data.Char (ord, digitToInt)
import Data.Bits ((.&.), complement, (.|.))

day6 = do
  content <- readFile "input/d6"
  let raw = lines content
      xs = map (delete "") $ groupBy (\_ y -> y /= "") raw
      xs' = map (map form) xs

  print $ sum $ map (length . collate) xs
  print $ sum $ map (length . collate') xs

  --print xs'
  print $ map (foldr (.|.) 0) xs'

form :: String -> Int
form xs = toDec $ concatMap show $ foldr f (replicate 26 0) xs where
  f x acc = (\(l,r) -> l ++ (1 : tail r)) $ splitAt (idx x) acc
  idx x = ord x - 97

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

collate :: (Eq a) => [[a]] -> [a]
collate = nub . concat

collate' :: (Eq a) => [[a]] -> [a]
collate' xss = collate $ map f xss where
  f xs = mapMaybe (\x -> if all (x `elem`) xss then Just x else Nothing) xs
