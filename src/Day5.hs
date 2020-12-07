module Day5
  ( day5
  ) where

import Data.List (find, sort)
import Data.Maybe (fromJust, mapMaybe)
import Text.ParserCombinators.ReadP (ReadP, count, satisfy)
import Parse (parseMaybe)

day5 = do
  content <- readFile "input/d5"
  let xs = lines content
      ys = mapMaybe seat xs
      zs = sort ys

  print $ foldr max (head ys) $ tail ys
  print $ fst . fromJust
    $ find (\(x,y) -> x+1 == y) $ zip [head zs .. last zs] zs

type Seat = Int

seat :: String -> Maybe Seat
seat = parseMaybe seatParser

rowLetter :: ReadP Char
rowLetter = satisfy (\c -> c == 'F' || c == 'B')

columnLetter :: ReadP Char
columnLetter = satisfy (\c -> c == 'R' || c == 'L')

seatParser :: ReadP Seat
seatParser = do
  r <- row <$> count 7 rowLetter
  c <- column <$> count 3 columnLetter

  return $ r * 8 + c

row :: String -> Int
row = bsp (0,127) . map (== 'F')

column :: String -> Int
column = bsp (0,7) . map (== 'L')

bsp :: (Int,Int) -> [Bool] -> Int
bsp (l,h) (True:x:xs) = bsp (l, mid l h) (x:xs)
bsp (l,h) (False:x:xs) = bsp (mid l h + 1, h) (x:xs)
bsp (l, _) [True] = l
bsp (_, h) [False] = h

mid l h = (l + h) `quot` 2
