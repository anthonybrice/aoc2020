{-# LANGUAGE OverloadedStrings #-}

module Day2
  ( day2
  ) where

import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.Either (rights)

day2 = do
  content <- readFile "input/d2"
  let raw = map T.pack $ lines content
      xs = map f raw
  print $ length $ filter valid xs
  print $ length $ filter valid' xs

  where
    f x = let [y1, y2, y3] = T.words x
              (p,_):(q,_):_ = rights $ map decimal $ T.splitOn "-" y1
          in ((p,q), T.index y2 0, y3)

    valid ((p,q), c, pw) =
      let n = T.length $ T.filter (== c) pw in p <= n && n <= q

    valid' ((p,q), c, pw) = (g p' == c || g q' == c) && (g p' /= c || g q' /= c)
      where p' = p-1; q' = q-1; g = T.index pw
