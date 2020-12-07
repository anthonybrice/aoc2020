module Day7
  ( day7
  ) where

import Data.Maybe (mapMaybe)
import Text.ParserCombinators.ReadP (many1, char, ReadP, string, (+++), optional)
import Parse (parseMaybe, word, space, integer)

day7 = do
  content <- readFile "input/d7"
  let xs = lines content
      ys = mapMaybe bagRule xs
      zs = map (flip bagTree ys . (\(BagRule b _) -> b)) ys

  print $ length $ filter canContainShinyGoldBag zs

data Bag = Bag BagDesc BagColor
  deriving (Show, Eq)

type BagDesc = String

type BagColor = String

data BagRule = BagRule Bag [BagContains]
  deriving (Show)

data BagContains = BagContains Integer Bag
  deriving (Show)

bagRule :: String -> Maybe BagRule
bagRule = parseMaybe bagRuleParser

bagRuleParser :: ReadP BagRule
bagRuleParser = do
  b <- bagParser
  string " contain "
  BagRule b <$> bagContainsParser

bagParser :: ReadP Bag
bagParser = do
  d <- bagDescParser
  space
  c <- bagColorParser
  string " bag"
  optional $ char 's'
  return $ Bag d c

bagDescParser :: ReadP BagDesc
bagDescParser = word

bagColorParser :: ReadP BagColor
bagColorParser = word

bagContainsParser :: ReadP [BagContains]
bagContainsParser = f +++ g where
  f = do
    string "no other bags."
    return []
  g = do
    bcs <- many1 g'
    char '.'
    return bcs
  g' = do
    n <- integer
    space
    b <- bagParser
    optional $ string ", "
    return $ BagContains n b

data BagTree = BagTree Bag [BagContainsTree]
  deriving (Show)

data BagContainsTree = BagContainsTree Integer BagTree
  deriving (Show)

bagTree :: Bag -> [BagRule] -> BagTree
bagTree b rs =
  let (BagRule _ bcs):_ = filter (\(BagRule b' _) -> b == b') rs
      bts = map (\(BagContains i b') -> BagContainsTree i $ bagTree b' rs) bcs
  in BagTree b bts

canContainShinyGoldBag :: BagTree -> Bool
canContainShinyGoldBag (BagTree _ bcs) = any f bcs where
  f (BagContainsTree _ bt@(BagTree b _)) =
    b == shinyGoldBag || canContainShinyGoldBag bt

shinyGoldBag = Bag "shiny" "gold"
