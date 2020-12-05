{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Day4 ( day4 ) where

import Data.List (groupBy)
import Data.Char (isDigit, isSpace, isAlpha)
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.ReadP

day4 = do
  content <- readFile "input/d4"
  let raw = lines content
      xs = map unwords $ groupBy (\_ y -> y /= "") raw

  print $ length $ mapMaybe passport xs

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
  case readP_to_S parser input of
    [] ->  Nothing
    ((result, _):_) -> Just result

data Passport = Passport
  { _byr :: Int
  , _iyr :: Int
  , _eyr :: Int
  , _hgt :: String
  , _hcl :: String
  , _ecl :: String
  , _pid :: String
  , _cid :: Maybe String
  } deriving (Show)

data PassportPart
  = Byr Int
  | Iyr Int
  | Eyr Int
  | Hgt String
  | Hcl String
  | Ecl String
  | Pid String
  | Cid (Maybe String)
  deriving (Show)

passport :: String -> Maybe Passport
passport = parseMaybe passportParser

passportParser :: ReadP Passport
passportParser = do
  vs <- count 8 parseAny <++ count 7 parseNoCid
  let p = foldr f (Passport {_cid = Nothing}) vs
      f (Byr v) p' = p' {_byr = v}
      f (Iyr v) p' = p' {_iyr = v}
      f (Eyr v) p' = p' {_eyr = v}
      f (Hgt v) p' = p' {_hgt = v}
      f (Hcl v) p' = p' {_hcl = v}
      f (Ecl v) p' = p' {_ecl = v}
      f (Pid v) p' = p' {_pid = v}
      f (Cid v) p' = p' {_cid = v}
  return p

letter :: ReadP Char
letter = satisfy isAlpha

digit :: ReadP Char
digit = satisfy isDigit

space :: ReadP Char
space = satisfy (== ' ')

byr :: ReadP PassportPart
byr = do
  optional space
  string "byr:"
  Byr <$> passportYearParser (\y -> y >= 1920 && y <= 2002)

iyr :: ReadP PassportPart
iyr = do
  optional space
  string "iyr:"
  Iyr <$> passportYearParser (\y -> y >= 2010 && y <= 2020)

eyr :: ReadP PassportPart
eyr = do
  optional space
  string "eyr:"
  Eyr <$> passportYearParser (\y -> y >=2020 && y <= 2030)

hgt :: ReadP PassportPart
hgt = do
  optional space
  string "hgt:"
  n <- read <$> many1 digit
  unit <- count 2 letter
  Hgt <$> case unit of
    "cm" | n >= 150 && n <= 193 -> return $ show n ++ "cm"
    "in" | n >= 59 && n <= 76 -> return $ show n ++ "in"
    _ -> pfail

hcl :: ReadP PassportPart
hcl = do
  optional space
  string "hcl:#"
  Hcl <$> count 6 (digit <++ satisfy (\c -> c >= 'a' && c <= 'f'))

ecl :: ReadP PassportPart
ecl = do
  optional space
  string "ecl:"
  Ecl <$> choice
    (map string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

pid :: ReadP PassportPart
pid = do
  optional space
  string "pid:"
  Pid <$> count 9 digit

cid :: ReadP PassportPart
cid = do
  optional space
  string "cid:"
  skipNonspaces
  return $ Cid Nothing

skipNonspaces :: ReadP ()
skipNonspaces = do
  s <- look
  skip s
  where skip (c:s) | (not . isSpace) c = do _ <- get; skip s
        skip _ = do return ()

parseAny :: ReadP PassportPart
parseAny = byr <++ iyr <++ eyr <++ hgt <++ hcl <++ ecl <++ pid <++ cid

parseNoCid :: ReadP PassportPart
parseNoCid = byr <++ iyr <++ eyr <++ hgt <++ hcl <++ ecl <++ pid

passportYearParser :: (Int -> Bool) -> ReadP Int
passportYearParser cond = do
  year <- read <$> count 4 digit
  if cond year then return year else pfail
