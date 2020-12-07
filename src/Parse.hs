module Parse
  ( letter
  , digit
  , space
  , parseMaybe
  , skipNonspaces
  , word
  , integer
  ) where

import Data.Char (isSpace, isAlpha, isDigit)
import Text.ParserCombinators.ReadP (get, look, ReadP, readP_to_S, satisfy, many1)


letter :: ReadP Char
letter = satisfy isAlpha

digit :: ReadP Char
digit = satisfy isDigit

integer :: ReadP Integer
integer = read <$> many1 digit

space :: ReadP Char
space = satisfy (== ' ')

word :: ReadP String
word = many1 letter

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
  case readP_to_S parser input of
    (result, []) : _ -> Just result
    _ -> Nothing

skipNonspaces :: ReadP ()
skipNonspaces = do
  s <- look
  skip s
  where skip (c:s) | (not . isSpace) c = do _ <- get; skip s
        skip _ = do return ()
