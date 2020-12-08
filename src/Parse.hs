module Parse
  ( letter
  , digit
  , space
  , parseMaybe
  , skipNonspaces
  , word
  , integer
  , signedInteger
  , (!^)) where

import Data.Char (isSpace, isAlpha, isDigit)
import Text.ParserCombinators.ReadP (munch1, (+++), char, get, look, ReadP, readP_to_S, satisfy, many1)
import Data.List (genericIndex)


letter :: ReadP Char
letter = satisfy isAlpha

digit :: ReadP Char
digit = satisfy isDigit

integer :: ReadP Integer
integer = read <$> munch1 isDigit

signedInteger :: ReadP Integer
signedInteger = posI +++ negI where
  posI = do char '+'; integer
  negI = do char '-'; (* (-1)) <$> integer

space :: ReadP Char
space = satisfy (== ' ')

word :: ReadP String
word = munch1 isAlpha

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

(!^) :: Integral b => [a] -> b -> a
(!^) = genericIndex
infixl 9 !^
