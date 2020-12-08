{-# LANGUAGE TupleSections #-}

module Day8
  ( day8
  ) where

import Text.ParserCombinators.ReadP (pfail, ReadP)
import Parse (space, signedInteger, word, parseMaybe, (!^))
import Data.Maybe (mapMaybe)
import Data.Either (fromLeft)

day8 = do
  content <- readFile "input/d8"
  let raw = lines content
      xs = mapMaybe instruction raw

  print $ fromLeft undefined $ boot xs

type BootCode =  [Instruction]

data Instruction = Instruction Op Arg
  deriving Show

data Op = Acc | Jmp | Nop
  deriving Show

type Arg = Integer

type Exception = Integer

boot :: BootCode -> Either Exception ()
boot code = boot' 0 0 $ map (False,) code

boot'
  :: Integer
  -> Integer
  -> [(Bool, Instruction)]
  -> Either Exception ()
boot' idx acc is | fst $ is !^ idx = Left acc
boot' _ _ [] = Right ()
boot' idx acc is =
  let (_, i@(Instruction op arg)) = is !^ idx
      visit = take idx' is ++ (True, i) : drop (idx'+1) is
      idx' = fromInteger idx
  in case op of
    Acc -> boot' (idx+1) (acc+arg) visit
    Jmp -> boot' (idx+arg) acc visit
    Nop -> boot' (idx+1) acc visit


instruction :: String -> Maybe Instruction
instruction = parseMaybe instructionParser

instructionParser :: ReadP Instruction
instructionParser = do
  op <- opParser
  space
  Instruction op <$> argParser

opParser :: ReadP Op
opParser = do
  s <- word
  case s of
    "nop" -> return Nop
    "acc" -> return Acc
    "jmp" -> return Jmp
    _ -> pfail

argParser :: ReadP Arg
argParser = signedInteger
