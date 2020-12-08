{-# LANGUAGE TupleSections #-}

module Day8
  ( day8
  ) where

import Text.ParserCombinators.ReadP (pfail, ReadP)
import Parse (space, signedInteger, word, parseMaybe, (!^))
import Data.Maybe (mapMaybe, catMaybes)
import Data.Either (fromLeft, rights)

day8 = do
  content <- readFile "input/d8"
  let raw = lines content
      xs = mapMaybe instruction raw
      y:_ = rights $ catMaybes $ tryFix Jmp Nop xs ++ tryFix Nop Jmp xs

  print $ fromLeft undefined $ boot xs
  print y

type BootCode = [Instruction]

data Instruction = Instruction Op Arg
  deriving Show

data Op = Acc | Jmp | Nop
  deriving (Show, Eq)

type Arg = Integer

type Exception = Integer

type BootResult = Either Exception Integer

tryFix :: Op -> Op -> BootCode -> [Maybe BootResult]
tryFix oldOp newOp is = zipWith (curry f) [0..] is where
  f (idx, Instruction op arg)
    | op == oldOp = Just $ boot $ take idx is ++ Instruction newOp arg : drop (idx+1) is
    | otherwise = Nothing

boot :: BootCode -> BootResult
boot = boot' 0 0 . map (False,)

boot'
  :: Integer
  -> Integer
  -> [(Bool, Instruction)]
  -> BootResult
boot' idx acc is
  | fromIntegral idx == length is = Right acc
  | fst $ is !^ idx = Left acc
  | otherwise =
  let (_, i@(Instruction op arg)) = is !^ idx
      visit = take idx' is ++ (True, i) : drop (idx'+1) is
      idx' = fromIntegral idx
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
