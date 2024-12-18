{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Aoc
import Aoc.Parse (skipHorizontalSpace)
import Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy1', string)

data Op = P | M
  deriving (Show)

data Input = Input !Int ![Int]
  deriving (Show)

pInput :: Parser [Input]
pInput = pOne `sepBy1'` endOfLine
  where
    pOne = Input <$> (decimal <* string ": ") <*> decimal `sepBy1'` skipHorizontalSpace

main :: IO ()
main = do
  d <- parseChallengeT (Sample 7 1) pInput
  print d
