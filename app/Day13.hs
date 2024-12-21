{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Main
  ( main,
  )
where

import Aoc
import Aoc.Parse (skipHorizontalSpace)
import Data.Attoparsec.Text (Parser, decimal, parseOnly, sepBy1', skipSpace, string)
import Data.Massiv.Array (Ix2 (..))
import qualified Data.Text as T

data Configuration = Configuration
  { buttonA :: !Ix2,
    buttonB :: !Ix2,
    prize :: !Ix2
  }
  deriving (Eq, Show)

-- >>> parseOnly (pButtonConfiguration 'A') "Button A: X+94, Y+34"
-- Right (94 :. 34)

pButtonConfiguration :: Char -> Parser Ix2
pButtonConfiguration c = do
  _ <- string "Button"
  _ <- skipHorizontalSpace
  _ <- string $ T.singleton c <> ":"
  _ <- skipHorizontalSpace
  _ <- string "X+"
  i <- decimal
  _ <- string ", Y+"
  j <- decimal
  pure $ i :. j

-- >>> parseOnly pPrize "Prize: X=8400, Y=5400"
-- Right (8400 :. 5400)

pPrize :: Parser Ix2
pPrize = do
  _ <- string "Prize: X="
  i <- decimal
  _ <- string ", Y="
  j <- decimal
  pure $ i :. j

pConfiguration :: Parser Configuration
pConfiguration = do
  a <- pButtonConfiguration 'A'
  _ <- skipSpace
  b <- pButtonConfiguration 'B'
  _ <- skipSpace
  p <- pPrize
  pure $ Configuration a b p

pInput :: Parser [Configuration]
pInput = pConfiguration `sepBy1'` skipSpace

main :: IO ()
main = do
  d <- parseChallengeT (Sample 13 1) pInput
  print d
