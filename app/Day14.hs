{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Aoc
import Data.Attoparsec.Text (Parser, char, decimal, sepBy1', signed, skipSpace, string)
import Data.Massiv.Array (Ix2 (..))

data S = S {_p :: !Ix2, _q :: !Ix2}
  deriving (Eq, Show)

pIx2With :: Parser Int -> Parser Int -> Parser Ix2
pIx2With pX pY = do
  x <- pX
  _ <- char ','
  y <- pY
  pure $ x :. y

pS :: Parser S
pS = do
  _ <- string "p="
  p <- pIx2With decimal decimal
  _ <- string " v="
  q <- pIx2With (signed decimal) (signed decimal)
  pure $ S p q

pInput :: Parser [S]
pInput = pS `sepBy1'` skipSpace

main :: IO ()
main = do
  d <- parseChallengeT (Sample 14 1) pInput
  mapM_ print d
