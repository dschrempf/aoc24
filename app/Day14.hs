{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Aoc
import Aoc.Function (nTimesStrict)
import Control.DeepSeq (NFData)
import Data.Attoparsec.Text (Parser, char, decimal, sepBy1', signed, skipSpace, string)
import Data.Foldable (Foldable (..))
import Data.List (sortOn)
import Data.Massiv.Array (Ix2 (..))
import GHC.Generics (Generic)

type P = Ix2

type Q = Ix2

data S = S {_p :: !P, _q :: !Q}
  deriving (Eq, Show, Generic)

instance NFData S

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

move :: Int -> Int -> P -> Q -> P
move xMax yMax (x :. y) (vx :. vy) = (x + vx) `mod` xMax :. (y + vy) `mod` yMax

moveS :: Int -> Int -> S -> S
moveS xMax yMax (S p q) = S (move xMax yMax p q) q

count :: Int -> Int -> [Ix2] -> (Int, Int, Int, Int)
count xMax yMax = foldl' addRobot (0, 0, 0, 0)
  where
    xH = xMax `div` 2
    yH = yMax `div` 2
    addRobot (tl, tr, bl, br) (x :. y) =
      case (x `compare` xH, y `compare` yH) of
        (LT, LT) -> (tl + 1, tr, bl, br)
        (GT, LT) -> (tl, tr + 1, bl, br)
        (LT, GT) -> (tl, tr, bl + 1, br)
        (GT, GT) -> (tl, tr, bl, br + 1)
        (_, _) -> (tl, tr, bl, br)

eval :: Int -> Int -> [S] -> Int
eval xMax yMax = (\(tl, tr, bl, br) -> tl * tr * bl * br) . count xMax yMax . map _p

next :: Int -> Int -> [S] -> [S]
next xMax yMax = map (moveS xMax yMax)

main :: IO ()
main = do
  -- d <- parseChallengeT (Sample 14 1) pInput
  d <- parseChallengeT (Full 14) pInput
  let -- Sample.
      -- (xMax, yMax) = (11, 7)
      -- -- Full.
      (xMax, yMax) = (101, 103)
      m100 = nTimesStrict 100 (moveS xMax yMax)
      d' = map m100 d
  print $ (\(tl, tr, bl, br) -> tl * tr * bl * br) $ count xMax yMax $ map _p d'
  let ss = iterate (next xMax yMax) d
  mapM_ print $ take 50 $ sortOn snd $ zip [0 :: Int ..] $ map (eval xMax yMax) $ take 10000 ss
