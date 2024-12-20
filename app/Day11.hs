module Main
  ( main,
  )
where

import Aoc
import Aoc.Function (nTimesStrict)
import Aoc.Parse (skipHorizontalSpace)
import Data.Attoparsec.Text (Parser, decimal, sepBy1')

type Stone = Int

type Stones = [Int]

-- >>> blink 2024
-- [20,24]

blink :: Stone -> [Stone]
blink 0 = [1]
blink x
  | even nDigits = map read [take n2 xStr, drop n2 xStr]
  | otherwise = [x * 2024]
  where
    xStr = show x
    nDigits = length xStr
    n2 = nDigits `div` 2

pInput :: Parser Stones
pInput = decimal `sepBy1'` skipHorizontalSpace

main :: IO ()
main = do
  d <- parseChallengeT (Full 11) pInput
  print $ length $ nTimesStrict 25 (concatMap blink) d
