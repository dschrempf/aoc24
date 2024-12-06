module Main
  ( main,
  )
where

import Aoc
import Aoc.Parse (skipHorizontalSpace)
import Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy1')
import Data.List (sort)

type Report = [Int]

pReport :: Parser Report
pReport = decimal `sepBy1'` skipHorizontalSpace

pInput :: Parser [Report]
pInput = pReport `sepBy1'` endOfLine

isMonotone :: Report -> Bool
isMonotone r = r == rs || reverse r == rs
  where
    rs = sort r

isAdjacent :: Report -> Bool
isAdjacent r = all (\x -> x >= 1 && x <= 3) $ map abs $ zipWith (-) r (tail r)

isSafe1 :: Report -> Bool
isSafe1 r = isMonotone r && isAdjacent r

drop1 :: [a] -> [[a]]
drop1 [] = [[]]
drop1 (x : xs) = xs : map (x :) (drop1 xs)

isSafe2 :: Report -> Bool
isSafe2 r = any isSafe1 $ drop1 r

main :: IO ()
main = do
  d <- parseChallengeT (Full 2) pInput
  print $ length $ filter isSafe1 d
  print $ length $ filter isSafe2 d
