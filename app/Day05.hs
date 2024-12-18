module Main
  ( main,
  )
where

import Aoc
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy1')
import Data.List (sortBy)
import Data.Set (Set, fromList, member)

data Input = Input ![(Int, Int)] ![[Int]]
  deriving (Show)

pOrder :: Parser (Int, Int)
pOrder = (,) <$> (decimal <* char '|') <*> decimal

pUpdate :: Parser [Int]
pUpdate = decimal `sepBy1'` char ','

pInput :: Parser Input
pInput =
  Input
    <$> (pOrder `sepBy1'` endOfLine <* endOfLine <* endOfLine)
    <*> pUpdate `sepBy1'` endOfLine

customOrdering :: Set (Int, Int) -> Int -> Int -> Ordering
customOrdering order a b
  | isLower = LT
  | isGreater = GT
  | otherwise = EQ
  where
    isLower = (a, b) `member` order
    isGreater = (b, a) `member` order

middle :: [a] -> a
middle [] = error "empty list"
middle xs
  | even l = error "even length"
  | otherwise = xs !! ((l - 1) `div` 2)
  where
    l = length xs

main :: IO ()
main = do
  -- (Input os us) <- parseChallengeT (Sample 5 1) pInput
  (Input os us) <- parseChallengeT (Full 5) pInput
  let order = fromList os
  -- 1
  let s = sortBy (customOrdering order)
  print $ sum $ map middle $ filter (\u -> u == s u) us
  -- 2
  print $ sum $ map (middle . s) $ filter (\u -> u /= s u) us
