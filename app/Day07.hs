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

data Input = Input {goal :: !Int, numbers :: ![Int]}
  deriving (Show)

pInput :: Parser [Input]
pInput = pOne `sepBy1'` endOfLine
  where
    pOne = Input <$> (decimal <* string ": ") <*> decimal `sepBy1'` skipHorizontalSpace

concatN :: Int -> Int -> Int
concatN x y = read $ show x ++ show y

calc :: Input -> [Int]
calc (Input _ []) = []
calc (Input g (x : xs)) = go [x] xs
  where
    go ys [] = ys
    go ys (z : zs) = go (concat [filter (<= g) [y + z, y * z, concatN y z] | y <- ys]) zs

isPossible :: Input -> Bool
isPossible i = goal i `elem` calc i

main :: IO ()
main = do
  d <- parseChallengeT (Full 7) pInput
  -- 2
  print $ sum $ map goal $ filter isPossible d
