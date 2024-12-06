module Main
  ( main,
  )
where

import Aoc
import Aoc.Occurrence (countOccurrences)
import Aoc.Parse (skipHorizontalSpace)
import Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy1')
import Data.Foldable (Foldable (..))
import Data.List (sort)
import Data.Map ((!?))
import Data.Maybe (fromMaybe)

pLine :: Parser (Int, Int)
pLine = do
  l <- decimal
  _ <- skipHorizontalSpace
  r <- decimal
  pure (l, r)

pInput :: Parser ([Int], [Int])
pInput = unzip <$> pLine `sepBy1'` endOfLine

main :: IO ()
main = do
  (ls, rs) <- parseChallengeT (Full 1) pInput
  -- Part 1.
  print $ sum $ zipWith (\x y -> abs (x - y)) (sort ls) (sort rs)
  -- Part 2.
  let os = countOccurrences rs
      addO acc x = acc + x * fromMaybe 0 (os !? x)
  print $ foldl' addO 0 ls
