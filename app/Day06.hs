module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (pMatrix)
import Data.Attoparsec.Text (Parser)
import Data.Massiv.Array (Array, B (..), Ix2, computeAs, findIndex, map)
import Data.Maybe (fromJust)
import Prelude hiding (map)

data Field = G | O
  deriving (Show)

fromChar :: Char -> Field
fromChar '.' = G
fromChar '#' = O
fromChar x = error $ "invalid char: " <> [x]

type Pitch = Array B Ix2 Field

extractPlayer :: Array B Ix2 Char -> (Ix2, Pitch)
extractPlayer xs = (p, computeAs B $ map toField xs)
  where
    p = fromJust $ findIndex (== '^') xs
    toField c = if c == '^' then G else fromChar c

pInput :: Parser (Ix2, Pitch)
pInput = extractPlayer <$> pMatrix id

main :: IO ()
main = do
  d <- parseChallengeT (Sample 6 1) pInput
  print d
