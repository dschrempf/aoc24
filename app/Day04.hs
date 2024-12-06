module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (pMatrix)
import Data.Attoparsec.Text (Parser)
import Data.Massiv.Array (Array, Ix2 (..), U, (!))
import Numeric.Natural (Natural)

type Field = Array U Ix2 Char

pInput :: Parser Field
pInput = pMatrix id

nXmas :: Field -> Ix2 -> Natural
nXmas xs ix
  | el == 'X' = undefined
  | otherwise = 0
  where
    el = xs ! ix

main :: IO ()
main = do
  d <- parseChallengeT (Sample 4 2) pInput
  print d
