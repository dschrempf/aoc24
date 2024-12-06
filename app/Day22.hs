module Main
  ( main,
  )
where

import Aoc
import Data.Attoparsec.Text (Parser)

pInput :: Parser Text
pInput = undefined

main :: IO ()
main = do
  d <- parseChallengeT (Sample 22 1) pInput
  print d
