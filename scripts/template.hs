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
  d <- parseChallengeT (Sample __DAY__ 1) pInput
  print d
