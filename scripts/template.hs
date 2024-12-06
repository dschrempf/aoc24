module Main
  ( main,
  )
where

import Aoc

main :: IO ()
main = do
  d <- parseChallengeT (Sample __DAY__) pInput
  print d
