-- |
-- Module      :  Aoc.Factor
-- Description :  Compute factors of numbers
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sat Sep 23 21:03:51 2023.
module Aoc.Factor
  ( factors,
  )
where

-- | Unsorted list of factors.
factors :: Int -> [Int]
factors 1 = [1]
factors x = go 1 x
  where
    go n y
      | n * n > y = []
      | r == 0 = n : d : go (succ n) y
      | otherwise = go (succ n) y
      where
        (d, r) = y `quotRem` n
