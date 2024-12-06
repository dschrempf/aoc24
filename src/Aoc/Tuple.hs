-- |
-- Module      :  Aoc.Tuple
-- Description :  Tuple helpers
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Jan 28 14:33:53 2024.
module Aoc.Tuple
  ( sortTuple,
  )
where

sortTuple :: (Ord a) => (a, a) -> (a, a)
sortTuple (a, b)
  | a <= b = (a, b)
  | otherwise = (b, a)
