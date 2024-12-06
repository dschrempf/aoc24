-- |
-- Module      :  Aoc.Bounded
-- Description :  Bounded and Enum helpers
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Dec 10 16:27:17 2023.
module Aoc.Bounded
  ( succWrap,
    predWrap,
  )
where

succWrap :: (Eq a, Enum a, Bounded a) => a -> a
succWrap x
  | x == maxBound = minBound
  | otherwise = succ x

predWrap :: (Eq a, Enum a, Bounded a) => a -> a
predWrap x
  | x == minBound = maxBound
  | otherwise = pred x
