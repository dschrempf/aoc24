-- |
-- Module      :  Aoc.Enum
-- Description :  Enum helpers
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Wed Dec 18 20:33:34 2024.
module Aoc.Enum
  ( enumerate,
  )
where

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]
