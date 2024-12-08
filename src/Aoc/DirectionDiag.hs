-- |
-- Module      :  Aoc.DirectionDiag
-- Description :  Directions with diagonals on a 2D grid
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Dec 10 20:37:01 2023.
module Aoc.DirectionDiag
  ( Direction (..),
    moveNStepsInDirection,
  )
where

import Data.Massiv.Array (Index (..), Ix2 (..))

-- import Data.Massiv.Array (Array)

data Direction = N | NE | E | SE | S | SW | W | NW
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Get shortest delta index from direction.
directionToIx :: Direction -> Ix2
directionToIx N = -1 :. 0
directionToIx NE = -1 :. 1
directionToIx E = 0 :. 1
directionToIx SE = 1 :. 1
directionToIx S = 1 :. 0
directionToIx SW = 1 :. -1
directionToIx W = 0 :. -1
directionToIx NW = -1 :. -1

-- >>> moveNStepsInDirection 5 (0 :. 0) NE
-- -5 :. 5

moveNStepsInDirection :: Int -> Ix2 -> Direction -> Ix2
moveNStepsInDirection n pos dir = pos + pureIndex n * directionToIx dir
