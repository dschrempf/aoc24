-- |
-- Module      :  Aoc.Direction
-- Description :  Directions on a 2D grid
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Dec 10 20:37:01 2023.
module Aoc.Direction
  ( Direction (..),
    ixToDirections,
    directionToIx,
    moveNStepsInDirection,
    isVertical,
    isHorizontal,
    turnRight,
    turnLeft,
  )
where

import Aoc.Bounded (predWrap, succWrap)
import Data.Massiv.Array (Index (pureIndex), Ix2 (..))
import Data.Maybe (catMaybes)

data Direction = N | E | S | W
  deriving (Show, Eq, Ord, Bounded, Enum)

_signToDirectionNS :: Int -> Maybe Direction
_signToDirectionNS x = case compare x 0 of
  GT -> Just S
  LT -> Just N
  EQ -> Nothing

_signToDirectionEW :: Int -> Maybe Direction
_signToDirectionEW x = case compare x 0 of
  LT -> Just W
  GT -> Just E
  EQ -> Nothing

-- | Extract direction components.
ixToDirections :: Ix2 -> [Direction]
ixToDirections (x :. y) = catMaybes [_signToDirectionNS x, _signToDirectionEW y]

-- | Get shortest delta index from direction.
directionToIx :: Direction -> Ix2
directionToIx N = -1 :. 0
directionToIx E = 0 :. 1
directionToIx S = 1 :. 0
directionToIx W = 0 :. -1

moveNStepsInDirection :: Int -> Ix2 -> Direction -> Ix2
moveNStepsInDirection n pos dir = pos + pureIndex n * directionToIx dir

isVertical :: Direction -> Bool
isVertical N = True
isVertical S = True
isVertical _ = False

isHorizontal :: Direction -> Bool
isHorizontal = not . isVertical

turnRight :: Direction -> Direction
turnRight = succWrap

turnLeft :: Direction -> Direction
turnLeft = predWrap
