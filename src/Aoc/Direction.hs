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
    ix2ToDirections,
    directionToIx2,
    moveNStepsInDirection,
    isVertical,
    isHorizontal,
    turnRight,
    turnLeft,
  )
where

import Aoc.Bounded (predWrap, succWrap)
import Data.Massiv.Array (Ix2 (..))
import Data.Maybe (catMaybes)

data Direction = North | East | South | West
  deriving (Show, Eq, Ord, Bounded, Enum)

signToDirectionNS :: Int -> Maybe Direction
signToDirectionNS x = case compare x 0 of
  GT -> Just South
  LT -> Just North
  EQ -> Nothing

signToDirectionEW :: Int -> Maybe Direction
signToDirectionEW x = case compare x 0 of
  LT -> Just West
  GT -> Just East
  EQ -> Nothing

-- | Extract direction components.
ix2ToDirections :: Ix2 -> [Direction]
ix2ToDirections (x :. y) = catMaybes [signToDirectionNS x, signToDirectionEW y]

-- | Get shortest delta index from direction.
directionToIx2 :: Direction -> Ix2
directionToIx2 North = -1 :. 0
directionToIx2 East = 0 :. 1
directionToIx2 South = 1 :. 0
directionToIx2 West = 0 :. -1

moveNStepsInDirection :: Int -> Ix2 -> Direction -> Ix2
moveNStepsInDirection nSteps pos dir = pos + (nSteps * m :. nSteps * n)
  where
    (m :. n) = directionToIx2 dir

isVertical :: Direction -> Bool
isVertical North = True
isVertical South = True
isVertical _ = False

isHorizontal :: Direction -> Bool
isHorizontal = not . isVertical

turnRight :: Direction -> Direction
turnRight = succWrap

turnLeft :: Direction -> Direction
turnLeft = predWrap
