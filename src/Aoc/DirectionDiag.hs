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
  )
where

data Direction = N | NE | E | SE | S | SW | W | NW
  deriving (Show, Eq, Ord, Bounded, Enum)
