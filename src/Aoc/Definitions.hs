-- |
-- Module      :  Aoc.Definitions
-- Description :  Definitions
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Nov  9 09:58:58 2023.
module Aoc.Definitions
  ( Year,
    year,
    Day,
    Challenge (..),
    getDay,
    getInputFile,
  )
where

import Numeric.Natural (Natural)
import Text.Printf (printf)

type Year = Natural

year :: Year
year = 2023

type Day = Natural

data Challenge
  = -- | Sample.
    Sample Day Natural
  | -- | Full.
    Full Day
  deriving (Show)

getDay :: Challenge -> Day
getDay (Full day) = day
getDay (Sample day _) = day

getInputFile :: Challenge -> String
getInputFile (Sample day number) = printf "inputs/day%02dsample%02d.txt" day number
getInputFile (Full day) = printf "inputs/day%02dfull.txt" day
