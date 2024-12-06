-- |
-- Module      :  Aoc.Char
-- Description :  Functions on characters
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Sep 24 22:11:59 2023.
module Aoc.Char
  ( readDigit,
    getAlphaIndexBase1,
  )
where

import Data.Char
import Data.List

readDigit :: Char -> Int
readDigit = read . singleton

getAlphaIndexBase1 :: Char -> Int
getAlphaIndexBase1 c = succ $ ci - ai
  where
    ai = ord 'a'
    ci = ord $ toLower c
