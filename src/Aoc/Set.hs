-- |
-- Module      :  Aoc.Set
-- Description :  Set helpers
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Dec 10 13:11:07 2023.
module Aoc.Set
  ( flatten,
  )
where

import Data.Foldable (Foldable (..))
import Data.Set (Set)
import qualified Data.Set as S

flatten :: (Ord a) => Set (Set a) -> Set a
flatten = foldl' S.union S.empty
