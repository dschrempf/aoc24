-- |
-- Module      :  Aoc.Occurrence
-- Description :  Count occurrences of things
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Dec 12 09:37:29 2022.
module Aoc.Occurrence
  ( OccurrenceMap,
    countOccurrences,
    addNElems,
  )
where

import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as M

type OccurrenceMap a = M.Map a Int

addNElems :: (Ord a) => Int -> OccurrenceMap a -> a -> OccurrenceMap a
addNElems n m k = M.insertWith (const (+ n)) k n m

-- | Count the number of occurrences of things in a container.
countOccurrences :: (Foldable f, Ord a) => f a -> OccurrenceMap a
countOccurrences = foldl' (addNElems 1) M.empty
