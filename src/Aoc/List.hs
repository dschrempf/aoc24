{-# LANGUAGE TupleSections #-}

-- |
-- Module      :  Aoc.List
-- Description :  Tools for lists
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sat Dec 10 15:15:27 2022.
module Aoc.List
  ( chop,
    pairs,
    findCycle,
    findFirstDuplicate,
  )
where

import qualified Data.Set as S

-- | Chop up a list into chunks of a given length. O(n).
--
-- Copied from Agda.Utils.List.
chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = ys : chop n zs
  where
    (ys, zs) = splitAt n xs

-- | Get all unordered pairs.
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

-- | Search for a cycle in a list; the cycle length is unknown, but assume the
-- cycle starts at the beginning of the list.
findCycle ::
  (Eq a) =>
  -- | Maximum length.
  Int ->
  -- | Number of repetitions checked (>=2).
  Int ->
  [a] ->
  Maybe (Int, [a])
findCycle maxLength nRepetitions
  | nRepetitions < 2 = error "findCycle: nRepetition is one or lower"
  | otherwise = go 1
  where
    go :: (Eq a) => Int -> [a] -> Maybe (Int, [a])
    go n xs
      | n <= maxLength =
          let h = take n xs
              xss = chop n $ take (nRepetitions * n) xs
           in if all (== h) xss then Just (n, h) else go (n + 1) xs
      | otherwise = Nothing

findFirstDuplicate :: (Ord a) => [a] -> Maybe Int
findFirstDuplicate = go S.empty 0
  where
    go _ _ [] = Nothing
    go seen i (x : xs)
      | x `S.member` seen = Just i
      | otherwise = go (S.insert x seen) (succ i) xs
