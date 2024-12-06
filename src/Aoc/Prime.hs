-- |
-- Module      :  Aoc.Prime
-- Description :  Compute prime numbers
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sat Sep 23 19:09:40 2023.
module Aoc.Prime (isPrime, primes) where

import Control.Monad.Fix

isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = go 2
  where
    go m
      | m * m > n = True
      | n `mod` m == 0 = False
      | otherwise = go (succ m)

-- | Lazily compute all primes.
primes :: [Integer]
primes = 2 : g (fix g)
  where
    g xs = 3 : gaps 5 (unionAll [[p * p, p * p + 2 * p ..] | p <- xs])

unionAll :: (Ord a) => [[a]] -> [a]
unionAll ((x : xs) : t) = x : union xs (unionAll $ pairs t)
  where
    pairs ((y : ys) : zs : s) = (y : union ys zs) : pairs s
    pairs _ = error "pairs: bug; fell through pattern"
unionAll _ = error "unionAll: bug; fell through pattern"

union :: (Ord a) => [a] -> [a] -> [a]
union (x : xs) (y : ys) = case compare x y of
  LT -> x : union xs (y : ys)
  EQ -> x : union xs ys
  GT -> y : union (x : xs) ys
union _ _ = error "union: bug; fell through pattern"

gaps :: (Ord t, Num t) => t -> [t] -> [t]
gaps k s@(x : xs)
  | k < x = k : gaps (k + 2) s
  | otherwise = gaps (k + 2) xs
gaps _ _ = error "singleton or empty list"
