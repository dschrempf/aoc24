-- |
-- Module      :  Aoc.Mod
-- Description :  Modular arithmetic
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Nov 12 12:24:41 2023.
--
-- Source: https://byorgey.wordpress.com/.
module Aoc.Mod
  ( eGcd,
    modExp,
    modInverse,
  )
where

-- | Extended greatest common divisor.
--
-- > egcd a b = (g,x,y)
--
-- where @g@ is the greatest common divisor of @a@ and @b@, and @ax + by = g@.
eGcd :: Integer -> Integer -> (Integer, Integer, Integer)
eGcd a 0 = (abs a, signum a, 0)
eGcd a b = (g, y, x - (a `div` b) * y)
  where
    (g, x, y) = eGcd b (a `mod` b)

-- | @modExp b e m@ computes @(b**e) mod m@.
modExp :: Integer -> Integer -> Integer -> Integer
modExp _ 0 _ = 1
modExp b e m
  | even e = (r * r) `mod` m
  | otherwise = (b * r * r) `mod` m
  where
    r = modExp b (e `div` 2) m

-- | @inverse m a@ is the modular inverse of @a mod m@.
--
-- That is, if @b = inverse m a@, then @(b*a) mod m = 1@. The last identity is
-- often written as @b*a = 1 (mod m)@, which is a bit confusing.
modInverse :: Integer -> Integer -> Integer
modInverse m a = y `mod` m
  where
    (_, _, y) = eGcd m a
