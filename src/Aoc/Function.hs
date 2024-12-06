-- |
-- Module      :  Aoc.Function
-- Description :  Tools for functions
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 08:27:51 2022.
module Aoc.Function
  ( nTimesStrict,
    nTimesLazy,
    nTimesStrictM,
    nTimesLazyM,
    fixPoint,
  )
where

import Control.DeepSeq

-- | Apply a function @n@ times.
nTimesStrict :: (NFData a) => Int -> (a -> a) -> a -> a
nTimesStrict n f x = case compare n 1 of
  LT -> error $ "nTimesStrict: n zero or negative: " ++ show n
  EQ -> force $ f x
  GT -> nTimesStrict (n - 1) f $ force $ f x

-- | Apply a function @n@ times.
nTimesLazy :: Int -> (a -> a) -> a -> a
nTimesLazy n f x = case compare n 1 of
  LT -> error $ "nTimesLazy: n zero or negative: " ++ show n
  EQ -> f x
  GT -> nTimesLazy (n - 1) f $ f x

-- | Apply a monadic function @n@ times.
nTimesStrictM :: (NFData a, Monad m) => Int -> (a -> m a) -> a -> m a
nTimesStrictM n f x = case compare n 1 of
  LT -> error $ "nTimesStrict: n zero or negative: " ++ show n
  EQ -> force <$> f x
  GT -> do
    f' <- force <$> f x
    nTimesStrictM (n - 1) f f'

-- | Apply a function @n@ times.
nTimesLazyM :: (Monad m) => Int -> (a -> m a) -> a -> m a
nTimesLazyM n f x = case compare n 1 of
  LT -> error $ "nTimesLazy: n zero or negative: " ++ show n
  EQ -> f x
  GT -> f x >>= nTimesLazyM (n - 1) f

-- | Apply a function until reaching a fix point.
fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint f x
  | x' == x = x'
  | otherwise = fixPoint f x'
  where
    x' = f x
