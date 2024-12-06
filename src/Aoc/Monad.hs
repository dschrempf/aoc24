-- |
-- Module      :  Aoc.Monad
-- Description :  Monadic helpers
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec 29 18:56:56 2023.
module Aoc.Monad
  ( (<.>),
  )
where

infixr 9 <.>

-- | Composition: pure function after functorial (monadic) function.
(<.>) :: (Functor f) => (b -> c) -> (a -> f b) -> a -> f c
(f <.> g) a = f <$> g a
