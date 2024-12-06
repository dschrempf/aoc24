{-# LANGUAGE DataKinds #-}

-- |
-- Module      :  Main
-- Description :  Scratch
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Wed Nov  8 16:55:36 2023.
module Main
  ( main,
  )
where

import Data.Massiv.Array as A

a1 :: Vector D Ix1
a1 = makeVectorR D Seq 10 id

a2 :: Array D Ix1 Int
a2 = A.map (^ (2 :: Int)) a1

a3 :: Array U Ix2 Double
a3 = fromLists' Par [[0, 0, 0, 0, 0], [0, 1, 2, 3, 4], [0, 2, 4, 6, 8]]

a4 :: Array D (IxN 4) Int
a4 =
  makeArrayR
    D
    Par
    (Sz (10 :> 20 :> 30 :. 40))
    (\(i :> j :> k :. l) -> (i * j + k) * k + l)

a5 :: Array U Ix3 Ix3T
a5 = makeArrayR U Seq (Sz (4 :> 2 :. 6)) fromIx3

a6 :: Array U Ix2 Int
a6 = makeArray Seq (Sz $ 4 :. 6) $ \(i :. j) -> i + j

sumNeighbors :: (Num a) => Stencil Ix2 a a
sumNeighbors = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \get ->
  get (-1 :. -1)
    + get (-1 :. 0)
    + get (-1 :. 1)
    + get (0 :. -1)
    + get (0 :. 0)
    + get (0 :. 1)
    + get (1 :. -1)
    + get (1 :. 0)
    + get (1 :. 1)
{-# INLINE sumNeighbors #-}

main :: IO ()
main = do
  print $ computeAs U a2
  print $ A.sum a3
  print $ A.sum a4
  let ix10 = 10 :> 9 :> 8 :> 7 :> 6 :> 5 :> 4 :> 3 :> 2 :. 1
  print $ pullOutDim' ix10 5
  print $ a5 !> 3 !> 1
  print a6
  print (mapStencil (Fill 0) sumNeighbors a6 :: Array DW Ix2 Int)
