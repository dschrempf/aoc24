module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (filterA, pMatrix)
import Aoc.DirectionDiag (Direction, moveNStepsInDirection)
import Aoc.Enum (enumerate)
import Data.Attoparsec.Text (Parser)
import Data.Massiv.Array (Array, Ix2 (..), Stencil, Sz (..), U (..), applyStencil, computeAs, imap, makeStencil, noPadding, sum, (!?))
import Prelude hiding (sum)

type Field = Array U Ix2 Char

pInput :: Parser Field
pInput = pMatrix id

isXmas :: Field -> Ix2 -> Direction -> Bool
isXmas xs ix d = all (uncurry isL) $ zip [0 .. 4] "XMAS"
  where
    mv n = moveNStepsInDirection n ix d
    isL n l = (xs !? mv n) == Just l

nXmas :: Field -> Ix2 -> Char -> Int
nXmas xs ix el
  | el == 'X' = length $ filter id $ map (isXmas xs ix) enumerate
  | otherwise = 0

masStencil :: Stencil Ix2 Char Bool
masStencil = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \g ->
  (g (0 :. 0) == 'A')
    && ((g (-1 :. -1) == 'M' && g (1 :. 1) == 'S') || (g (-1 :. -1) == 'S' && g (1 :. 1) == 'M'))
    && ((g (-1 :. 1) == 'M' && g (1 :. -1) == 'S') || (g (-1 :. 1) == 'S' && g (1 :. -1) == 'M'))

main :: IO ()
main = do
  d <- parseChallengeT (Full 4) pInput
  -- d <- parseChallengeT (Sample 4 2) pInput
  -- 1
  print $ sum $ computeAs U $ imap (nXmas d) d
  -- 2
  print $ length $ filterA id $ computeAs U $ applyStencil noPadding masStencil d
