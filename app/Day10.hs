{-# LANGUAGE DeriveGeneric #-}

module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (filterA, neighborsNoDiagonal, pMatrix)
import Aoc.Function (nTimesStrict)
import Aoc.Set (flatten)
import Control.DeepSeq (NFData)
import Data.Attoparsec.Text (Parser)
import Data.Char (digitToInt)
import Data.Massiv.Array (Array, Ix2, Size (..), U, (!))
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics

type Pitch = Array U Ix2 Int

pInput :: Parser Pitch
pInput = pMatrix (\c -> if c == '.' then (-1) else digitToInt c)

type Elevation = Int

data State = State Ix2 Elevation [Ix2]
  deriving (Eq, Ord, Show, Generic)

instance NFData State

getTrailHeads :: Pitch -> [State]
-- getTrailHeads xs = S.map (,0) $ S.filter (\i -> (xs ! i) == 0) $ border (size xs)
getTrailHeads xs = map (\i -> State i 0 [i]) $ filterA (== 0) xs

step :: Pitch -> State -> Set State
step xs (State ix el hs) =
  S.fromList
    [ State n el' (n : hs)
      | n <- filter (\i -> (xs ! i) == el') ns
    ]
  where
    el' = el + 1
    ns = neighborsNoDiagonal (size xs) ix

next :: Pitch -> Set State -> Set State
next xs = flatten . S.map (step xs)

walk :: Pitch -> State -> Set State
walk xs = nTimesStrict 9 (next xs) . S.singleton

main :: IO ()
main = do
  d <- parseChallengeT (Full 10) pInput
  -- print $ map (walk d) $ getTrailHeads d
  -- print $ sum $ map (S.size . walk d) $ getTrailHeads d
  let theads = getTrailHeads d
  let tends = map (walk d) theads
  print $ sum $ map S.size tends
