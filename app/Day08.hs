module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (pMatrix)
import Data.Attoparsec.Text (Parser)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Massiv.Array (Array, Index (isSafeIndex), Ix2, Size (..), Sz, U, ifoldlS)
import Data.Set (Set)
import qualified Data.Set as S
import Prelude hiding (map)

type Pitch = Array U Ix2 Char

pInput :: Parser Pitch
pInput = pMatrix id

data Antenna = Antenna {_pos :: Ix2, _sym :: Char}
  deriving (Eq, Ord, Show)

getAntennas :: Pitch -> Set Antenna
getAntennas = ifoldlS addAntenna S.empty
  where
    addAntenna as ix c =
      if c == '.'
        then as
        else Antenna ix c `S.insert` as

type Antennas = Set Ix2

type Antinodes = Set Ix2

getAntinodePair :: Ix2 -> Ix2 -> [Ix2]
getAntinodePair a b = [a - d, b + d]
  where
    d = b - a

getAntinodes :: Pitch -> Antennas -> Antinodes
getAntinodes pitch antennas =
  S.fromList
    $ filter
      (isSafeIndex $ size pitch)
    $ concat
      [ getAntinodePair a b | a <- S.toList antennas, b <- S.toList antennas, a /= b
      ]

getAllAntinodes :: Pitch -> Set Antenna -> Map Char Antinodes
getAllAntinodes pitch antennas = M.map (getAntinodes pitch) antennaMap
  where
    antennaMap = S.foldl' addAntenna M.empty antennas
    addIndex i Nothing = Just $ S.singleton i
    addIndex i (Just s) = Just $ i `S.insert` s
    addAntenna m (Antenna p s) = M.alter (addIndex p) s m

getAntinodePair2 :: Sz Ix2 -> Ix2 -> Ix2 -> [Ix2]
getAntinodePair2 sz a b = befores ++ afters
  where
    d = b - a
    befores = a : go a
      where
        go x = let x' = x - d in if isSafeIndex sz x' then x' : go x' else []
    afters = a : go b
      where
        go x = let x' = x + d in if isSafeIndex sz x' then x' : go x' else []

getAntinodes2 :: Sz Ix2 -> Antennas -> Antinodes
getAntinodes2 sz antennas =
  S.fromList $ concat [getAntinodePair2 sz a b | a <- S.toList antennas, b <- S.toList antennas, a /= b]

getAllAntinodes2 :: Pitch -> Set Antenna -> Map Char Antinodes
getAllAntinodes2 pitch antennas = M.map (getAntinodes2 $ size pitch) antennaMap
  where
    antennaMap = S.foldl' addAntenna M.empty antennas
    addIndex i Nothing = Just $ S.singleton i
    addIndex i (Just s) = Just $ i `S.insert` s
    addAntenna m (Antenna p s) = M.alter (addIndex p) s m

main :: IO ()
main = do
  d <- parseChallengeT (Full 8) pInput
  let antennas = getAntennas d
  -- 1
  let antinodesMap = getAllAntinodes d antennas
      antinodes = M.foldl' S.union S.empty antinodesMap
  print $ length antinodes
  -- 2
  let antinodesMap2 = getAllAntinodes2 d antennas
      antinodes2 = M.foldl' S.union S.empty antinodesMap2
  print $ length antinodes2
