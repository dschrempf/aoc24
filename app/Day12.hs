module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (areNeighborsNoDiagonal, filterA, neighborsNoDiagonalInf, pMatrix)
import Aoc.Function (fixPoint)
import Data.Attoparsec.Text (Parser)
import Data.Foldable (Foldable (..))
import Data.Massiv.Array (Array, Ix2, U, toLists)
import Data.Set (Set)
import qualified Data.Set as S

type Pitch = Array U Ix2 Char

pInput :: Parser Pitch
pInput = pMatrix id

getChars :: Pitch -> Set Char
getChars = S.fromList . concat . toLists

getAreas :: Pitch -> Char -> [Set Ix2]
getAreas xs c = fixPoint connectAreas $ map S.singleton $ filterA (== c) xs

connectAreas :: [Set Ix2] -> [Set Ix2]
connectAreas = go []
  where
    go :: [Set Ix2] -> [Set Ix2] -> [Set Ix2]
    go as [] = as
    go as (x : xs) = go (connectOne as x) xs
    connectOne :: [Set Ix2] -> Set Ix2 -> [Set Ix2]
    connectOne [] x = [x]
    connectOne (a : as) x
      | or [areNeighborsNoDiagonal i j | i <- S.toList a, j <- S.toList x] = S.union a x : as
      | otherwise = a : connectOne as x

getArea :: Set Ix2 -> Int
getArea = S.size

getPerimeter :: Set Ix2 -> Int
getPerimeter xs = foldl' addFence 0 xs
  where
    addFence tot ix =
      let ns = neighborsNoDiagonalInf ix
          nsNotInArea = filter (`S.notMember` xs) ns
       in tot + length nsNotInArea

-- getNSides :: Set Ix2 -> Int
-- getNSides xs = undefined
--   where
--     getBorder = S.filter isBorder
--     isBorder ix = any (`S.notMember` xs) $ neighborsNoDiagonalInf ix

main :: IO ()
main = do
  d <- parseChallengeT (Full 12) pInput
  let chars = S.toList $ getChars d
      areas = concatMap (getAreas d) chars
      as = map getArea areas
      ps = map getPerimeter areas
  print $ sum $ zipWith (*) as ps
