module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (areNeighborsNoDiagonal, filterA, neighborsNoDiagonalInf, pMatrix)
import Aoc.Direction (Direction (..), moveNStepsInDirection)
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

getNSides :: Set Ix2 -> Int
getNSides xs =
  sum $
    map
      (\(d1, d2) -> S.size $ S.filter (isOuterCorner d1 d2) xs)
      [(N, E), (S, E), (S, W), (N, W)]
      ++ map
        (\(d1, d2) -> S.size $ S.filter (isInnerCorner d1 d2) xs)
        [(N, E), (S, E), (S, W), (N, W)]
  where
    isOuterCorner d1 d2 ix =
      let go = moveNStepsInDirection 1 ix
          isOut jx = jx `S.notMember` xs
       in isOut (go d1) && isOut (go d2)
    isInnerCorner d1 d2 ix =
      let go = moveNStepsInDirection 1 ix
          isIn jx = jx `S.member` xs
          isOut jx = jx `S.notMember` xs
          diag = moveNStepsInDirection 1 (moveNStepsInDirection 1 ix d2) d1
       in isIn (go d1) && isIn (go d2) && isOut diag

main :: IO ()
main = do
  d <- parseChallengeT (Full 12) pInput
  -- 1
  let chars = S.toList $ getChars d
      areas = concatMap (getAreas d) chars
      as = map getArea areas
      ps = map getPerimeter areas
  print $ sum $ zipWith (*) as ps
  -- 2
  let ss = map getNSides areas
  print $ sum $ zipWith (*) as ss
