module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (pMatrix)
import Aoc.Direction (Direction (..), moveNStepsInDirection, turnRight)
import Data.Attoparsec.Text (Parser)
import Data.Massiv.Array (Array, B (..), Ix2, computeAs, findIndex, map, (!?))
import Data.Maybe (fromJust)
import Data.Set (Set, empty, insert)
import qualified Data.Set as S
import Prelude hiding (map)

data Field = G | O
  deriving (Show)

fromChar :: Char -> Field
fromChar '.' = G
fromChar '#' = O
fromChar x = error $ "invalid char: " <> [x]

type Pitch = Array B Ix2 Field

extractPlayer :: Array B Ix2 Char -> (Ix2, Pitch)
extractPlayer xs = (p, computeAs B $ map toField xs)
  where
    p = fromJust $ findIndex (== '^') xs
    toField c = if c == '^' then G else fromChar c

pInput :: Parser (Ix2, Pitch)
pInput = extractPlayer <$> pMatrix id

data State = State
  { _pos :: !Ix2,
    _dir :: !Direction,
    visited :: !(Set Ix2)
  }

move :: Pitch -> State -> Maybe State
move xs (State p d vs) = case xs !? p' of
  Nothing -> Nothing
  Just G -> Just $ State p' d $ insert p' vs
  Just O -> Just $ State p (turnRight d) vs
  where
    p' = moveNStepsInDirection 1 p d

loopUntil :: (a -> Maybe a) -> a -> a
loopUntil f x = case f x of
  Nothing -> x
  Just x' -> loopUntil f x'

main :: IO ()
main = do
  (start, pitch) <- parseChallengeT (Full 6) pInput
  let si = State start N empty
      se = loopUntil (move pitch) si
  print $ S.size $ visited se
