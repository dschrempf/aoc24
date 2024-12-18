module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (filterA, pMatrix)
import Aoc.Direction (Direction (..), moveNStepsInDirection, turnRight)
import Data.Attoparsec.Text (Parser)
import Data.Massiv.Array (Array, B (..), Ix2, computeAs, findIndex, imap, map, (!?))
import Data.Maybe (fromJust)
import Data.Set (Set, empty, insert, member)
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

data State2 = State2
  { _pos2 :: !Ix2,
    _dir2 :: !Direction,
    _obstace :: !Ix2,
    _visited2 :: !(Set (Ix2, Direction))
  }

data Loop = Loop | NoLoop
  deriving (Eq, Show)

move2 :: Pitch -> State2 -> Either Loop State2
move2 xs (State2 p d o vs) = case (xs !? p', p' == o) of
  (Nothing, _) -> Left NoLoop
  (Just G, False) ->
    if (p', d) `member` vs
      then Left Loop
      else Right $ State2 p' d o $ insert (p', d) vs
  _ -> Right $ State2 p (turnRight d) o vs
  where
    p' = moveNStepsInDirection 1 p d

loopUntilE :: (a -> Either l a) -> a -> l
loopUntilE f x = case f x of
  Left l -> l
  Right x' -> loopUntilE f x'

isLoop :: Ix2 -> Pitch -> Ix2 -> Field -> Loop
isLoop _ _ _ O = NoLoop
isLoop start xs ix G
  | start == ix = NoLoop
  | otherwise = loopUntilE (move2 xs) s0
  where
    s0 = State2 start N ix empty

main :: IO ()
main = do
  (start, pitch) <- parseChallengeT (Full 6) pInput
  -- 1
  let si = State start N empty
      se = loopUntil (move pitch) si
  print $ S.size $ visited se
  -- 2
  let ls = imap (isLoop start pitch) pitch
  print $ length $ filterA (== Loop) ls
