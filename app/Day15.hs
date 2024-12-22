{-# LANGUAGE FlexibleContexts #-}

module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (filterA, pMatrix)
import Aoc.Direction (Direction (..), moveNStepsInDirection)
import Control.Applicative (Alternative (..))
import Control.Monad.ST (runST)
import Data.Attoparsec.Text (Parser, char, choice, endOfLine, sepBy', skipSpace)
import Data.Foldable (Foldable (..))
import Data.Massiv.Array (Array, B (..), Ix2 (..))
import qualified Data.Massiv.Array as A

data Field = Wa | Gr | Bo
  deriving (Eq)

instance Show Field where
  show Wa = "#"
  show Gr = "."
  show Bo = "O"

toField :: Char -> Field
toField '#' = Wa
toField '.' = Gr
toField '@' = Gr
toField 'O' = Bo
toField _ = error "unknown field"

type Pitch = Array B Ix2 Field

type Pos = Ix2

type Move = Direction

pMove :: Parser Direction
pMove = choice [N <$ char '^', E <$ char '>', S <$ char 'v', W <$ char '<']

pMoves :: Parser [Direction]
pMoves = concat <$> (some pMove `sepBy'` endOfLine)

pInput :: Parser (Pitch, Pos, [Move])
pInput = do
  f <- pMatrix id :: Parser (Array B Ix2 Char)
  let posRobot = head $ filterA (== '@') f
      f' = A.computeAs B $ A.map toField f
  _ <- skipSpace
  mvs <- pMoves
  pure (f', posRobot, mvs)

type State = (Pitch, Pos)

move :: State -> Move -> State
move (xs, p) d = case xs A.! p' of
  Gr -> (xs, p')
  Wa -> (xs, p)
  Bo -> case moveBo xs p' d of
    Just xs' -> (xs', p')
    Nothing -> (xs, p)
  where
    p' = moveNStepsInDirection 1 p d

moveBo :: Pitch -> Pos -> Direction -> Maybe Pitch
moveBo xs p d = case xs A.! p' of
  Gr -> Just $ mv xs
  Wa -> Nothing
  Bo -> case moveBo xs p' d of
    Just xs' -> Just $ mv xs'
    Nothing -> Nothing
  where
    p' = moveNStepsInDirection 1 p d
    mv ys = runST $ do
      m <- A.thawS ys
      A.write_ m p Gr
      A.write_ m p' Bo
      A.freezeS m

score :: Pitch -> Int
score = sum . map (\(m :. n) -> 100 * m + n) . filterA (== Bo)

main :: IO ()
main = do
  (xs, pos, mvs) <- parseChallengeT (Full 15) pInput
  let (xs', pos') = foldl' move (xs, pos) mvs
  print xs'
  print pos'
  print $ score xs'
