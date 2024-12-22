{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Main
  ( main,
  )
where

import Aoc
import Aoc.Parse (skipHorizontalSpace)
import Data.Attoparsec.Text (Parser, decimal, sepBy1', skipSpace, string)
import qualified Data.Text as T

data Configuration = Conf
  { _a :: !(Integer, Integer),
    _b :: !(Integer, Integer),
    _x :: !(Integer, Integer)
  }
  deriving (Eq, Show)

-- >>> parseOnly (pButtonConfiguration 'A') "Button A: X+94, Y+34"
-- Right (94 :. 34)

pButtonConfiguration :: Char -> Parser (Integer, Integer)
pButtonConfiguration c = do
  _ <- string "Button"
  _ <- skipHorizontalSpace
  _ <- string $ T.singleton c <> ":"
  _ <- skipHorizontalSpace
  _ <- string "X+"
  i <- decimal
  _ <- string ", Y+"
  j <- decimal
  pure (i, j)

-- >>> parseOnly pPrize "Prize: X=8400, Y=5400"
-- Right (8400 :. 5400)

pPrize :: Parser (Integer, Integer)
pPrize = do
  _ <- string "Prize: X="
  i <- decimal
  _ <- string ", Y="
  j <- decimal
  pure (i, j)

pConfiguration :: Parser Configuration
pConfiguration = do
  a <- pButtonConfiguration 'A'
  _ <- skipSpace
  b <- pButtonConfiguration 'B'
  _ <- skipSpace
  p <- pPrize
  pure $ Conf a b p

pInput :: Parser [Configuration]
pInput = pConfiguration `sepBy1'` skipSpace

safeDiv :: Integer -> Integer -> Maybe Float
safeDiv a b
  | b == 0 = Nothing
  | otherwise = Just $ fromIntegral a / fromIntegral b

getNPushesA :: Configuration -> Maybe Float
getNPushesA (Conf (xa, ya) (xb, yb) (x, y)) =
  safeDiv (x * yb - xb * y) (xa * yb - xb * ya)

getNPushesB :: Configuration -> Maybe Float
getNPushesB (Conf (xa, ya) (xb, yb) (x, y)) =
  safeDiv (x * ya - xa * y) (xb * ya - xa * yb)

data Solution
  = ParallelNotReachable
  | ParallelNoInteger
  | ParallelNPushesTooHigh
  | NoInteger
  | NPushesTooHigh
  | Win {_nPushesA :: !Integer, _nPushesB :: !Integer}
  deriving (Show)

isInteger :: Float -> Maybe Integer
isInteger x
  | abs (x - fromIntegral i) < 1e-12 = Just i
  | otherwise = Nothing
  where
    i :: Integer
    i = round x

getNTokens :: Configuration -> Solution
getNTokens c = case mNPushes of
  Just (nas, nbs) -> case (isInteger nas, isInteger nbs) of
    (Just nasI, Just nbsI) ->
      if nasI <= 100 && nbsI <= 100
        then Win nasI nbsI
        else NPushesTooHigh
    _ -> NoInteger
  -- Nothing -> getNTokensParallel c
  Nothing -> error "parallel, part 1"
  where
    mNPushes = (,) <$> getNPushesA c <*> getNPushesB c

cost :: Integer -> Integer -> Integer
cost nPushesA nPushesB = 3 * nPushesA + nPushesB

-- getNTokensParallel :: Configuration -> Solution
-- getNTokensParallel (Configuration (xa :. ya) (xb :. yb) (x :. y))
--   | not prizeIsOnLine = ParallelNotReachable
--   | nas' > 100 && nbs' > 100 = ParallelNPushesTooHigh
--   | otherwise = case sortOn
--       (uncurry cost)
--       [ (a, b)
--         | a <- [0 .. nas],
--           b <- [0 .. nbs],
--           a * xa + b * xb == x && a * ya + b * yb == y
--       ] of
--       ((a, b) : _) -> Win a b
--       _ -> ParallelNoInteger
--   where
--     x' = fromIntegral x :: Double
--     y' = fromIntegral y :: Double
--     xa' = fromIntegral xa :: Double
--     xb' = fromIntegral xb :: Double
--     ya' = fromIntegral ya :: Double
--     prizeIsOnLine = (x' / xa') == (y' / ya')
--     nas' = x' / xa'
--     nas = ceiling nas' :: Integer
--     nbs' = x' / xb'
--     nbs = ceiling nbs' :: Integer

eval :: Solution -> Integer
eval (Win a b) = cost a b
eval _ = 0

getNTokens2 :: Configuration -> Solution
getNTokens2 c = case mNPushes of
  Just (nas, nbs) -> case (isInteger nas, isInteger nbs) of
    (Just nasI, Just nbsI) -> Win nasI nbsI
    _ -> NoInteger
  -- Nothing -> getNTokensParallel2 c
  Nothing -> error "parallel, part 2"
  where
    mNPushes = (,) <$> getNPushesA c <*> getNPushesB c

-- getNTokensParallel2 :: Configuration -> Solution
-- getNTokensParallel2 (Configuration (xa :. ya) (xb :. yb) (x :. y))
--   | not prizeIsOnLine = ParallelNotReachable
--   | otherwise = case sortOn
--       (uncurry cost)
--       [ (a, b)
--         | a <- [0 .. nas],
--           b <- [0 .. nbs],
--           a * xa + b * xb == x && a * ya + b * yb == y
--       ] of
--       ((a, b) : _) -> Win a b
--       _ -> ParallelNoInteger
--   where
--     x' = fromIntegral x :: Double
--     y' = fromIntegral y :: Double
--     xa' = fromIntegral xa :: Double
--     xb' = fromIntegral xb :: Double
--     ya' = fromIntegral ya :: Double
--     prizeIsOnLine = (x' / xa') == (y' / ya')
--     nas' = x' / xa'
--     nas = ceiling nas' :: Int
--     nbs' = x' / xb'
--     nbs = ceiling nbs' :: Int

main :: IO ()
main = do
  d <- parseChallengeT (Sample 13 1) pInput
  -- d <- parseChallengeT (Full 13) pInput
  -- 1
  let ss = map getNTokens d
  print ss
  print $ sum $ map eval ss
  -- 2
  let d2 = map (\(Conf a b (x, y)) -> Conf a b (x + 10000000000000, y + 10000000000000)) d
  let ss2 = map getNTokens2 d2
  print ss2
  print $ sum $ map eval ss2
