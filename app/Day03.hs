{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Aoc
import Control.Applicative (Alternative (..))
import Data.Attoparsec.Text (Parser, char, choice, count, digit, string, take, takeWhile)
import Data.Char (ord)
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Prelude hiding (take, takeWhile)

pDigit' :: Parser Int
pDigit' = toInt <$> digit
  where
    toInt c = fromIntegral (ord c - 48)

-- >>> toN [1,4,2]
-- 52

toN :: [Int] -> Int
toN = go 0
  where
    go n [] = n
    go n (x : xs) = go (n * 10 + x) xs

pSomeDigits :: Parser Int
pSomeDigits = choice [toN <$> count 3 pDigit', toN <$> count 2 pDigit', pDigit']

pMul :: Parser (Int, Int)
pMul = do
  _ <- string "mul("
  l <- pSomeDigits
  _ <- char ','
  r <- pSomeDigits
  _ <- char ')'
  pure $ (l, r)

pNextMul :: Parser (Int, Int)
pNextMul = pMul <|> (take 1 *> pNextMul)

pInput :: Parser [(Int, Int)]
pInput = many pNextMul

pDo :: Parser Text
pDo = string "do()"

pDont :: Parser Text
pDont = string "don't()"

pNextDo :: Parser Text
pNextDo = pDo <|> (take 1 *> pNextDo)

pNextMulDo :: Parser (Maybe (Int, Int))
pNextMulDo =
  (Just <$> pMul)
    <|> (pDont *> pNextMulDont)
    <|> (take 1 *> pNextMulDo)

pNextMulDont :: Parser (Maybe (Int, Int))
pNextMulDont = (pNextDo *> pNextMulDo) <|> takeWhile (const True) $> Nothing

pInput2 :: Parser [(Int, Int)]
pInput2 = catMaybes <$> many pNextMulDo

main :: IO ()
main = do
  d <- parseChallengeT (Full 3) pInput
  print $ sum $ map (uncurry (*)) d
  d2 <- parseChallengeT (Full 3) pInput2
  print d2
  print $ sum $ map (uncurry (*)) d2
