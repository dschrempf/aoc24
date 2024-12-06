-- |
-- Module      :  Main
-- Description :  Helper functions for parsing the input
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Wed Nov  8 12:21:12 2023.
module Aoc.Parse
  ( parseChallengeT,
    parseChallengeB,
    skipHorizontalSpace,
  )
where

import Aoc.Definitions (Challenge, getInputFile)
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Char8 as B
import Data.Functor ((<&>))
import qualified Data.Text.IO as T

parseChallengeWith ::
  (String -> IO a) ->
  (p -> a -> Either String b) ->
  Challenge ->
  p ->
  IO b
parseChallengeWith readFileF parseOnlyF challenge parser =
  readFileF (getInputFile challenge)
    <&> either error id . parseOnlyF parser

parseChallengeT :: Challenge -> AT.Parser a -> IO a
parseChallengeT = parseChallengeWith T.readFile AT.parseOnly

parseChallengeB :: Challenge -> AB.Parser a -> IO a
parseChallengeB = parseChallengeWith B.readFile AB.parseOnly

skipHorizontalSpace :: AT.Parser ()
skipHorizontalSpace = AT.skipWhile AT.isHorizontalSpace
