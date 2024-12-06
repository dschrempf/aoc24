{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aoc.Definitions (Challenge (..), getDay, getInputFile, year)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.List.Extra ((!?))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Conduit (Request)
import Network.HTTP.Simple (addRequestHeader, getResponseBody, httpBS, parseRequest)
import Numeric.Natural (Natural)
import System.Directory (doesFileExist, getFileSize)
import System.Environment (getArgs, lookupEnv)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor
  ( Cursor,
    content,
    element,
    fromDocument,
    ($/),
    (&/),
    (&//),
  )

baseUrl :: String
baseUrl = "https://adventofcode.com/" <> show year <> "/day/"

getRoute :: Challenge -> String
getRoute (Full day) = baseUrl <> show day <> "/input"
getRoute challenge = baseUrl <> show (getDay challenge)

getRequest :: (MonadThrow m) => String -> Challenge -> m Request
getRequest token challenge = do
  req <- parseRequest (getRoute challenge)
  return $ addRequestHeader "cookie" (pack token) req

extractSamples :: B.ByteString -> [B.ByteString]
extractSamples = map T.encodeUtf8 . findSamples . fromDocument . parseLBS . B.fromStrict

findSamples :: Cursor -> [T.Text]
findSamples cursor =
  cursor
    $/ element "body"
    &/ element "main"
    &/ element "article"
    &// element "pre"
    &/ element "code"
    &/ content

writeInput :: Challenge -> B.ByteString -> IO ()
writeInput challenge = B.writeFile (getInputFile challenge)

handleResponse :: Challenge -> B.ByteString -> IO ()
handleResponse challenge@(Sample _ n) response =
  case extractSamples response !? fromIntegral (pred n) of
    Just sample -> writeInput challenge sample
    Nothing -> error $ "handleResponse: did not find sample for challenge " <> show challenge
handleResponse challenge@(Full _) response = writeInput challenge response

fetchInput :: Challenge -> IO ()
fetchInput challenge = do
  isCached <- checkFileExistsWithData file
  token' <- lookupEnv "AOC_TOKEN"
  case (isCached, token') of
    (True, _) -> putStrLn "Input has been downloaded already."
    (False, Nothing) -> putStrLn "No session token."
    (False, Just token) -> do
      req <- getRequest token challenge
      response <- getResponseBody <$> httpBS req
      handleResponse challenge response
  where
    file = getInputFile challenge

checkFileExistsWithData :: FilePath -> IO Bool
checkFileExistsWithData file = do
  exists <- doesFileExist file
  if not exists
    then return False
    else do
      size <- getFileSize file
      return $ size > 0

usage :: String
usage =
  unlines
    [ "Usage: COMMAND DAY [N]",
      "",
      "Download input for DAY.",
      "If N is provided, download sample number N of DAY."
    ]

getChallengeForInput :: [Natural] -> Challenge
getChallengeForInput [day] = Full day
getChallengeForInput [day, number] = Sample day number
getChallengeForInput _ = error usage

main :: IO ()
main = do
  loadFile defaultConfig
  ns <- map (read :: String -> Natural) <$> getArgs
  fetchInput $ getChallengeForInput ns
