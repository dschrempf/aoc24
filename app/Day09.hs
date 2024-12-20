module Main
  ( main,
  )
where

import Aoc
import Control.Applicative (Alternative (..))
import Data.Attoparsec.Text (Parser, digit)
import Data.Char (digitToInt)
import Data.Foldable (Foldable (..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import qualified Data.Set as S

pInput :: Parser [Int]
pInput = many $ digitToInt <$> digit

type Pos = Int

type FileId = Int

type Disk = IntMap FileId

data Storage = Storage {_disk :: !Disk, _curFileId :: !FileId, _curPos :: !Pos}
  deriving (Eq, Show)

addFile :: Storage -> Int -> Storage
addFile (Storage d cId cPos) fSize = Storage d' (cId + 1) (cPos + fSize)
  where
    d' = foldl' (\x p -> M.insert p cId x) d [cPos .. cPos + fSize - 1]

addSpace :: Storage -> Int -> Storage
addSpace (Storage d cId cPos) space = Storage d cId (cPos + space)

getStorage :: [Int] -> Storage
getStorage = go emptyStorage
  where
    emptyStorage = Storage M.empty 0 0
    go s (x : y : xs) =
      let s' = addFile s x
          s'' = addSpace s' y
       in go s'' xs
    go s [x] = addFile s x
    go _ [] = error "at the end we should have a file"

-- We assume there is some free space...
getNextFree :: Pos -> Disk -> Pos
getNextFree c d = case d M.!? (c + 1) of
  Nothing -> c + 1
  Just _ -> getNextFree (c + 1) d

getPrevTaken :: Pos -> Disk -> (FileId, Pos)
getPrevTaken c d = case d M.!? (c - 1) of
  Nothing -> getPrevTaken (c - 1) d
  Just fId -> (fId, c - 1)

compress :: Storage -> Disk
compress (Storage d _ p) = go 0 p d
  where
    go free taken disk =
      let free' = getNextFree free disk
          (fId, taken') = getPrevTaken taken disk
       in if taken' <= free'
            then disk
            else go free' taken' $ move fId taken' free' disk
    move fId from to disk = M.insert to fId $ M.delete from disk

checksum :: Disk -> Int
checksum = M.foldlWithKey addBlock 0
  where
    addBlock s pos fId = s + pos * fId

getFirstFreeLargeEnough :: Pos -> Int -> Disk -> Maybe Pos
getFirstFreeLargeEnough maxPos s d = go 0
  where
    go p
      | p >= maxPos = Nothing
      | otherwise =
          let p' = getNextFree p d
           in if p' >= maxPos
                then Nothing
                else
                  let sz = getSizeOfSpace p' d
                   in if sz >= s then Just p' else go (p' + sz)

getSizeOfSpace :: Pos -> Disk -> Int
getSizeOfSpace p d = go 0 p
  where
    go s i = case d M.!? i of
      Nothing -> go (s + 1) (i + 1)
      Just _ -> s

getPrevFile :: Int -> Disk -> Maybe (FileId, Pos, Int)
getPrevFile p d = go p
  where
    go i
      | i <= 0 = Nothing
      | otherwise =
          let (fId, i') = getPrevTaken p d
              sz = getSizeOfFile fId i' d
           in Just (fId, i' - sz + 1, sz)

getSizeOfFile :: FileId -> Pos -> Disk -> Int
getSizeOfFile fId p d = go 0 p
  where
    go s i = case d M.!? i of
      Just fId'
        | fId == fId' -> go (s + 1) (i - 1)
        | otherwise -> s
      Nothing -> s

compress2 :: Storage -> Disk
compress2 (Storage d _ p) = go p d S.empty
  where
    go i disk moved = case getPrevFile i disk of
      Nothing -> disk
      Just (fId, fPos, fSz) -> case getFirstFreeLargeEnough fPos fSz disk of
        Nothing -> go fPos disk moved
        Just free ->
          if fId `S.member` moved
            then go fPos disk moved
            else go fPos (move fId fPos fSz free disk) (S.insert fId moved)
    move fId fPos fSz to disk = store fId to fSz $ delete fPos fSz disk
    delete pos sz disk = foldr' M.delete disk [pos .. pos + sz - 1]
    store fId pos sz disk = foldr' (`M.insert` fId) disk [pos .. pos + sz - 1]

-- showDisk :: Int -> Disk -> String
-- showDisk sz d = go 0
--   where
--     go i
--       | i >= sz = ""
--       | otherwise = case d M.!? i of
--           Nothing -> '.' : go (i + 1)
--           Just fId -> intToDigit fId : go (i + 1)

main :: IO ()
main = do
  d <- parseChallengeT (Full 9) pInput
  let storage = getStorage d
  print $ checksum $ compress storage
  print $ checksum $ compress2 $ getStorage d
