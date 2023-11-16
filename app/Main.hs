{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Unsafe #-}

module Main (main) where

import Codec.Archive.Tar qualified as Tar
-- import Codec.Archive.Tar.Entry qualified as Tar

-- import GHC.RTS.Flags (DoCostCentres (CostCentresJSON))

import Codec.Archive.Tar.Index qualified as TAR
import Codec.Compression.GZip qualified as GZip
import Data.Aeson (fromJSON, ToJSON (toJSON), encode)
import Data.ByteString.Lazy qualified as BS
import Model
import Prelude

import Data.Text (Text,pack)

-- | This is like the standard 'foldr' function on lists, but for 'Entries'.
-- Compared to 'foldEntries' it skips failures.
foldEntriesIgnoreFailure :: (Tar.Entry -> a -> a) -> a -> Tar.Entries e -> a
foldEntriesIgnoreFailure next done = fold
  where
    fold (Tar.Next e es) = next e (fold es)
    fold Tar.Done = done
    fold (Tar.Fail _) = done

-- Convert an entry to its filepath
entryToPath :: Tar.Entry -> String
entryToPath entry = show $ Tar.entryPath entry

-- Convert an entry to its ByteString
entryToByteString :: Tar.Entry -> BS.ByteString
entryToByteString entry = extractFileData (Tar.entryContent entry)
  where
    -- Extract the file data here case
    extractFileData (Tar.NormalFile d _) = d
    extractFileData _ = BS.empty

-- Is this Entry the location data we are looking to export
entryIsLocationData :: Tar.Entry -> Bool
entryIsLocationData e = case Tar.entryContent e of
  Tar.NormalFile _ _ -> doesPathMatch (Tar.entryPath e)
  _ -> False
  where
    doesPathMatch :: String -> Bool
    doesPathMatch p = "Takeout/Location History/Records.json" == p

-- Code to process the incoming JSON
decodeJSON bs = decodeJSON bs :: Model

-- data Locations = Locations
--   { locationsAccuracy :: Int,
--     locationsTimestamp :: Text,
--     locationsLongitudeE7 :: Int,
--     locationsLatitudeE7 :: Int
--   }


location = Locations 1 (pack "test") 2 3
model = Model [location]

main :: IO ()
main = do
  fileContent <- GZip.decompress <$> BS.readFile "takeout.tgz"
  let entries = Tar.read fileContent
  let entryList = foldEntriesIgnoreFailure (:) [] entries
  -- let filteredEntryList = map (id . entryToByteString) (filter entryIsLocationData entryList)
  let filteredEntryList = map (decodeJSON . entryToByteString) (filter entryIsLocationData entryList)
  let singleEntry = take 1 filteredEntryList
  -- print (singleEntry)
  -- print (model)
  print (encode (toJSON model))
  print ("finished")
