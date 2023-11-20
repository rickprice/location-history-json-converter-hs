{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Unsafe #-}

module Main (main) where

import Codec.Archive.Tar as Tar

import Codec.Compression.GZip as GZip
import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Time.Clock
import Model
import System.Exit
import Prelude

-- import Data.Time
import Control.Monad (mfilter)

{- | This is like the standard 'foldr' function on lists, but for 'Entries'.
 Compared to 'foldEntries' it skips failures.
-}
foldEntriesIgnoreFailure :: (Tar.Entry -> a -> a) -> a -> Tar.Entries e -> a
foldEntriesIgnoreFailure next done = fold
  where
    fold (Tar.Next e es) = next e (fold es)
    fold Tar.Done = done
    fold (Tar.Fail _) = done

-- Convert an entry to its filepath
-- entryToPath :: Tar.Entry -> String
-- entryToPath entry = show $ Tar.entryPath entry

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

-- location = Locations 1 (Text.pack "test") 2 3
-- model = Model [location]

main :: IO ()
main = do
    fileContent <- GZip.decompress <$> BS.readFile "takeout.tgz"
    let entries = Tar.read fileContent
    let entryList = foldEntriesIgnoreFailure (:) [] entries
    let locationRecordFiles = map entryToByteString (filter entryIsLocationData entryList)
    let locationRecordFile = pure (head locationRecordFiles)
    -- print "starting"
    -- Get JSON data and decode it
    d <- (eitherDecode <$> locationRecordFile) :: IO (Either String Model)
    -- If d is Left, the JSON was malformed.
    -- In that case, we report the error.
    -- Otherwise, we perform the operation of
    -- our choice. In this case, just print it.
    case d of
        Left err -> die err
        Right ps -> do
            now <- getCurrentTime
            let twoWeekAgo = addUTCTime (-nominalDay * 7 * 2) now
            let locationList = locations ps
            let locationListFiltered = filter isComplete locationList
            let locationListFilteredDate = mfilter (\x -> timestamp x > Just twoWeekAgo) locationListFiltered
            -- let lengthOriginal = length locationList
            -- let lengthFiltered = length locationListFiltered
            -- let lengthFilteredDate = length locationListFilteredDate
            -- print locationListFiltered
            -- print locationListFilteredDate
            -- print lengthOriginal
            -- print lengthFiltered
            -- print lengthFilteredDate
            putStrLn $ toXMLString locationListFilteredDate

-- print "finished"
