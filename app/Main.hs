{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Unsafe #-}

module Main (main) where

import Codec.Archive.Tar qualified as Tar
-- import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy qualified as BS
import Prelude

-- | This is like the standard 'foldr' function on lists, but for 'Entries'.
-- Compared to 'foldEntries' it skips failures.
foldEntriesIgnoreFailure :: (Tar.Entry -> a -> a) -> a -> Tar.Entries e -> a
foldEntriesIgnoreFailure next done = fold
  where
    fold (Tar.Next e es) = next e (fold es)
    fold Tar.Done = done
    fold (Tar.Fail _) = done

-- Convert a entry to its filepath
entryToPath :: Tar.Entry -> String
entryToPath entry = show $ Tar.entryPath entry

-- Is this Entry the location data we are looking to export
entryIsLocationData :: Tar.Entry -> Bool
entryIsLocationData e = case Tar.entryContent e of
  Tar.NormalFile _ _ -> doesPathMatch (Tar.entryPath e)
  _ -> False
  where
    doesPathMatch :: String -> Bool
    doesPathMatch p = "Takeout/Location History/Records.json" == p

main :: IO ()
main = do
  fileContent <- GZip.decompress <$> BS.readFile "takeout.tgz"
  let entries = Tar.read fileContent
  let entryList = foldEntriesIgnoreFailure (:) [] entries
  let filteredEntryList = map entryToPath (filter entryIsLocationData entryList)
  print filteredEntryList

-- print entryPaths
