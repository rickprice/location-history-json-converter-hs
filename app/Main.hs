{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Unsafe #-}

module Main (main) where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy qualified as BS
import Prelude

filterEntry :: (Tar.Entry -> Bool) -> [Tar.Entry] -> [Tar.Entry]
filterEntry pred list = list

foldEntryToPath :: Tar.Entry -> [String] -> [String]
foldEntryToPath entry list = list ++ [show $ Tar.entryPath entry]

-- Converts TAR errors to a string.
entryFailMapper :: Tar.FormatError -> [String]
entryFailMapper _ = ["Invalid Tar Entry"]

main :: IO ()
main = do
  fileContent <- GZip.decompress <$> BS.readFile "takeout.tgz"
  let entries = Tar.read fileContent
  let entryPaths = Tar.foldEntries foldEntryToPath [] entryFailMapper entries
  print entryPaths
