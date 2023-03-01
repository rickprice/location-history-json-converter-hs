{-# LANGUAGE Unsafe #-}

module Main (main) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS

foldEntryToPath :: Tar.Entry -> [String] -> [String]
foldEntryToPath entry list = list ++ [show $ Tar.entryPath entry]

-- Converts TAR errors to a string.
entryFailMapper :: String -> [String]
entryFailMapper err = [err]

main :: IO ()
main = do
  fileContent <- fmap GZip.decompress $ BS.readFile "foo.tar.gz"
  entries <- fmap Tar.read fileContent :: Tar.Entries
  -- Here I don't know how to correctly apply fmap
  entryPaths <- Tar.foldEntries foldEntryToPath [] entryFailMapper entries :: [String]
  -- This should print ["a.txt", "b.txt", "c.txt"]
  print entryPaths
