{-# LANGUAGE Unsafe #-}

module Main (main) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Prelude

-- add :: Num a => a -> a -> a
-- add a b = a + b

main :: IO ()
main =
  do
    putStrLn "About to open sqlite database test1.db"
    conn <- connectSqlite3 "test1.db"
    putStrLn "Creating a table"
    _ <- run conn "CREATE TABLE IF NOT EXISTS test (id INTEGER NOT NULL, desc VARCHAR(80))" []
    putStrLn "Inserting data"
    _ <- run conn "INSERT INTO test (id) VALUES (?)" [toSql (1 :: Int)]
    putStrLn "Commiting"
    commit conn
    putStrLn "Disconnecting"
    disconnect conn
