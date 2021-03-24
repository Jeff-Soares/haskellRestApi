{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Database.SQLite.Simple

initDB :: FilePath -> IO ()
initDB dbfile = withConnection dbfile $ \conn ->
    execute_ conn
        "CREATE TABLE IF NOT EXISTS products (id INTEGER PRIMARY KEY, name TEXT not null, price REAL not null)"

main :: IO ()
main = do
    initDB dbfile
    startApp dbfile
    where
        dbfile = "data.db"
