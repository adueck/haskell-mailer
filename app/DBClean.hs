{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import DB qualified
import Database.PostgreSQL.Simple

clearDB :: Connection -> IO ()
clearDB conn = do
  _ <-
    execute
      conn
      "DROP TABLE IF EXISTS sends; DROP TABLE IF EXISTS mailings; DROP TABLE IF EXISTS contacts;"
      ()
  return ()

main :: IO ()
main = do
  putStrLn "WILL RESET DB"
  conn <- DB.makeDBConnection
  clearDB conn
  DB.setupDB conn
