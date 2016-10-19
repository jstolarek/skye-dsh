module Main where

import           Control.Monad

import           Database.HDBC.ODBC

import           Database.DSH.Backend
import           Database.DSH.Backend.Sql

getConn :: String -> IO (BackendConn PgVector)
getConn connString = pgConn <$> connectODBC connString

main :: IO ()
main = void $ getConn "DSN=skye-simple"
