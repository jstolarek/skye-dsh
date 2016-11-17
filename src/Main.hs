module Main where

import           System.Environment
import           Text.Printf

import           Database.HDBC
import           Database.HDBC.ODBC

import           Database.DSH             (Q, QA)
import           Database.DSH.Backend
import           Database.DSH.Backend.Sql
import           Database.DSH.Compiler

import           Queries.PPDP2016Tours
--import qualified Queries.PPDP2016ToursProv  as Prov
import qualified Queries.PPDP2016ToursProv2 as Prov2

getConn :: String -> IO Connection
getConn dsn = connectODBC (printf "DSN=%s" dsn)

-- | Compile a DSH query to a bundle of PostgreSQL SQL queries, execute them and
-- print the resulting Haskell value.
execQ :: (QA a, Show a) => BackendConn PgVector -> Q a -> IO ()
execQ c q = runQ naturalPgCodeGen c q >>= print

main :: IO ()
main = do
    argv <- getArgs
    case argv of
        [dsn] -> do
            c <- getConn dsn
            let dshConn = pgConn c
            execQ dshConn q1
            execQ dshConn q1'
            execQ dshConn q1''
            execQ dshConn q2
            -- queries with provenance tracking
            execQ dshConn Prov2.q1
            execQ dshConn Prov2.q1'
            execQ dshConn Prov2.q1''
            execQ dshConn Prov2.q2
            disconnect c
        _     -> do
            putStrLn "Prov2.q1"
            showComprehensionsQ optResugar Prov2.q1
            putStrLn "Prov2.q1'"
            showComprehensionsQ optResugar Prov2.q1'
