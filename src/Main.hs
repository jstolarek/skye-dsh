module Main where

import           System.Environment
import           Text.Printf

import           Database.HDBC
import           Database.HDBC.ODBC

import           Database.DSH             (Q, QA)
import           Database.DSH.Backend
import           Database.DSH.Backend.Sql
import           Database.DSH.Compiler

import qualified Queries.PPDP2016Tours      as Tours
import qualified Queries.PPDP2016ToursProv  as Prov

getConn :: String -> IO Connection
getConn dsn = connectODBC (printf "DSN=%s" dsn)

-- | Compile a DSH query to a bundle of PostgreSQL SQL queries, execute them and
-- print the resulting Haskell value.
execQ :: (QA a, Show a) => BackendConn PgVector -> Q a -> IO ()
execQ c q = runQ naturalPgCodeGen c q >>= print

debugFunctions :: [ (CLOptimizer -> Q a -> IO (), String) ]
debugFunctions =  -- CL
                  [ ( showComprehensionsQ, "CL: showComprehensionsQ" )
                  , ( showDesugaredOptQ  , "CL: showDesugaredOptQ"   )
                  -- FKL
                  , ( showLiftedQ        , "FKL: showLiftedQ"        )
                  , ( showLiftedOptQ     , "FKL: showLiftedOptQ"     )
                  , ( showFlattenedQ     , "FKL: showFlattenedQ"     )
                  , ( showFlattenedOptQ  , "FKL: showFlattenedOptQ"  )
                  ]


main :: IO ()
main = do
    argv <- getArgs
    case argv of
        [dsn] -> do
            c <- getConn dsn
            let dshConn = pgConn c
            putStrLn "Queries without provenance"
            execQ dshConn Tours.q1
            execQ dshConn Tours.q1'
            execQ dshConn Tours.q1''
            execQ dshConn Tours.q2

            putStrLn "Queries with built-in provenance tracking"
            execQ dshConn Prov.q1
            execQ dshConn Prov.q1'
            execQ dshConn Prov.q1''
            execQ dshConn Prov.q2

            disconnect c
        _     -> do
            putStrLn "Prov.q1"
            mapM_ (\(f, h) -> putStrLn h >> f optResugar Prov.q1)
                  debugFunctions
