module Main where

import           System.Environment
import           Text.Printf

import           Database.HDBC
import           Database.HDBC.ODBC

import           Database.DSH             (Q, QA)
import           Database.DSH.Backend
import           Database.DSH.Backend.Sql
import           Database.DSH.Compiler

import qualified Queries.PPDP2016.Tours.NoProv            as NP
import qualified Queries.PPDP2016.Tours.WhereProv         as WP
import qualified Queries.PPDP2016.Tours.WhereProvPolyKeys as PK
import qualified Queries.PPDP2016.Tours.Lineage           as L
import qualified Queries.PPDP2016.Tours.LineagePolyKeys   as LP


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
            putStrLn "No provenance"
            execQ dshConn NP.q1
            execQ dshConn NP.q1'
            execQ dshConn NP.q1''
            execQ dshConn NP.q2

            putStrLn "Where-provenance"
            execQ dshConn WP.q1
            execQ dshConn WP.q1'
            execQ dshConn WP.q1''
            execQ dshConn WP.q2

            putStrLn "Where-provenance, polymorphic keys"
            execQ dshConn PK.q1
            execQ dshConn PK.q1'
            execQ dshConn PK.q1''
            execQ dshConn PK.q2

            putStrLn "Lineage"
            execQ dshConn L.q1
            execQ dshConn L.q1'
            execQ dshConn L.q1''
            execQ dshConn L.q2
            putStrLn "Lineage: FL primitves"
--            execQ dshConn L.q1map
            execQ dshConn L.q1append
--            execQ dshConn L.q1reverse

            putStrLn "Lineage, polymorphic keys"
            execQ dshConn LP.q1
            execQ dshConn LP.q1'
            execQ dshConn LP.q1''
            execQ dshConn LP.q2

{-
            putStrLn "Lineage, nested"
            execQ dshConn L.qNested

            putStrLn "Lineage bug"
            execQ dshConn L.lineageBug
            putStrLn "Lineage - broken let-bindings demo"
            execQ dshConn L.brokenLet
-}
            disconnect c
        _     -> do
            putStrLn "L.q2"
            mapM_ (\(f, h) -> putStrLn h >> f optResugar L.brokenLet)
                  debugFunctions
