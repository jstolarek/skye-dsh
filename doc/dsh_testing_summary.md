Running DSH Tests: A Summary
============================

This document summarizes my results from running various tests provided by
`dsh-sql` package.


TestBasic
---------

This test is located in `testsuite` directory. It imports tests from `DSH`
package (`Database.DSH.Tests` module) and runs them.  It seems that these tests
don't need a database with specific data inside.

### Running the test

```
cd modules/dsh-sql
cabal configure
cabal build
./dist/build/test-basic/test-basic dummy
```

where `dummy` is an empty database.

### Results

  - Some tests pass or fail randomly, eg. `Decimal`, `Scientific`:

    ```
    Decimal:                             FAIL (0.03s)
      *** Failed! Assertion failed (after 64 tests and 2 shrinks):
      (Positive {getPositive = 1},11)
      Use --quickcheck-replay '63 TFGenR 128A6DDAADB1315EF0329A85F95D79A84BB684F8B056A3577540711A3E58C28C 0 31 5 0' to reproduce.
    Scientific:                          FAIL (0.02s)
      *** Failed! Assertion failed (after 38 tests and 1 shrink):
      (11,-1)
      Use --quickcheck-replay '37 TFGenR 00000000F8E208E500000000000186A0000000000000E15800000001DCD65000 0 17592186044352 44 0' to reproduce.
    ```

  - Moreover, I witnessed these failures:

    ```
    cond ([[Integer]], [[Integer]]):     FAIL (0.61s)
      *** Failed! Assertion failed (after 6 tests and 4 shrinks):
      (False,([],[[]]))
      Use --quickcheck-replay '5 TFGenR 9BAF90A7BFB62B267FC981A3D3AAFA553304737698E4EDC9F1FF0BF70A3B715C 0 132120576 27 0' to reproduce.
    Numerics
      trig_asin:                           FAIL (0.02s)
        *** Gave up! Passed only 33 tests.
      trig_acos:                           FAIL (0.02s)
        *** Gave up! Passed only 32 tests.
      exp:                                 FAIL (0.04s)
        *** Gave up! Passed only 70 tests.
    Lists
      sortWith [(Char,)]:                  FAIL (0.04s)
        *** Failed! (after 5 tests and 1 shrink):
        Exception:
          DSH: Impossible happened at ("src/Database/DSH/Backend/Sql/Pg.hs",150,68)
          CallStack (from HasCallStack):
            error, called at src/Database/DSH/Backend/Sql/Pg.hs:150:68 in dsh-sql-0.3.0.1-16dwQPqpVig1rFKnsSPbCV:Database.DSH.Backend.Sql.Pg
        [('\NUL',0)]
        Use --quickcheck-replay '4 TFGenR 0BDE0A4DA2BFFEEE57406D11AE71212334343458653052C880D48808544F6AB1 0 69805794224242688 56 0' to reproduce.
      zip tuple1:                          FAIL
        Exception: Prelude.chr: bad argument: 3604528
      zip tuple2:                          FAIL
        Exception: Prelude.chr: bad argument: 3604528
    Lifted operations
      map asin:                            FAIL (0.02s)
        *** Gave up! Passed only 47 tests.
      map acos:                            FAIL (0.03s)
        *** Gave up! Passed only 48 tests.
      map exp:                             FAIL (0.04s)
        *** Gave up! Passed only 44 tests.
    Combinations of operators
      map elem + sort:                     FAIL (0.04s)
        *** Failed! Assertion failed (after 2 tests and 4 shrinks):
        ([],[(0,'a')])
        Use --quickcheck-replay '1 TFGenR C5D04289EF2A44EDA211FA97DA60B5A12650CF5FEC1B1E785D2DAF9C29B50A23 0 13510798882111488 54 0' to reproduce.
      filter elem + sort:                  FAIL (0.39s)
        *** Failed! Assertion failed (after 11 tests and 7 shrinks):
        ([-6,-2],[(-6,'a'),(-2,'A')])
        Use --quickcheck-replay '10 TFGenR C5D04289EF2A44EDA211FA97DA60B5A12650CF5FEC1B1E785D2DAF9C29B50A23 0 18437736874454810624 64 0' to reproduce.
    ```

    Exact number of failing tests differs between runs, but these failures seem
    to be reproducable.

  - `deep`, `cartprod_deep` and `cartprod_deep_nested` tests take several
    minutes to finish


TestTPCH
--------

This test is located in `testsuite` directory. It imports queries from module
`Queries.TPCH.Standard` in `dsh-example-queries` package and executes these
queries on a database containing TPC-H data.

### Running the test

```
cd modules/dsh-sql
cabal configure
cabal build
./dist/build/test-tpch/test-tpch tpch
```

where `tpch` is a database containing TPC-H data prepared according to
description given in [Preparing PostgreSQL databases for
DSH](https://github.com/jstolarek/skye-dsh/blob/master/doc/preparing_databases.md)
document.

### Results

Q22 does not finish in reasonable time (killed after ~10 minutes).


RunPg
-----

This test is located in the top directory of `dsh-sql` package.  It runs three
queries against TPC-H database.

### Running the test

```
cd modules/dsh-sql
cabal configure
cabal build
./dist/build/runpg/runpg tpch
```

where `tpch` is a database containing TPC-H data prepared according to
description given in [Preparing PostgreSQL databases for
DSH](https://github.com/jstolarek/skye-dsh/blob/master/doc/preparing_databases.md)
document.

### Results

All tests passing.