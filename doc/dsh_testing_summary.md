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
    Equality, Boolean Logic and Ordering
      cond ([[Integer]], [[Integer]]):     FAIL (0.29s)
        *** Failed! Assertion failed (after 3 tests and 2 shrinks):
        (False,([],[[]]))
        Use --quickcheck-replay '2 TFGenR 2D005362D7D7365E3765595553BD4B40291763FCE21FEA8A2AF0DB9B685E79A5 0 14680064 24 0' to reproduce.

      trig_asin:                           FAIL (0.02s)
        *** Gave up! Passed only 35 tests.
      trig_acos:                           FAIL
        *** Gave up! Passed only 25 tests.
      exp:                                 FAIL (0.05s)
        *** Gave up! Passed only 74 tests.
    ```

    Exact number of failing tests differs between runs, but these failures seem
    to be reproducable.

  - `deep`, `cartprod_deep` and `cartprod_deep_nested` tests take several
    minutes to finish

  - `partitionEithers` (`Either` test group) suffers from a memory leak.  I
    killed the process after using 14GB of RAM.

  - no tests beyond `partitionEithers` were run


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