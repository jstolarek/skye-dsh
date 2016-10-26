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

  - Some tests pass or fail randomly, eg. `Decimal`, `Scientific`.  Moreover, I
    witnessed these failures:

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

  - Several tests take ~10 minutes to finish

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

Most tests fail.  Q13 takes quite long time to finish.  Q22 does not finish in
reasonable time (killed after 1h).

```
TPC-H standard tests
  q1:  FAIL
    message threw an exception: Prelude.chr: bad argument: 5832776
  q2:  FAIL
    Exception: decimalVal: SqlDouble 9938.5302734375
    CallStack (from HasCallStack):
      error, called at src/Database/DSH/Backend/Sql/Pg.hs:162:51 in dsh-sql-0.3.0.1-16dwQPqpVig1rFKnsSPbCV:Database.DSH.Backend.Sql.Pg
  q3:  FAIL
    message threw an exception: Prelude.chr: bad argument: 5832776
  q4:  FAIL
    message threw an exception: Prelude.chr: bad argument: 5832776
  q5:  FAIL
    message threw an exception: Prelude.chr: bad argument: 5832776
  q6:  FAIL
    message threw an exception: Prelude.chr: bad argument: 5832776
  q7:  FAIL
    message threw an exception: Prelude.chr: bad argument: 5832776
  q8:  FAIL
    message threw an exception: Prelude.chr: bad argument: 5832776
  q9:  FAIL
    message threw an exception: Prelude.chr: bad argument: 5832776
  q10: FAIL
    message threw an exception: Prelude.chr: bad argument: 5832776
  q11: OK (0.11s)
  q12: FAIL
    message threw an exception: Prelude.chr: bad argument: 5832776
  q13: OK (106.23s)
  q14: FAIL
    message threw an exception: Prelude.chr: bad argument: 5832776
  q15: FAIL
    message threw an exception: Prelude.chr: bad argument: 5832776
  q16: OK (2.54s)
  q17: FAIL
    Exception: decimalVal: SqlDouble 348406.10714285716
    CallStack (from HasCallStack):
      error, called at src/Database/DSH/Backend/Sql/Pg.hs:162:51 in dsh-sql-0.3.0.1-16dwQPqpVig1rFKnsSPbCV:Database.DSH.Backend.Sql.Pg
  q18: FAIL
    Exception: dayVal: SqlInt32 765676800
    CallStack (from HasCallStack):
      error, called at src/Database/DSH/Backend/Sql/Pg.hs:165:45 in dsh-sql-0.3.0.1-16dwQPqpVig1rFKnsSPbCV:Database.DSH.Backend.Sql.Pg
  q22:
```


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

The last of three queries ends with an error.

```
["Customer#000001701","Customer#000003507","Customer#000010139","Customer#000027363","Customer#000029804","Customer#000029980","Customer#000036470","Customer#000043478","Customer#000047520","Customer#000047648","Customer#000049763","Customer#000055932","Customer#000057767","Customer#000064147","Customer#000066977","Customer#000068578","Customer#000077158","Customer#000080736","Customer#000106382","Customer#000112065","Customer#000112796","Customer#000121024","Customer#000134789","Customer#000135346","Customer#000136940","Customer#000137198","Customer#000137375","Customer#000138209","Customer#000149938"]
[("AFRICA                   ",["ALGERIA                  ","ETHIOPIA                 ","KENYA                    ","MOROCCO                  ","MOZAMBIQUE               "]),("AMERICA                  ",["ARGENTINA                ","BRAZIL                   ","CANADA                   ","PERU                     ","UNITED STATES            "]),("ASIA                     ",["INDIA                    ","INDONESIA                ","JAPAN                    ","CHINA                    ","VIETNAM                  "]),("EUROPE                   ",["FRANCE                   ","GERMANY                  ","ROMANIA                  ","RUSSIA                   ","UNITED KINGDOM           "]),("MIDDLE EAST              ",["EGYPT                    ","IRAN                     ","IRAQ                     ","JORDAN                   ","SAUDI ARABIA             "])]
runpg: decimalVal: SqlDouble 48.0
CallStack (from HasCallStack):
  error, called at src/Database/DSH/Backend/Sql/Pg.hs:162:51 in dsh-sql-0.3.0.1-16dwQPqpVig1rFKnsSPbCV:Database.DSH.Backend.Sql.Pg
```
