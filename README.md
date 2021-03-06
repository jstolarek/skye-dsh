Skye-DSH
========

Skye-DSH is an experimental extension of [Database-Supported
Haskell](https://github.com/ulricha/dsh) with support for provenance tracking as
described in ["Language-integrated Provenance in
Haskell"](https://arxiv.org/abs/1803.10202), Jan Stolarek and James Cheney, The
Art, Science, and Engineering of Programming, 2018, Vol. 2, Issue 3, Article 11.
This project is a meta-project that uses git submodules to group together forks
of several DSH subprojects.  The main development branch is `provenance` in the
`dsh` submodule.  Skye-DSH currently supports *where-provenance* and *lineage*
tracking.


Building and running
--------------------

1. Get the sources:

   ```
   git clone https://github.com/jstolarek/skye-dsh
   cd skye-dsh
   git submodule init
   git submodule update
   ```

2. Install dependencies into a Cabal sandbox:

   ```
   ./sandbox-setup.sh
   cabal install --dependencies-only
   ```

3. Build and run the executable:

   ```
   cabal build
   ./dist/build/skye-dsh/skye-dsh ppdp2016travels
   ```

This assumes you have prepared the PPDP 2016 Travels database and a
corresponding ODBC connection.  See
[here](https://github.com/jstolarek/skye-dsh/blob/master/doc/preparing_databases.md#ppdp2016-databases)
for a description how to do that.  On Debian 8 (and probably some other Linux
distros) you can run `ODBCManageDataSourcesQ4` to see a list of configured ODBC
connections.


Documentation
-------------

There is no user documentation at the moment, but studying examples in
`src/Schema` and `src/Queries` should give you an idea how provenance works.
