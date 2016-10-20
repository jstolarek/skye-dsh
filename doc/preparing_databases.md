Preparing PostgreSQL databases for DSH
======================================

This document describes how to set up database for the purpose of running
examples in `dsh-example-queries` package stored as a git submodule in
`modules/` directory.

To create a database with PostgreSQL run `psql` command as `postgres` user.  Use
`su - postgres` as root or `sudo su - postgres` if you're a sudoer. Then on
the `psql` command prompt:

```
postgres=# create database foo;;
CREATE DATABASE
postgres=# grant all privileges on database foo to bar;;
GRANT
```

where `foo` is database name and `bar` is database user name.


TPC-H database
--------------

1. Generate TCP-H data:

   ```
   git clone https://github.com/gregrahn/tpch-kit
   cd tpch-kit
   make
   ./dbgen
   ```

   This generates several `*.tbl` files, a total of 1GB of data.


2. Load generated data into Postgres using scripts from `dsh-example-queries`
   repo:

   ```
   cd modules/dsh-example-queries/loading/tpch/pg
   sudo su - postgres
   ./load.sh tpch /path/to/tpch-kit/
   psql -d tpch -a -f add_constraints.sql
   psql -d tpch -a -f additional_indexes.sql
   psql -d tpch -a -f dshifiy-tpch.sql
   ```

   The `load.sh` script drops `tpch` database if it existed, which means you
   need to grant yourself permissions to that database.


Shredding database
------------------

1. Build `dsh-example-queries`:

   ```
   cd modules/dsh-examples-queries
   cp ../../cabal.sandbox.config
   cabal configure
   cabal build
   ```

   The above assumes that cabal sandbox in the main directory of the project
   exists and all the project dependencies have been installed with `cabal
   install --dependencies-only`.

2. Run the created generator:

   ```
   ./dist/build/gen-organisation/gen-organisation
   ```

   This will create `contacts100.csv`, `departments100.csv`, `employees100.csv`,
   `tasks100.csv`, where 100 stands for number of generated entries.  This can
   be changed by passing `-d N` flag to the generator, where `N` stands for
   number of entries to generate.

3. Create `shredding` database and grant yourself full privileges on it.

4. Load generated data into Postgres using scripts from `dsh-example-queries`
   repo:

   ```
   cd modules/dsh-example-queries/loading/shredding/pg
   sudo su - postgres
   psql -d shredding -a -f schema_shredding.sql
   psql -d shredding -a -f additional_indexes.sql
   ./load_shredding.sh shredding /path/to/skye-dsh/modules/dsh-example-queries 100
   ```

   **Notes**:

     - `load_shredding` does not create the `shredding` database so it is
       important that it already exists.

     - loading script will not expand `..` in directory location, so the
       following does not work:

       ```
       ./load_shredding.sh shredding ../../.. 100
       ```


AQuery databases
----------------

There are two separate sets of example queries, so the following steps will
create two databases.

1. Assuming you have already built `dsh-example-queries` (see previous section):

   ```
   ./dist/build/gen-trades/gen-trades -d 5 -s 100 -t 1000
   ./dist/build/gen-flows/gen-flows
   ```

   **Note**: with default setting `gen-trades` generates LOTS of data.  Loading
     trades generated for 5 days takes several hours.  This is most likely
     caused by indexing.  According to pgAdmin indices take several times more
     space than the data itself.

2. Create `trades` and `packets` databases and grant yourself full privileges on
   them.

3. Load generated data into Postgres using scripts from `dsh-example-queries`
   repo:

   ```
   cd modules/dsh-example-queries/loading/aquery/pg
   sudo su - postgres
   psql -d trades -a -f trades_schema.sql
   psql -d trades -a -f trades_indexes.sql
   psql -d packets -a -f packets_schema.sql
   psql -d packets -a -f packets_indexes.sql
   ./load_trades.sh trades /path/to/trades.csv
   ./load_packets.sh packets /path/to/packets.csv
   ```


Setting up ODBC connections
---------------------------

To access created databases from Haskell you need to create ODBC connetcions for
them.  This can be done either using a graphical tool (`ODBCManageDataSourcesQ4`
on Debian 8) or by editing `~/.odbc.ini` file.  Give connections the same name
as the database they are connecting to.  Use your database user name.
