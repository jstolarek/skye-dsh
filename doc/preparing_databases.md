Preparing PostgreSQL databases for DSH
======================================

This document describes how to set up database for the purpose of running
examples in `dsh-example-queries` package stored as a git submodule in
`modules/` directory.  This assumes you are running a local instance of
PostgreSQL, you have full administrative access to it and are not overly
concerned with security.

To create a database with PostgreSQL run `psql` command as `postgres` user.  Use
`su - postgres` as root or `sudo su - postgres` if you're a sudoer. Then on
the `psql` command prompt:

```
postgres=# CREATE DATABASE foo;
CREATE DATABASE
postgres=# GRANT ALL PRIVILEGES ON DATABASE foo TO bar;
GRANT
postgres=# ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TABLES TO bar;
ALTER DEFAULT PRIVILEGES
```

where `foo` is database name and `bar` is database user name.

**Note:** all the `load_*` scripts below use `COPY` SQL statement to insert data
  from CSV files into a database.  `COPY` requires superuser privileges.  There
  are two solutions:

  1. Run the scripts as `postgres` database user (this is the default user and
     has superuser privileges).  To do this run `sudo su - postgres` or `su -
     postgres` before actually running the `load_*` script.

  2. Grant your database user superuser privileges:

     ```
     postgres=# ALTER USER bar WITH SUPERUSER;
     ALTER ROLE
     ```

In what follows it is assumed that you followed the second approach.  If not,
remember to run `load_*` scripts as `postgres` user.

It is also assumed that your database user name is identical to your system user
name.  If not you need to provide your database user name using `-U` option when
invoking `psql` command.


TPC-H database
--------------

1. Generate TCP-H data:

   ```
   git clone https://github.com/gregrahn/tpch-kit
   cd tpch-kit/dbgen
   make
   ./dbgen
   ```

   This generates several `*.tbl` files, a total of 1GB of data.


2. Load generated data into Postgres using scripts from `dsh-example-queries`
   repo:

   ```
   cd loading/tpch/pg
   ./load.sh tpch /path/to/tpch-kit/
   psql -d tpch -a -f add_constraints.sql
   psql -d tpch -a -f additional_indexes.sql
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
   cd loading/shredding/pg
   psql -d shredding -a -f schema_shredding.sql
   ./load_shredding.sh shredding /path/to/skye-dsh/modules/dsh-example-queries 100
   psql -d shredding -a -f additional_indexes.sql
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
   ./dist/build/gen-trades/gen-trades -d 5
   ./dist/build/gen-flows/gen-flows
   ```

2. Create `trades` and `packets` databases and grant yourself full privileges on
   them.

3. Load generated data into Postgres using scripts from `dsh-example-queries`
   repo:

   ```
   cd loading/aquery/pg
   psql -d trades -a -f trades_schema.sql
   ./load_trades.sh trades /path/to/trades.csv
   psql -d trades -a -f trades_indexes.sql
   psql -d packets -a -f packets_schema.sql
   ./load_packets.sh packets /path/to/packets.csv
   psql -d packets -a -f packets_indexes.sql
   ```

   **Note:** Make sure to first import the data into the database and only then
     generate indices.  Otherwise it might take several hours to import data
     into the database.


PPDP2016 databases
------------------

These are the databases used by Stefan Fehrenbach and James Cheney for their
"Language integrated provenance" paper.  Databases are available as SQL dumps as
part of a [release on Stefan's
github](https://github.com/fehrenbach/links/releases/tag/ppdp2016-where).  These
dumps assume `postgres` as the database user.  Below I assume that `bar` is your
username and a call to `sed` is made to amend database dumps before loading
them.  Databases are available in different sizes: 4, 8, 16, 32, 64, 128, 256,
512, 1024, 2048 and 4096.  In the instructions below N represents that database
size. Substitute it with the size you want to use.

1. Create `ppdp2016` database and grant yourself full privileges on it.

2. Download a databse dump and modify it to contain your user name instead of
   default `postgres` user name

   ```
   wget https://github.com/fehrenbach/links/releases/download/ppdp2016-where/N.sql
   sed 's/postgres/bar/g' -i N.sql
   ```

3. Load the database dump:

   ```
   psql -d ppdp2016 -f N.sql
   ```

4. Create indices:

   ```
   wget https://raw.githubusercontent.com/fehrenbach/links/where/benchmarks/ppdp2016/indices.sql
   psql -d ppdp2016 -a -f indices.sql
   ```

There is also a very small database used in first few examples of the paper.

1. Create `ppdp2016travels` database and grant yourself full privileges on it.

2. Load the databse schema and data:

   ```
   cd db/ppdp2016
   psql -d ppdp2016travels -a -f travel_portal.sql
   ```


Setting up ODBC connections
---------------------------

To access created databases from Haskell you need to create ODBC connections for
them.  This can be done either using a graphical tool (`ODBCManageDataSourcesQ4`
on Debian 8) or by editing `~/.odbc.ini` file.  Give connections the same name
as the database they are connecting to.  Use your database user name.
