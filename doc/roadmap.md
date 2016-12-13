Open questions, ideas, further steps and other TODOs
====================================================

  * **Benchmark DSH:** in the where-provenance paper there are some benchmarks.
    It would be nice to re-do them in DSH and perhaps compare them to Links.
    This is potentially time consuming, so a good idea would be start with some
    planning.  One thing to check is comparing generated query plans for Links
    and DSH.  Things to test:

      - performance of where-provenance using `Maybe` (ie. generating extra SQL
        query for each where-provenance annotated field) vs. approach that
        always assumes presence of provenance vs. Links

      - number of generated SQL queries.  In theory we know the number of
        queries that should be generated, but it is worth double-checking.

  * Investigate further into DSH to understand how Haskell data types are
    reconstructed after receiving data from the execution of SQL query.  Can we
    insert where-provenance at this point?  Do we have all the required
    information to do that (table name, column name)?  I'm afraid these might be
    lost together with the table schema in `vecTableRef`.

  * semantics of where-provenance should follow semantics of the query language.
    On the surface it looks like DSH is very similar to Links and that having
    where-provenance as a `Maybe` is sufficient.  However, under the hood it
    looks like DSH supports joins.  With joins `Maybe` no longer suffices to
    express where-provenance - we need sets.  So the question is: does DSH
    really compile queries to joins?  If so then in what situations do the DSH
    comprehensions compile to joins?

  * Implement flat approach to where-provenance, ie. without using a `Maybe`.
    One alternative is to assume it is always present, another is to assume a
    `Bool` tag that says whether provenance is present.  In such case we must be
    able to create default value of provenance.  This value is of course bogus
    but it would never be inspected.

  * **Automatic where-provenance annotations:** it should be fairly simple to
    write a TH function that adds where-provenance annotation to every field of
    a record type that represents a table row.  (Or perhaps a type family would
    work?)  The difficult bit is how to automatically modify an existing query
    to work on this modify this existing data type.  The problem with doing this
    using TH is that TH does not have any semantic information about the syntax
    it works on.  So for example if I am selecting a record field TH will only
    see an application and two identifier names.  It has no means of telling
    whether the function is a record selector.  Although it is possible to come
    up with naming conventions to address this, this would be very fragile
    approach.  Perhaps another possible approach would be to deal with this at
    DSH level.  Perhaps extra compilation pass would work?  So for example I
    could say "compile this query with where-provenance support" and this extra
    pass would figure out where to put where-provenance information.

  * It seems that `QA` and `TA` classes overlap.
