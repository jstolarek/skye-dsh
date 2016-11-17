Open questions, ideas, further steps and other TODOs
====================================================

  * Figure out how function application works in DSH.  Can I use lambdas?  If I
    call my own functions what constructs can they contain?  What happens if
    they call functions unsupported by DSH?

  * Investigate further into DSH to understand how Haskell data types are
    reconstructed after receiving data from the execution of SQL query.  Can we
    insert where-provenance at this point?  Do we have all the required
    information to do that (table name, column name)?  I'm afraid these might be
    lost together with the table schema in `vecTableRef`.

  * **Benchmark DSH:** in the where-provenance paper there are some benchmarks.
    It would be nice to re-do them in DSH and perhaps compare them to Links.
    This is potentially time consuming, so a good idea would be start with some
    planning.  One thing to check is comparing generated query plans for Links
    and DSH.

  * Directly related to the above, how is record field selection compiled?  This
    is very important for dealing with automatic where-provenance annotations:

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

  * Provenance should index rows by their keys, not by integers.  But how do I
    encode that?  Existentials?  Type classes?