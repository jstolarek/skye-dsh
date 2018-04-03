Open questions, ideas, further steps and other TODOs
====================================================

  * At the moment where-provenance abstracts the key.  Can we abstract over the
    whole where-provenance representation?  One idea would be to provide a
    function that constructs provenance values based on some input, eg. table
    name and the row itself.  This function would have to be called explicitly
    in the rewritten query, I think.

  * Implicit where-provenance transformation that DSH now performs under the
    hood seems to be a special case of the view transformation problem,
    ie. having a view that differs from the actual underlying table.  Can we
    come up with a more general solution to this and only have where-provenance
    transformation as an instance of that general approach?

  * **Benchmark DSH:** in the where-provenance paper there are some benchmarks.
    It would be nice to re-do them in DSH and perhaps compare them to Links.
    One thing to check is comparing generated query plans for Links and DSH.
    Things to test:

      - performance of where-provenance using `Maybe` (ie. generating extra SQL
        query for each where-provenance annotated field) vs. approach that
        always assumes presence of provenance vs. Links

      - number of generated SQL queries.  In theory we know the number of
        queries that should be generated, but it is worth double-checking.

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

