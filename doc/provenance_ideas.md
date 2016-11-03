Ideas on extending DSH with provenance tracking
===============================================

Where-provenance could be tracked with a triple `(String, String, Integer)`
standing for table name, column name and row id.  Probably a better idea would
be to define `Provenance` record with three records and then write `QA` instance
for it.  I'm not sure if the `View` instance will be necessary - see later.

Now I could mark provenance tracking by introducing `Prov` at the type level
like this:

```haskell
data Agency = Agency
    { a_id       :: Integer
    , a_name     :: Prov Text
    , a_based_in :: Text
    , a_phone    :: Text
    } deriving (Show)
```

It is not yet clear to me what `Prov` should be.  One idea is to have it as an
identity type family defined for base types only (those types that allow
provenance tracking).  The purpose of `Prov` type family would be to simply
detect its presence during TH transformations and generate extra provenance
information for a field which type is `Prov sth`.  (Note that TH can't tell
whether `Prov` is a type family, type constructor or type synonym - it only sees
a type application.) A rough idea is:

  - in the `QA` instance generate extra tuple fields in `Rep` instance.
    Hopefully extra tuple field like `Rep Provenance` will be permitted.  So in
    the above example I would get:

    ```haskell
    instance QA Agency where
      type Rep Agency = (Rep Integer, Rep Text, Rep Provenance, Rep Text, Rep Text)
    ```

    It is not clear to me how to connect provenance to a particular field.
    (Quote from paper, sec. 3.1: _"Provenance information has meaning only in
    the context of data it belongs to."_) In the example above I placed the
    provenance info right after a field it is assigned to but that seems very
    fragile, more of a hack than a proper solution.  More on this later.

    Generating appropriate definitions of `toExp`/`fromExp` should be
    straightforward.

  - I think `View` instance should take provenance into account.  This will
    later be used by `generateTableSelectors` to generate selectors that allow
    to read provenance.  So in the example above we would have:

    ```haskell
    a_nameQ :: Q Agency -> Q Text
    a_nameQ (view -> (_, name, _, _, _)) = name

    prov_a_nameQ :: Q Agency -> Q Provenance
    prov_a_nameQ (view -> (_, _, prov, _)) = prov
    ```

  - Table schema must contain information about provenance columns.  One
    solution is to do this completely explicitly, ie. require programmer to list
    and name provenance columns.  Another approach would be to create a simple
    function to automatically generate provenance columns.  So for example:

    ```haskell
    agencies :: Q [Agency]
    agencies = table "agencies"
                   ( "a_id" :|
                     prov "a_name" ++
                   [ "a_based_in"
                   , "a_phone"
                   ])
                   (TableHints (pure $ Key (pure "a_id") ) NonEmpty)
    ```

    where `prov a_name` would return `["a_name", "a_name_prov_table",
    "a_name_prov_column", "a_name_prov_row"]`.

That more or less covers the treatment of provenance on the surface.  I do not
yet have any idea how to handle provenance under the hood, especially the fact
that it must be tracked implicitly.

Some open questions and loose thoughts
--------------------------------------

  * Should I add provenance to internal type language?  My guess is this would
    simplify (or perhaps even enable?) handling of provenance under the hood.
    Still it is not clear to me how to make a connection between provenance
    information and the field it is supposed to be assigned to.  (Quote from
    paper: _"The type system should capture the special nature of provenance
    meta-data."_)

    BTW. Does the DSH's internal type system have any typing rules written down
    somewhere?  I should probably ask Alex.

  * Directly connected to the previous question is the problem of propagating
    provenance implicitly under the hood.  Two things how to do this come to my
    mind:

      - Handle provenance tracking during comprehension desugaring.  I'm not
        sure if this would be a good idea.  I think that desugaring should only
        insert information that provenance is supposed to be tracked but without
        assigning any semantic meaning.  In other words desugaring should only
        do desugaring.

      - Handle provenance during translation to CL.  `translate` function would
        convert provenance annotations into concrete query constructs.  This
        means that `CL` and any subsequent internal representation (NKL, FKL,
        SL) would not have to be modified.

  * The consequence of proposed representation is that provenance could only be
    inspected (via generated table selectors) but not modified in any way.
    Also, perhaps it would make sense to generate a separate type class for
    viewing provenance?  I'm not sure if this has any merit but the thought
    crossed my mind at one point so I'm noting it down.

  * One thing to keep in mind is that DSH assumes maximum tuple width to be 16.
    So it would be possible to attach provenance to at most 8 fields.  That's
    not a theoretical problem but could potentially limit practical
    applications.  Of course it is trivially simple to modify DSH to allow wider
    tuples but, as usual with TH, performance might drop.

  * It should be possible to use DSH's internal type information to guide basic
    provenance transformations.  However, I doubt this approach will work for
    some complex transformations.  I fear that using type information provided
    in the AST will sooner or later require a full implementation of type
    checking and inference algorithm.  Needless to say, this will be far from
    trivial.

Some notes from "Language-integrated Provenance in Links" paper
---------------------------------------------------------------

### Section 3.1 - Type system

Four requirements for a type system that tracks provenance:

  - **"provenance is attached to data and automatically propagated"**

    Still need to figure out how to do this - see earlier discussion.

  - **"type system should prevent accidental modification [of provenance]"**

    I think this will be prevented by the surface encoding.  There will only be
    a view that allows to extract provenance from internal representation.  This
    should allow to write queries that inspect provenance - cf. example in the
    paper in Figure 2, where comment provenance is queried.

  - **"changes to data would invalidate where-provenance"**

    I think this is closely related to (1).  This definitely needs to be
    implemented explicitly in the query translation.

  - **"it is not possible for a programmer to forge provenance"**

    Loosely related to (2).  I fear the proposed approach might be susceptible
    to provenance forgery in a quite simple way.  Programmer could write a data
    type that explicitly contains provenance as one of the fields.  In this way
    provenance would become available for modification in the surface language.
    And since internal representation of such a data type would be identical to
    data type with implicit provenance, it could be used to forge provenance.
    Moreover, queries over this data type would be compatible with the database
    schema.  It seems that the only hope here is that `table` declarations have
    an assigned type.  Then again, programmer can write a new `table`
    declaration that works with forged data type.  OTOH, is that really a
    problem?  If a programmer has full access to the database they can do
    literally anything with it.

In the paper provenance can only be attached to base types.  In my proposal this
would be achieved by defining `Prov` as a closed type family with instances only
for those types for which we can track provenance.
