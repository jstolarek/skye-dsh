Ideas on extending DSH with provenance tracking
===============================================

Where-provenance representation
-------------------------------

Where-provenance can be represented as:

```haskell
data WhereProvData = WhereProvData
    { where_prov_table  :: Text
    , where_prov_column :: Text
    , where_prov_key    :: Integer
    } deriving ( Show )
```

**Open question:** `where_prov_key` is currently encoded as `Integer` but it
would be nice if this field was polymorphic and admitted any key that allows to
identify a row.  I also wonder how the choice of int interacts with the backend
that seems to arbitrarily pick the key (see definition of `vecTableRef` in
`dsh-sql` and later dsicussion in this document).

Important requirements for where-provenance are:

  - where-provenance representation should be made abstract to prevent
    programmer from creating values of that type (cf. "Language-integrated
    provenance", Section 3.1).
  - it must be attached to a particular record field
  - it can only be tracked for basic types
  - it may be absent, ie. the value was not copied but created by the query

First requirement can be achieved by not exporting constructors.  Remaining
three requirements can be achieved using a closed type family defined for basic
data types for which we track provenance:

```haskell
type WhereProvInfo = Maybe WhereProvData

type family WhereProv a where
     WhereProv Text    = ( Text   , WhereProvInfo )
     WhereProv Integer = ( Integer, WhereProvInfo )
     WhereProv Bool    = ( Bool   , WhereProvInfo )
```

Now we can mark provenance tracking by using `WhereProv` like this:

```haskell
data Agency = Agency
    { a_id       :: Integer
    , a_name     :: Text
    , a_based_in :: Text
    , a_phone    :: WhereProv Text
    } deriving (Show)
```

This attaches provenance information to `a_name` field.  The extra purpose of
`WhereProv` is to be detected during TH transformations.  This will allow to
generate different view patterns that will allow to access provenance
components.  Note that TH can't tell whether `Prov` is a type family, type
constructor or type synonym - it only sees a type application.

There will also be some helper functions to drop/extract provenance information:

```haskell
provQ   :: (QA a) => Q (a, WhereProvInfo) -> Q WhereProvInfo
unprovQ :: (QA a) => Q (a, WhereProvInfo) -> Q a
```

Note that because of defining `WhereProv` as a type family it is impossible to
write the type signatures of the above functions as:

```haskell
provQ   :: (QA a) => Q (WhereProv a) -> Q WhereProvInfo
unprovQ :: (QA a) => Q (WhereProv a) -> Q a
```

The reason is that the type checker does not know that `WhereProv a` will always
be a tuple.  The alternatives could be to turn `WhereProv` into a type synonym.
However, it would allow to declare where-provenance tracking for any data type.
I don't think this error would be easy to catch.  Note also that the proposed
type signature of `prov` would require to declare `WhereProv` as injective.


Where-provenance implementation - technical details
---------------------------------------------------

A rough idea to implementing where-provenance is outlined below, based on the
example of tracking where-provenance of `a_phone` field in the `Agency` data
type given above.


### `QA` instances

The idea here is to do nothing special and generate `QA`
instances without any modifications.  This will result in:

```haskell
instance QA Agency where
  type Rep Agency = (Rep Integer, Rep Text, Rep Text, Rep (WhereProv Text))
```

and straightforward definitions of `toExp`/`frExp`.  The major advantage of
this approach is that provenance information is directly attached to the
data value it belongs to.  (Quote from paper, sec. 3.1: _"Provenance
information has meaning only in the context of data it belongs to."_) The
downside is that with the current implementation of DSH nested tuples are
not permitted, so this will result in a compilation error when translating
from frontend language to CL language.  This is discussed later.


### `View` instance

Remains unchanged, but selectors generated with `generateTableSelectors` now
have to take provenance into account. In the example above we have:

```haskell
a_phoneQ :: Q Agency -> Q Text
a_phoneQ (view -> (_, _, _, name)) = unprovQ name

a_phone_provQ :: Q Agency -> Q (WhereProvInfo)
a_phone_provQ (view -> (_, _, _, name)) = provQ name
```


### Smart constructor

Smart constructor now needs to insert empty provenance information, ie. a
`Nothing`.  Internally `Nothing` is represented as an empty list, so we have:

```haskell
agency :: Q Integer -> Q Text -> Q Text -> Q Text -> Q Agency
agency (Q a_id) (Q a_name) (Q a_based_in) (Q a_phone)
  = Q (TupleConstE (Tuple4E a_id a_name a_based_in
                   (TupleConstE (Tuple2E a_phone (ListE $ S.empty)))))
```

The consequence of proposed representation is that provenance can only be
inspected (via generated table selectors) but not modified or forged in any way.


### Table declaration

This seems to be the crucial bit.  Table declaration, when evaluated, yields
data extracted from a database.  Recall that `table` declaration: a) provides a
list of columns of a database table; and b) has type `Q [Agency]`.  However,
with where-provenance added we're in a situation where the Haskell data type
structure does not match structure of a data base.  So what we need to do is
modify the representation of data returned by evaluating table declaration
expression.  In other words, we want to attach where-provenance on the fly,
after the data is pulled out of the data base.  To do this we need to know which
fields need to have where-provenance information.  I propose:

```haskell
agencies :: Q [Agency]
agencies = table "agencies" ( "a_id" :| [ "a_name", "a_based_in", "a_phone" ])
                 (TableHints (pure $ Key (pure "a_id") ) NonEmpty
                             (WhereProvenance $ pure "a_phone"))
```

Note the last line that contains extra hint about which fields need extra
where-provenance information.  At this point it is not yet clear to my how
exactly this information will be used under the hood.  Nevertheless, this
information should suffice.


### [WIP] Passing table with provenance annotations down the compilation pipeline

Table expression generated with `table` function is translated into
`BaseTableSchema` - see ["Translation of table declarations into
CL"](https://github.com/jstolarek/skye-dsh/blob/master/doc/understanding_dsh.md#translation-of-table-declarations-into-cl).
Here we encountr first bump: it is not permitted to have tuples as fields in a
data type.  Unfortunatelly, field annotated with where-provenance is a tuple.
And so attempting to run a query with provenance information results in error
`skye-dsh: Non-scalar types in table agencies`.  A possible solution might be to
use provenance hint in table declaration to modify the type of field on the fly
and thus pass the check.  It is also crucial to include where-provenance
information in `BaseTableSchema`.  `BaseTableSchema` is passed unchanged all the
way to the SQL backend in `dsh-sql` library.

This is as far as my knowledge goes at the moment.  Conceptually, we need to
attach provenance information to the data once it is pulled out of the data
base.  Below is a rough list of open questions:

  * it seems that `BaseTableSchema` is passed all the way down to `dsh-sql` and
    used in `Database.DSH.Backend.Sql.Relational.Natural` module in the function
    `vecTableRef` (in the `instance SegmentAlgebra TableAlgebre` declaration).
    What this function seems to be doing is to construct some abstract
    representation of table with information about columns, their required
    projections and sorting order.  This is not where the provenance information
    should go - the query needs to be executed without any modifications.
    However, the bad news is that the database schema seems to be discarded at
    this point.  If this is indeed the case then things get complicated.

  * DSH seems to compile a query down to some sort of abstract algebra.  DAG in
    the name of the library suggests directed acyclic graphs - see also
    `AlgebraDag` data type in `Database.Algebra.Dag` module, `algebra-dag`
    library.  I have absolutely no idea how this representation works

  * I have not yet located place in the code where the data pulled from the data
    based is actually assembled into a result.  I believe this is where the
    provenance information should be added.

  * DSH adds implicit ordering information to maintain list semantics.  Figuring
    out how this is done could be helpful.  Adding where-provenance might turn
    out to be similar.


Some open questions and loose thoughts
--------------------------------------

  * Should I somehow add provenance to internal type language?  (Quote from
    paper: _"The type system should capture the special nature of provenance
    meta-data."_) Does the DSH's internal type system have any typing rules
    written down somewhere?  I should probably ask Alex.

  * It should be possible to use DSH's internal type information to guide basic
    provenance transformations.  However, I doubt this approach will work for
    some complex transformations.  I fear that using type information provided
    in the AST will sooner or later require a full implementation of type
    checking and inference algorithm.  Needless to say, this will be far from
    trivial.

  * The above discussion concentrates on where-provenance.  But what about other
    forms of provenance?  Can they be expressed using this approach?  It would
    be nice to come up with something that allows extension to other forms of
    provenance and a uniform treatment of all of them.

  * I assumed that each row can be identified with an `Integer`.  Ideally, we'd
    like this to be polymorphic, ie. we want to by able to identify a row by a
    key value.  There are several problems here though.  One problem is how to
    expose this information to a library user?  Do we want this polymorphism to
    be exposed in type or do we want to hide it as an existential?  I'm also not
    sure how to treat this under the hood.  Most importantly, if a table has
    several keys will it be possible to use any of them for provenance tracking?
    There is code that hints that might not be the case - see module
    `Database.DSH.Backend.Sql.Relational.Natural`, the call to `chooseBaseKey`
    inside `vecTableRef`.


Where-provenance by explicit construction
-----------------------------------------

Downsides of this approach:

  * duplicate data type required, need some way to handle name clashes

  * two SQL queries generated:

    ```sql
    SELECT a2.a_id AS r1, a3.et_id AS r2, a2.a_id AS o1,
           a3.et_id AS o2, 'agencies' AS i1, 'a_phone' AS i2, a2.a_id AS i3
    FROM agencies AS a2, externaltours AS a3
    WHERE (a2.a_name = a3.et_name) AND (a3.et_type = 'boat')
    ORDER BY r1 ASC, r2 ASC, o1 ASC, o2 ASC

    SELECT a0.a_id AS k1, a1.et_id AS k2, a0.a_id AS o1,
           a1.et_id AS o2, a1.et_name AS i1, a0.a_phone AS i2
    FROM agencies AS a0, externaltours AS a1
    WHERE (a0.a_name = a1.et_name) AND (a1.et_type = 'boat')
    ORDER BY o1 ASC, o2 ASC
    ```

    This happens because provenance annotation is a `Maybe`, which is internally
    represented by DSH as a list (`Just` is a singleton list, `Nothing` is an
    empty list).  If we assume that the provenance annotation is always present
    we get a single SQL query just as we would expect:

    ```SQL
    SELECT a0.a_id AS o1, a1.et_id AS o2, a1.et_name AS i1,
           a0.a_phone AS i2, 'agencies' AS i3, 'a_phone' AS i4,
           a0.a_id AS i5
    FROM agencies AS a0,  externaltours AS a1
    WHERE (a0.a_name = a1.et_name) AND (a1.et_type = 'boat')
    ORDER BY o1 ASC, o2 ASC
    ```

  * smart constructor for `whereProvData` required, so we expose internal
    implementation of where-provenance.  Perhaps this wouldn't be necessary if
    we generated code with TH.




Some notes on "Language-integrated Provenance in Links" paper
-------------------------------------------------------------

### Section 3.1 - Type system

Four requirements for a type system that tracks provenance:

  - **"provenance is attached to data and automatically propagated"**

    Still need to figure out how to do this - see earlier discussion.

  - **"type system should prevent accidental modification [of provenance]"**

    I think this will be prevented by the surface encoding.  There will only be
    a view that allows to extract provenance from internal representation.  This
    should allow to write queries that inspect provenance - cf. example in the
    paper in Figure 2, where comment provenance is queried.  Another thing to do
    is to ensure encapsulation of provenance abstractions.  This should be
    easily done using Haskell modules.

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
would be achieved by defining `WhereProv` as a closed type family with instances
only for those types for which we can track provenance.


### Section 3.2 - Translation

There are two things I don't understand in this section.  Firstly, in general
where-provenance might be absent if a value was generated by a query and not
copied from another table.  (This problem does not arise with the example in the
paper but in general it is possible.)  How would such a situation be represented
by proposed encoding?  Secondly, the paper does not seem to say how to compute
provenance.  It assumes that all the provenance information is somehow magically
present in `top_comments` table and then just shuffles it around.


Some notes on "Language-integrated Provenance" paper
----------------------------------------------------

### Section 1 - Introduction

_"no prior work considers the interaction of provenance with clients of the
database"_ - WAT?  As in "nobody ever implemented these ideas"?


### Section 2.4 - Where-provenance and lineage

Attaching `prov` annotation to a table field adds extra provenance information
to that field: table and column name (these are the same for all entries) and id
of corresponding row.  This is rather simple.  I think the biggest difficulty
here is assigning id number based on database row.  However, in this system
provenance data is managed explicitly.  For example:

```links
var q1''' = query {
  for (a <-- agencies)
    for (e <-- externalTours)
    where (a.name == e.name && e.type == "boat")
      [(name = e.name,
        phone = data a.phone, p_phone = prov a.phone)]
}
```

Here provenance is managed explicitly.  I was thinking that provenance should be
handled implicitly:

```links
var q1''' = query {
  for (a <-- agencies)
    for (e <-- externalTours)
    where (a.name == e.name && e.type == "boat")
      [(name = e.name, phone = a.phone)]
}
```

Here, since `phone` field is annotated with provenance information, provenance
would be implicitly assigned to `phone` field in the result.  Of course if we
wanted to inspect provenance then we would still have to extract it explicitly.


### Section 3.1

This section clarifies how where-provenance is propagated.  The idea is that by
default provenance information is attached to a particular field and propagated
together with value of that field.  However, users are allowed to define their
own functions that compute provenance.  The intended use case is to fetch
provenance metadata stored separately from the actual data.  This is probably
something worth having in the final system, but at the moment I don't have a
good idea how to incorporate this into my encoding.
