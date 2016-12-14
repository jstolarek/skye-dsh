Ideas on extending DSH with provenance tracking
===============================================

Where-provenance representation
-------------------------------

Where-provenance is represented as:

```haskell
data WhereProvAnnot a = WhereProvAnnot
    { where_prov_table  :: Text
    , where_prov_column :: Text
    , where_prov_key    :: a
    }

data WhereProv a key where
    NoProv    :: BasicType a => a -> WhereProv a key
    WhereProv :: BasicType a => a -> WhereProvAnnot key -> WhereProv a key
```

Important requirements for where-provenance are:

  - where-provenance representation is abstract to prevent
    programmer from creating values of that type (cf. "Language-integrated
    provenance", Section 3.1).  Achieved by not exporting constructors.
  - it is attached to a particular record field, cf. `WhereProv` data type
  - it can only be tracked for basic types, cf. `BasicType a` constraint on the
    constructors of `WhereProv`
  - it may be absent, ie. the value was not copied but created by the query,
    cf. `NoProv` constructor of `WhereProv` data type

We mark provenance tracking by using `WhereProv` like this:

```haskell
data Agency = Agency
    { a_id       :: Integer
    , a_name     :: Text
    , a_based_in :: Text
    , a_phone    :: WhereProv Text Integer
    } deriving (Show)
```

This attaches provenance information to `a_phone` field.  `WhereProv` is
detected during TH transformations.  This allows to generate additional
boilerplate to access provenance components.  Note that TH can't tell whether
`WhereProv` is a type family, type constructor or type synonym - it only sees a
type application.  TH can also look through type synonyms so it is actually
possible and convenient to set a particular type of provenance key for a given
table:

```haskell
type WhereProvI a = WhereProv a Integer

data Agency = Agency
    { a_id       :: Integer
    , a_name     :: Text
    , a_based_in :: Text
    , a_phone    :: WhereProvI Text
    } deriving (Show)
```

Other important bits of the implementation are functions to project data and
provenance components of annotated values:

```haskell
dataQ :: (QA a, QA key) => Q (WhereProv a key)-> Q (ProvData (WhereProv a key))
provQ :: (QA a, QA key, Default key) => Q (WhereProv a key)
                                     -> Q (ProvAnnot (WhereProv a key))
```

and a function that attaches bottom provenance to unannotated value:

```haskell
emptyProvQ :: forall a key. (QA a, QA key, Default key)
           => Q a
           -> Q (WhereProv a key)
```


Where-provenance implementation - boilerplate
---------------------------------------------

An example below outlines how where-provenance is implemented.  This is based on
the example of tracking where-provenance of `a_phone` field in the `Agency` data
type given above.


### `QA` and `View` instances

`QA` instances are generated without any modifications:

```haskell
instance QA Agency where
  type Rep Agency = (Rep Integer, Rep Text, Rep Text, Rep (WhereProvI Text))
```

and straightforward definitions of `toExp`/`frExp`.  The major advantage of
this approach is that provenance information is directly attached to the
data value it belongs to.  (Quote from paper, sec. 3.1: _"Provenance
information has meaning only in the context of data it belongs to."_)

`View` instance remains unchanged.


### Table selectors

Table selectors generated with `generateTableSelectors` now
take provenance into account. In the example above we have:

```haskell
a_phoneQ :: Q Agency -> Q (WhereProvI Text)
a_phoneQ (view -> (_, _, _, name)) = name

a_phone_dataQ :: Q Agency -> Q (ProvData (WhereProvI Text))
a_phone_dataQ (view -> (_, _, _, name)) = provQ name

a_phone_provQ :: Q Agency -> Q (ProvAnnot (WhereProvI Text))
a_phone_provQ (view -> (_, _, _, name)) = provQ name
```

The consequence of proposed representation is that provenance can only be
inspected (via generated table selectors) but it cannot be re-attached to a
value it does not belong to.


### Smart constructor

If at least one field of a row has where-provenance annotation then two smart
constructors are ganerated.  One accepts raw, unannotated value and attached
bottom provenance to them; another accepts provenance-annotated values.

```haskell
agency :: Q Integer -> Q Text -> Q Text -> Q Text -> Q Agency
agency (Q a_id) (Q a_name) (Q a_based_in) a_phone
   = Q (TupleConstE (Tuple4E a_id a_name a_based_in
                     (unQ ((emptyProvQ a_phone) :: Q (WhereProvI Text)))))

agencyProv :: Q Integer -> Q Text -> Q Text -> Q (WhereProvI Text) -> Q Agency
agencyProv (Q a_id) (Q a_name) (Q a_based_in) (Q a_phone)
 = Q (TupleConstE (Tuple4E a_id a_name a_based_in a_phone))
```


### Table declaration

This seems to be the crucial bit.  Table declaration, when evaluated, yields
data extracted from a database.  Recall that `table` declaration: a) provides a
list of columns of a database table; and b) has type `Q [Agency]`.  However,
with where-provenance added we're in a situation where the Haskell data type
structure does not match structure of a data base.  So what we need to do is
modify the representation of data returned by evaluating table declaration
expression.  In other words, we want to attach where-provenance on the fly.  To
do this we need to know which fields need to have where-provenance information.
This is achieved with extra provenance hints:

```haskell
agencies :: Q [Agency]
agencies = table "agencies" ( "a_id" :| [ "a_name", "a_based_in", "a_phone" ])
                 (TableHints (pure $ Key (pure "a_id") ) NonEmpty
                             (WhereProvenance $ pure "a_phone"))
```

Note the last line that contains hint about which fields have extra
where-provenance information.


A separate type to represent provenance annotations internally
--------------------------------------------------------------

Quote from the paper: _"The type system should capture the special nature of
provenance meta-data."_

With the current implementation the interface exposed to the user is explicit
about provenance annotations and the user has to explicitly project data and
provenance components.  I also briefly explored the idea of having a separate
type to represent provenance in DSH's internal type system.  The primary
motivation was the implementation of function that discards provenance
annotations at the type level:

```haskell
discardWhereProvType :: Type -> Type
discardWhereProvType (TupleT [ ScalarT t
                             , TupleT [ ScalarT BoolT
                                      , TupleT [ ScalarT StringT
                                               , ScalarT StringT
                                               , _ ]]]) = ScalarT t
discardWhereProvType (ScalarT t) = ScalarT t
discardWhereProvType (ListT   t) = ListT (discardWhereProvType t)
discardWhereProvType (TupleT  t) = TupleT $ map discardWhereProvType t
```

This function is used when performing the where-provenance transformation.  The
first pattern-match is intended and is simply horrible.  The idea was that
having a separate type that captures provenance explicitly will make things
easier.

After a quick prototype implementation this turns out not to be easy.  With the
current implementation type- and term-level encodings go hand in hand.
Introducing explicit representation of provenance at the type level breaks
things horribly, because at the term level everything is still encoded using
tuples and there seems to be no easy way to make things go hand in hand again.
It seems that to get this right I would need to be explicit about provenance in
the internal expression language as well.  This cost of extra complexity in the
internal language seems to outweight the benefits, so I've given up on the idea.


Change in internal representation of Maybe compared to original DSH
-------------------------------------------------------------------

Current implementation works by performing compile-time transformation of the
query to attach where-provenance transformation.  At the prototyping stage I
wrote transformed queries by hand to explore what happens.  A surprising result
was that two SQL queries where generated in place of original query:

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

This happened because provenance annotation was a `Maybe` (at this point of my
prototyping), which is internally represented by DSH as a list (`Just` is a
singleton list, `Nothing` is an empty list).  If we assume that the provenance
annotation is always present we get a single SQL query just as we would expect:

```SQL
SELECT a0.a_id AS o1, a1.et_id AS o2, a1.et_name AS i1,
       a0.a_phone AS i2, 'agencies' AS i3, 'a_phone' AS i4,
       a0.a_id AS i5
FROM agencies AS a0,  externaltours AS a1
WHERE (a0.a_name = a1.et_name) AND (a1.et_type = 'boat')
ORDER BY o1 ASC, o2 ASC
```

I have introduced modification to DSH that resolves the problem by encoding
`Maybe a` as a pair `(Bool, a)`, where the first component is an explicit tag
indicating whether this is encoding of `Just` or not.  This encoding forces us
to rely on `Data.Default` to generate default values in case of `Nothing`.  The
resulting SQL query is:

```SQL
SELECT a0.a_id AS o1, a1.et_id AS o2, a1.et_name AS i1,
       a0.a_phone AS i2, TRUE AS i3, 'agencies' AS i4, 'a_phone' AS i5,
       a0.a_id AS i6
FROM agencies AS a0, externaltours AS a1
WHERE (a0.a_name = a1.et_name) AND (a1.et_type = 'boat')
ORDER BY o1 ASC, o2 ASC
```

This discussion carries over to current implementation that no longer uses
`Maybe`, because `WhereProv` GADT has two constructors and we still need to
encode which of the constructor's is being used (so the `Maybe` is implicitly
a part of the current implementation).


Some notes on "Language-integrated Provenance in Links" paper
-------------------------------------------------------------

### Section 3.1 - Type system

Four requirements for a type system that tracks provenance:

  - **"provenance is attached to data and automatically propagated"**

    Done.

  - **"type system should prevent accidental modification [of provenance]"**

    This is prevented by encapsulation of provenance abstractyions.  There is
    only a view that allows to extract provenance from an annotated value.  This
    allows to write queries that inspect provenance - cf. example in the paper
    in Figure 2, where comment provenance is queried.

  - **"changes to data would invalidate where-provenance"**

    With the current implementation changing data requires projecting it
    (ie. getting the data component from an annotated value).  Once this is done
    there is no way to attach the provenance back to a modified value.  The only
    thing that can be done is adding a bottom provenance to a data value.

  - **"it is not possible for a programmer to forge provenance"**

    This seems to be the case with the current design.

In the paper provenance can only be attached to base types.  In my proposal this
is achieved by imposing `BasicType` constraint in the constructors of
`WhereProv` data type.


### Section 3.2 - Translation

There is one thing I don't understand in this section.  In general
where-provenance might be absent if a value was generated by a query and not
copied from another table.  (This problem does not arise with the example in the
paper but in general it is possible.)  How would such a situation be represented
by proposed encoding?


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

I was thinking that provenance should be handled implicitly:

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
