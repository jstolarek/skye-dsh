Lineage design ideas
====================

Lineage is attached to all rows of a table and then accumulated if result is
constructed using data from several tables.  If the result contains nested lists
then lineage is also nested - each collection in the result has a separate
lineage.

**DISCLAIMER**: Whenever code is presented in this document it may be
prettyfied, eg. by removing type-class constraints.


Lineage data types and primitives
---------------------------------

Here are the data types and primitives used when performing lineage
transformation (type-class constraints omitted for the sake of clarity):

```haskell
data LineageAnnotEntry a = LineageAnnotEntry
    { lineage_table :: Text
    , lineage_key   :: a
    }

newtype LineageAnnot a = LineageAnnot [LineageAnnotEntry a]

data Lineage a key where
    Lineage  :: TA a => a -> LineageAnnot key -> Lineage a key

-- | Lineage smart constructor
lineageQ :: Q a -> Q (LineageAnnot key) -> Q (Lineage a key)
lineageQ (Q row) (Q lineage) = Q (TupleConstE (Tuple2E row lineage))

-- | Attach empty lineage to a value
emptyLineageQ :: Q a -> Q (Lineage a key)
emptyLineageQ (Q a) = Q (TupleConstE (Tuple2E a (ListE (S.empty))))

-- | LineageAnnot smart constructor
lineageAnnotQ :: Text -> Q key -> Q (LineageAnnot key)
lineageAnnotQ table_name (Q key) =
    Q (ListE (S.singleton (TupleConstE (Tuple2E (TextE table_name) key))))

-- | Extract data from a lineage-annotated row
lineageDataQ :: Q (Lineage a key) -> Q a
lineageDataQ (view -> (a, _)) = a

-- | Extract lineage from a lineage-annotated row
lineageProvQ :: Q (Lineage a key) -> Q (LineageAnnot key)
lineageProvQ (view -> (_, lineage)) = lineage

-- | Append two lineage annotations
lineageAppendQ :: Q (LineageAnnot key) -> Q (LineageAnnot key)
               -> Q (LineageAnnot key)
lineageAppendQ (Q l1) (Q l2) = Q (AppE Append (pairE l1 l2))

-- | Extend a given table with lineage tracking
lineageTable ::  Text -> Q [a] -> (Q a -> Q key) -> Q [Lineage a key]
lineageTable name tbl key =
    [ lineageQ a (lineageAnnotQ name (key a)) | a <- tbl ]
```

These primitives are here primarily to demonstrate the concepts behind lineage
transformation.  They are not actually used to perform the transformation
internally inside the library.


Lineage transformation by hand
------------------------------

Below are two example queries and their transformed versions with lineage
tracking.  Both examples assume that the table `agenciesL` is a lineage-extended
version of `agencies`:

```haskell
agenciesL :: Q [Lineage Agency Integer]
agenciesL = [ lineageQ a (lineageAnnotQ "agencies" (a_idQ a)) | a <- agencies ]
```

### Example 1

```haskell
q1 :: Q [Text]
q1 = [ a_nameQ a
     | a <- agencies
     ]

q1' :: Q [Lineage Text Integer]
q1' = [ lineageQ (lineageDataQ z_a)
                 (lineageProvQ al `lineageAppendQ` lineageProvQ z_a)
      | al  <- agenciesL
      , a   <- [ lineageDataQ al ]
      , z_a <- [ emptyLineageQ (a_nameQ a) :: Q (Lineage Text Integer) ]
      ]
```

Desugaring of `q1`:

```haskell
concatMap (\a -> [a_nameQ a]) agencies
```

Desugaring of `q1'`:

```haskell
concatMap (\al ->
  concatMap (\a ->
    concatMap (\z_a ->
               [ lineageQ (lineageDataQ z_a)
                          (lineageProvQ al `lineageAppendQ` lineageProvQ z_a) ])
               [emptyLineageQ (a_nameQ a)])
  [lineageDataQ al])
(concatMap (\a -> [(a, [(name, rowKey a)])]) agencies) -- agenciesL
```

### Example 2

```haskell
q2 :: Q [(Text, Text)]
q2 = [ tup2 (et_nameQ et) (a_phoneQ a)
     | a  <- agencies
     , et <- externalTours
     , a_nameQ a  == et_nameQ et
     , et_typeQ et == "boat"
     ]

q2' :: Q [Lineage (Text, Text) Integer]
q2' = [ lineageQ (lineageDataQ z_a)
                 (lineageProvQ al `lineageAppendQ` lineageProvQ z_a)
      | al  <- agenciesL
      , a   <- [ lineageDataQ al ]
      , z_a <- [ lineageQ (lineageDataQ z_et)
                          (lineageProvQ etl `lineageAppendQ` lineageProvQ z_et)
               | etl <- externalTours
               , et <- [ lineageDataQ etl ]
               , z_et <- [ emptyLineageQ (tup2 (et_nameQ et) (a_phoneQ a)) ::
                               Q (Lineage (Text, Text) Integer)
                         | a_nameQ a  == et_nameQ et
                         , et_typeQ et == "boat" ]
               ]
      ]
```

Desugaring of `q2`:

```haskell
concatMap (\a ->
  concatMap (\et ->
    if (a_nameQ a  == et_nameQ et)
    then (if et_typeQ et == "boat"
          then [tup2 (et_nameQ et) (a_phoneQ a)]
          else [])
    else [])
  externalTours)
agencies
```

Desugaring of `q2'`, step 1:

```haskell
concatMap (\al ->
  concatMap (\a ->
    concatMap (\z_a ->
             [ lineageQ (lineageDataQ z_a)
                        (lineageProvQ al `lineageAppendQ` lineageProvQ z_a) ])
         ( NESTED COMPREHENSION ))
  [ lineageDataQ al ])
agenciesL
```

Desugaring of `q2'`, step 2:

```haskell
concatMap (\al ->
  concatMap (\a ->
    concatMap (\z_a ->  -- (1)
              [ lineageQ (lineageDataQ z_a)
                         (lineageProvQ al `lineageAppendQ` lineageProvQ z_a) ])
         (concatMap (\etl ->  -- second argument to (1)
            concatMap (\et -> concatMap (\z_et ->
                        [ lineageQ (lineageDataQ z_et)
                          (lineageProvQ etl `lineageAppendQ` lineageProvQ z_et)
                        ] )
                     ( NESTED COMPREHENSION ))
            [ lineageDataQ etl ])
         externalToursL))
  [ lineageDataQ al ])
agenciesL
```

Desugaring of `q2'`, final step:

```haskell
concatMap (\al ->
  concatMap (\a ->
    concatMap (\z_a ->  -- (1)
              [ lineageQ (lineageDataQ z_a)
                         (lineageProvQ al `lineageAppendQ` lineageProvQ z_a) ])
         (concatMap (\etl ->  -- second argument to (1)
            concatMap (\et -> concatMap (\z_et ->
                        [ lineageQ (lineageDataQ z_et)
                          (lineageProvQ etl `lineageAppendQ` lineageProvQ z_et)
                        ] )
                    (if (a_nameQ a  == et_nameQ et)
                     then (if et_typeQ et == "boat"
                           then [tup2 (et_nameQ et) (a_phoneQ a)]
                           else [])
                     else []))
            [lineageDataQ etl])
         externalToursL))
  [lineageDataQ al])
agenciesL
```

Below are types of variables and subexpressions in the transformed code above.
`r` type variable stands for result type (`(Text, Text)` in this example).
`arg` and `res` are argument and result type indices for `ConcatMap` data
constructor used by DSH.

```
al           :: Lineage Agency
agenciesL    :: [ Lineage Agency ]
(\al -> ...) :: Lineage Agency -> [ Lineage r ]
arg          :: ( Lineage Agency -> [ Lineage r ], [ Lineage Agency ] )
res          :: [ Lineage r ]

a                 :: Agency
[lineageDataQ al] :: [Agency]
(\a -> ...)       :: Agency -> [ Lineage r ]
arg               :: (Agency -> [ Lineage r ], [Agency])
res               :: [ Lineage r ]

2z_a          :: Lineage r
(z_a -> ...) :: Lineage r -> [ Lineage r ]
arg          :: ( Lineage r -> [ Lineage r ], [ Lineage r ])
res          :: [ Lineage r ]
```

Failed attempt: relying on lineage hints
----------------------------------------

My initial idea was to add `LineageHint` hint to table declaration:

```haskell
agencies :: Q [Lineage Agency Integer]
agencies = table "agencies"
                 ( "a_id" :|
                 [ "a_name"
                 , "a_based_in"
                 , "a_phone"
                 ])
                 (TableHints (pure $ Key (pure "a_id") ) NonEmpty
                             LineageHint)
```

`LineageHint` would inform DSH that we track lineage for this table and every
query using this table needs needs to be transformed to add lineage information.
(Note that lineage information is be reflected in the type of table
declaration.) The transformation would be performed during translation from
frontend language to CL language.

Going back to an earlier example, we would have to modify the type of `q1` to
also include `Lineage`:

```haskell
q1 :: Q [Lineage Text Integer]
q1 = [ a_nameQ a
     | a <- agencies
     ]
```

The above query gets desugared into `concatMap (\a -> [a_nameQ a]) agencies`.
The idea was for DSH to realize that `agencies` table has `LineageHint`
annotation and rewrite the query.

The reason this does not work is that desugared expression does not type check.
The problem is that `a` has type `Lineage Agency Integer`, whereas `a_nameQ`
expects `Agency` as it's input.  One could consider projections that work under
`Lineage`, eg.

```haskell
a_nameQ :: Lineage Agency Integer -> Lineage Text Integer
```

the idea being that projecting a column retains the lineage information.  This
approach quickly breaks however.  Looking at the body of second example query
`q2` we have `tup2 (et_nameQ et) (a_phoneQ a)`.  `tup2` constructor would
somehow have to extract lineage from its arguments and combine it.  This means
that `tup2` would have to work both for arguments with and without lineage
annotation.

**Take-away message**: desugared expression must type-check since it is embedded
in Haskell's type system.  It is not possible to rewrite it later to a form that
would type check.


Ongoing work: Frontend-to-Frontend transformation
-------------------------------------------------

Another idea is to write a user-space function that will take a well-typed query
and transform it into a query with lineage.  Usage would look like this:

```haskell
q1' :: Q [Lineage Text Integer]
q1' = lineage [ a_nameQ a | a <- agencies ]
```

Type of `lineage` would be:

```haskell
lineage :: Q [a] -> Q [Lineage a key]
```

Recall that `Q a` is a newtype corresponding to `Exp (Rep a)`.  (More on `Rep`
in a moment.)  This means that `lineage` would have to work by directly
constructing a syntax tree of `Exp`.  This approach seems sound but I have
encountered a lot of problems with implementing it.  Below are some of them:

1. Rewriting a table expression (`TableE`) requires performing a transformation
   that is equivalent to:

   ```haskell
   lineageTable :: Text -> Q [a] -> (Q a -> Q key) -> Q [Lineage a key]
   lineageTable name tbl key =
       [ lineageQ a (lineageAnnotQ name (key a)) | a <- tbl ]
   ```

   The difficult part of the transformation is projecting the key column.
   `TableE` expression stores information about the number of columns and a key
   column (I'm ignoring compound keys for the sake of simplicity).  This allows
   to easily figure out what the projection should be. For `agencies` table the
   expression projecting the key column would be `AppE (TupElem Tup4_1) a` where
   `Tup4_1` represents a function projecting first component of a 4-tuple and
   `a` is a variable that represents a table row.  This however does not
   type-check.  The reason is that `Exp` is a GADT with a type index enforcing
   correct typing of expressions.  We have the following types:

   ```haskell
   TupElem Tup4_1 :: Fun (a,b,c,d) a
   AppE :: Fun a b -> Exp a -> Exp b
   ```

   `AppE (TupElem Tup4_1)` has type `Exp (a,b,c,d) -> Exp a` and so `a` would
   have to have a type `(a, b, c, d)` for everything to type check.
   Unfortunately, the type of `TableE` expression is `Exp [a]` - it is a list
   of things with an unknown internal structure.  In order for this code to
   typecheck we would have to make a connection between the number of columns,
   which is a run-time information, and the arity of tuples representing a
   table row, which is compile-time information.


2. An important observation related to the above problem is that we can easily
   construct column projections in the source language but not in the desugared
   frontend language.  The reason why everything works in the surface language
   is the existence of `View` type class and its interaction with `Rep`
   associated type family.  Here are the definitions of `View` and `Rep`:

   ```haskell
   class View a where
      type ToView a
      view :: a -> ToView a

   class QA a where
      type Rep a
   ```

   Instances for `Agency` look like this:

   ```haskell
   instance QA Agency where
      type Rep Agency = (Integer, Text, Text, Text)

   instance View (Q Agency) where
     type ToView (Q Agency) = (Q Integer, Q Text, Q Text, Q Text)
     view (Q agency)
       = ((Q $ (AppE (TupElem Tup4_1) agency)),
          (Q $ (AppE (TupElem Tup4_2) agency)),
          (Q $ (AppE (TupElem Tup4_3) agency)),
          (Q $ (AppE (TupElem Tup4_4) agency)))

   a_nameQ :: Q Agency -> Q Text
   a_nameQ (view -> (_, name, _, _)) = name
   ```

   Note that `Q Agency` really means `Exp (Rep Agency)` - an expression that
   represents a 4-tuple.  This connection makes everything work in the surface
   language.

   Based on this observation, I decided to solve the problem outlined in (1) by
   introducing a type class that will contain a function for projecting the key
   column.  A speculative definition might look like this:

   ```haskell
   class RowKey a where
      rowKey :: Q a -> Q Integer
   ```

   (**IMPORTANT**: Note that previously I have assumed polymorphic keys in the
   representation of `Lineage`.  At this point I am abandoning the idea and
   specializing keys to be of `Integer` type.  The reason is that key
   polymorphism brings in more problems and I want to solve more important
   problems first.)


3. Another problem we now face is: should our function operate on `Q` or on
   `Exp`?  All the surface functions operate on `Q`.  So does `rowKey` function
   of `RowKey` type class, the intention being that `Agency` data type would be
   an instance of `RowKey`.

   Recall that `lineage` function has the type `Q [a] -> Q [Lineage a Integer]`
   - it operates on `Q`.  But `Q` is only a newtype.  The actual underlying data
   type is `Exp` and so `lineage` has to traverse a syntax tree of type `Exp`.
   Importantly, subtrees in that syntax tree are also of type `Exp` so `lineage`
   needs a worker that works on `Exp`.  But what should the return type of that
   worker be?  List expressions should be extended with lineage, but other
   expressions (eg. constants) should remain unchanged.  Therefore, I propose:

   ```haskell
   type family LineageRep a where
       LineageRep [a] = [(a, [(Text, Integer)])] -- representation of lineage
       LineageRep a   = a

   lineageTransform :: Exp a -> Exp (LineageRep a)
   ```

   Using a type family allows to have a single function for processing all
   expressions.  An important aside is that with this approach it would not be
   possible to work on `Q`.  Assume we have:

   ```haskell
   lineageTransform :: Q a -> Q (LineageRep a)
   lineageTransform (Q UnitE) = Q UnitE
   ```

   On the LHS we have `UnitE :: Exp ()`, and since the input argument is really
   of type `Exp (Rep a)` we learn that `Rep a ~ ()`.  By similar reasoning on
   the RHS we have to prove that `(Rep (LineageRep a) ~ ())`.  But this cannot
   be deduced from the LHS since `Rep` is not injective.  So the code working on
   `Q` would not type check.  Or at least it would require some more type-level
   hackery to make this work.

   So having `rowKey :: Q a -> Q Integer` is not very useful if we need to work
   on `Exp`.  Another attempt would be:

   ```haskell
   class RowKey a where
      rowKey :: Exp a -> Exp Integer
   ```

   This works well inside `lineageTransform`, but now the problem is instances
   would have to be written on representations of surface data types, eg. we
   would have to write `instance RowKey (Integer, Text, Text, Text) where...`
   instead of `instance RowKey Agency where...`.  This is not acceptable - two
   data types might have identical internal representations but different ways
   of projecting a key.  So another attempt would be:

   ```haskell
   class QA a => RowKey a where
      rowKey :: Exp (Rep a) -> Exp Integer
   ```

   The intention here being that instances are written on surface data types but
   the function itself works on internal representations of surface data types.
   This is what we really want, but now we have a different problem: type of
   `rowKey` is ambiguous because type variable `a` appears only under
   (non-injective) type family.  Solution to this problem is closely related to
   problem (4).


4. Transforming `TableE` with `lineageTransform` now relies on `rowKey` function,
   which means data type that represents a table row must be an instance of
   `RowKey`.  This means we need a type class constraint.  But it only makes
   sense to have that constraint when dealing with `TableE`.  The solution is to
   add a type class constraint to `TableE` constructor:

   ```haskell
   TableE :: (RowKey a) => Table -> Exp (Rep [a])
   ```

   The intention here is that `a` stands for a surface data type.  We require
   that this data type is an instance of `RowKey` and index the `Exp`
   constructor with `Rep [a]` - a representation of list containing surface data
   type `a`.  But here we also face the same problem as with the updated
   definition of `rowKey`: type variable `a` is ambiguous because it appears
   only under a type family and inside a context.  The solution in both cases is
   to introduce a type proxy:

   ```haskell
   TableE :: (RowKey a) => Proxy a -> Table -> Exp (Rep [a])

   class QA a => RowKey a where
      rowKey :: Proxy a -> Exp (Rep a) -> Exp Integer
   ```

   Proxies can now be used to guide type inference in `lineageTransform`

   ```haskell
   lineageTransform t@(TableE proxyA (TableDB name _ _)) = do
     let lam :: forall b. (QA b, RowKey b)
             => Proxy b -> Integer -> Exp [(Rep b, [(Text, Integer)])]
         lam proxyB a = ListE (S.singleton (TupleConstE (Tuple2E (VarE a)
                         (ListE (S.singleton (TupleConstE (Tuple2E
                           (TextE (pack name)) (rowKey proxyB (VarE a)))))))))
     return (AppE ConcatMap (TupleConstE (Tuple2E (LamE (lam proxyA)) t)))
   ```


5. Implementation of lineage transformation that was outlined earlier looks like
   this:

   ```haskell
   lineageTransform (AppE ConcatMap (TupleConstE (Tuple2E (LamE lam) tbl))) = do
     -- translate the comprehension generator
     tbl' <- lineageTransform tbl
     -- saturate the lambda and translate the resulting expression
     boundVar <- freshVar
     bodyExp  <- lineageTransform (lam boundVar)

     let lam_z al z = singletonE (lineageE (lineageDataE (VarE z))
                ((lineageProvE (VarE al)) `appendE` (lineageProvE (VarE z))))
         innerComp al  = AppE ConcatMap (TupleConstE
                (Tuple2E (LamE (lam_z al)) bodyExp))
         innerComp2 al = AppE ConcatMap (TupleConstE
                (Tuple2E (LamE (\a -> subst a boundVar (innerComp al)))
                               (singletonE (lineageDataE (VarE al)))))
     return (AppE ConcatMap (TupleConstE (Tuple2E (LamE innerComp2) tbl')))
   ```

   This code performs transformation corresponding to desugaring step 1 in `q2'`
   example.  `lineageTransform tbl` is equivalent of translating `agenciesL` and
   `lineageTransform (lam boundVar)` corresponds to `NESTED COMPREHENSION` part
   in the mentioned example.

   The above code however does not compile.  The problem arises from creating
   applications with `AppE`.  Constructor `AppE` is defined as:

   ```haskell
   AppE :: (Reify a, Reify b) => Fun a b -> Exp a -> Exp b
   ```

   Both indices `a` and `b` must be an instance of `Reify`: it must be possible
   to assign internal DSH type to both the argument expression and the result of
   application.  In order to verify this requirement the exact types `a` and `b`
   must be known, but that is not the case in the above code for lineage
   transformation.  Importantly, new variables are created (calls to `VarE` in
   `lam_z` and `innerComp2`) but are not assigned any particular types.

   Observation: created expressions will always be an instance of `Reify`.  One
   can only write queries involving surface types that are instances of `QA`
   class and that enforces a corresponding instance of `Reify`.

   One idea to solve this problem was to change how `Reify` works: instead of
   reifying type indices to `Exp` we could base reification on the structure of
   expressions.  Alas, this would not work, because there are expressions with
   internal structure nit corresponding to type.  Most importantly `TableE`
   expression does not allow to reify the type of expression that the table
   represents.

   Another idea is to guide type inference in application using proxies:

   ```haskell
   AppE :: (Reify a) => Proxy a -> Fun a b -> Exp a -> Exp b
   ```

   This makes explicit applications slightly verbose to implement, but proxies
   would not leak to the user space, which makes this an acceptable solution.
   Proxies would need to be created during desugaring from source Haskell into
   Frontend language - see functions in `Database.DSH.Frontend.Externals`.
   Hopefully, they could be then used to guide type inference during lineage
   transformation.

   This is a work in progress.


TODO
----

  * rename proxy functions

  * better naming of everything in lineageTransform

  * move helpers to a different module so that functions working on Q reuse
    helpers

  * search for all JSTOLAREK tags

  * polymorphic keys

  * generate substTuple with TH

  * generate RowKey instances with TH

  * update description above

  * implement all example queries

  * think of interface that should be exposed to the user

  * try to break the implementation


Questions
---------

  1. Do all collections in a query need to have Lineage attached?  What happens
     if one table in a query is annotated with provenance but another one is
     not?  I think supporting this would be a useful feature.


Case studies
------------

Lineage transformation of a hard-coded list:

```
L[| for (x <- [1,2]) [x]) |]

for (y <- L[| [1,2] |])
    for (z <- L[| [x] |] [x -> y.data])
       [( data = z.data, prov = y.prov ++ z.prov )]

for (y <- L[| [1,2] |])
    for (z <- L[| [x] |] [x -> y.data])
       [( data = z.data, prov = y.prov ++ z.prov )]

for (y <- [(data: 1, prov: []), (data: 2, prov: [])])
    for (z <- [(data: y.data, prov: [])])
       [( data = z.data, prov = y.prov ++ z.prov )]
```

Example from "Lineage-integrated Provenance" paper.  Interestingly, the result
is different than the one obtained in the paper.  I believe that both results
(ie. mine and the one in the paper) are semantically equivalent, but
syntactically they are different.  Either I have misunderstood rules of the
transformation or the paper presents a prettyfied result.

```
-- original query
L[| for ( a <- agencies )
      for (e <- externalTours)
      where (a.name == e.name && e.type == "boat")
        [( name = e.name, phone = a.phone )]
 |]

-- transform outer for comprehension
for ( y_a <- L[| agencies |] )
  for ( z_a <- L[| for (e <- externalTours)
                   where (a.name == e.name && e.type == "boat")
                     [( name = e.name, phone = a.phone )] |] [ a -> y_a.data ] )
    [( data = z_a.data, prov = y_a.prov ++ z_a.prov )]

-- perform [ a -> y_a.data ] substitution
for ( y_a <- L[| agencies |] )
  for ( z_a <- L[| for (e <- externalTours)
                   where (y_a.data.name == e.name && e.type == "boat")
                     [( name = e.name, phone = y_a.data.phone )] |] )
    [( data = z_a.data, prov = y_a.prov ++ z_a.prov )]

-- transform inner for comprehension
for ( y_a <- L[| agencies |] )
  for ( z_a <- for (y_e <- L[| externalTours |] )
                 for (z_e <- L[| where (y_a.data.name == e.name && e.type == "boat")
                                 [( name = e.name, phone = y_a.data.phone )] |]
                                 [ e -> y_e.data ] )
                     [( data = z_e.data, prov = y_e.prov ++ z_e.prov )]
                    )
    [( data = z_a.data, prov = y_a.prov ++ z_a.prov )]

-- perform [ e -> y_e.data ] substitution
for ( y_a <- L[| agencies |] )
  for ( z_a <-
         for (y_e <- L[| externalTours |] )
           for (z_e <- L[| where (y_a.data.name == y_e.data.name &&
                                  y_e.data.type == "boat")
                           [( name = y_e.data.name, phone = y_a.data.phone )] |] )
                     [( data = z_e.data, prov = y_e.prov ++ z_e.prov )]
                    )
    [( data = z_a.data, prov = y_a.prov ++ z_a.prov )]

-- transform where
for ( y_a <- L[| agencies |] )
  for ( z_a <- for (y_e <- L[| externalTours |] )
           for (z_e <- where
            L[| (y_a.data.name == y_e.data.name && y_e.data.type == "boat") |]
            L[| [( name = y_e.data.name, phone = y_a.data.phone )] |] )
                     [( data = z_e.data, prov = y_e.prov ++ z_e.prov )] )
    [( data = z_a.data, prov = y_a.prov ++ z_a.prov )]


for ( y_a <- L[| agencies |] )
  for ( z_a <- for (y_e <- L[| externalTours |] )
           for (z_e <- where (y_a.data.name == y_e.data.name &&
                               y_e.data.type == "boat")
             [ ( data : ( name = y_e.data.name, phone = y_a.data.phone )
               , prov : [] ) ] )
                     [( data = z_e.data, prov = y_e.prov ++ z_e.prov )] )
    [( data = z_a.data, prov = y_a.prov ++ z_a.prov )]
```
