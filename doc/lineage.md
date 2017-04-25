Lineage design ideas
====================

Lineage is attached to all rows of a table and then accumulated if result is
constructed using data from several tables.


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

These primitives show are here primarily to demonstrate the concepts behind
lineage transformation.  They are not actually used to perform the
transformation for reasons explained later.


Lineage transformation by example
---------------------------------

Below are two example queries and their transformed versions with lineage
tracking.

```haskell
q1 :: Q [Text]
q1 = [ a_nameQ a
     | a <- agencies
     ]

q1' :: Q [Lineage Text Integer]
q1' = [ lineageQ (lineageDataQ z_a)
                 (lineageProvQ al `lineageAppendQ` lineageProvQ z_a)
      | al <- lineage agencies
      , let a = lineageDataQ al
      , z_a <- [ emptyLineageQ (a_nameQ a) :: Q (Lineage Text Integer)
               | true
               ]
      ]
```

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
      | al <- agencies
      , let a = lineageDataQ al
      , z_a <- [ lineageQ (lineageDataQ z_et)
                          (lineageProvQ etl `lineageAppendQ` lineageProvQ z_et)
               | etl <- externalTours
               , let et = lineageDataQ etl
               , z_et <- [ emptyLineageQ (tup2 (et_nameQ et) (a_phoneQ a)) ::
                               Q (Lineage (Text, Text) Integer)
                         | a_nameQ a  == et_nameQ et
                         , et_typeQ et == "boat"
                         , true ]
               ]
      ]
```


Failed attempt #1: relying on lineage hints
-------------------------------------------

My inital idea was to add `LineageHint` hint to table declaration:

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

The reason this does not work is that desugared expression does not type check.
Going back to an earlier example, we would have to modify its type:

```haskell
q1 :: Q [Lineage Text Integer]
q1 = [ a_nameQ a
     | a <- agencies
     ]
```

CONTINUE HERE

Failed ideas and dead ends
--------------------------

Not possible to perform transformation when translating from frontend to CL.
Reason: desugared syntax tree must be a well-typed Haskell expression after
desugaring.

Type class RowKey required to construct well-typed key column projections.

Proxies required to guide type inference.


Questions
---------

  1. Do all collections in a query need to have Lineage attached?  What happens
     if one table is annotated with provenance but the other is not?  I think
     this should be supported and is a useful feature.


Case studies
------------

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
           for (z_e <- L[| where (y_a.data.name == y_e.data.name && y_e.data.type == "boat")
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
           for (z_e <- where (y_a.data.name == y_e.data.name && y_e.data.type == "boat")
             [data : ( name = y_e.data.name, phone = y_a.data.phone ), prov : []] )
                     [( data = z_e.data, prov = y_e.prov ++ z_e.prov )] )
    [( data = z_a.data, prov = y_a.prov ++ z_a.prov )]
```


-----------------------------------

-- perform [ a -> y_a.data ] substitution
for ( y_a <- L[| agencies |] )
  for ( z_a <- for (e <- externalTours)
                   where (y_a.data.name == e.name && e.type == "boat")
                     [ data : ( name = y_e.data.name, phone = y_a.data.phone )
                     , prov : []] )
    [( data = z_a.data, prov = y_a.prov ++ z_a.prov )]
