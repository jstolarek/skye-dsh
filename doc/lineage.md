Lineage design ideas
====================

Lineage is attached to all rows of a table.  Idea: add `Lineage` hint to table
declaration:

```haskell
agencies :: Q [Lineage Agency Integer]
agencies = table "agencies"
                 ( "a_id" :|
                 [ "a_name"
                 , "a_based_in"
                 , "a_phone"
                 ])
                 (TableHints (pure $ Key (pure "a_id") ) NonEmpty
                             Lineage)
```

Note: lineage tracking reflected in type.

Presence of `Lineage` hint should trigger rewriting of query inside DSH in the
same way `WhereProvenance` hint works.  To make this work I need to change the
representation of `TableHints`:

```haskell
data Provenance = Lineage
                | WhereProvenance (NonEmpty String)
                deriving (Eq, Ord, Show)

data TableHints = TableHints
    { keysHint       :: NonEmpty Key
    , nonEmptyHint   :: Emptiness
    , provenanceHint :: [Provenance]
    } deriving (Eq, Ord, Show)
```

It would be good to validate that `provenanceHint` does not contain duplicates.

Create a TH function to generate boilerplate needed to work with lineage:

```haskell
deriveLineage ''Agency
```

Questions
---------

  1. Do all collections in a query need to have Lineage attached?  What happens
     if one table is annotated with provenance but the other is not?  I think
     this should be supported and is a useful feature.


Case studies
------------

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



-----------------------------------

-- perform [ a -> y_a.data ] substitution
for ( y_a <- L[| agencies |] )
  for ( z_a <- for (e <- externalTours)
                   where (y_a.data.name == e.name && e.type == "boat")
                     [ data : ( name = y_e.data.name, phone = y_a.data.phone )
                     , prov : []] )
    [( data = z_a.data, prov = y_a.prov ++ z_a.prov )]
