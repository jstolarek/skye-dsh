Lineage design ideas
====================

Lineage is attached to all rows of a table.  Idea: add `Lineage` hint to table
declaration:

```haskell
agencies :: Q [Lineage Agency]
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