Understanding DSH SQL backend
=============================

Below are notes on my attempts to understand how DSH's PostgreSQL backend works.
The backend is implemented by the `dsh-sql` library located in the `modules`
directory.  This document is in a very early stage and lacks any sensible
structure.

DSH has two backend languages: Segment Language (SL) and Virtual Segment
Language (VSL), located in `Database.DSH.SL` and `Database.DSH.VSL`.  SL
provides something that is called _"segment algebra"_ inside
`Database.DSH.SL.SegmentAlgebra` module:

```haskell
class SegmentAlgebra a where
    type SLDVec a -- Data Vector
--  (...)

    -- | A reference to a database-resident table.
    vecTableRef :: String -> L.BaseTableSchema -> Build a (SLDVec a)
--  (...)
```

This looks like a key bit to implementing adding where-provenance.
`vecTableRef` looks like the thing that takes the database table schema and
turns it into actual data.

Library `algebra-dag` defines an abstract notion of algebra in
`Database.Algebra.Dag.Common`:

```haskell
data Algebra t b u n c = TerOp t c c c
                       | BinOp b c c
                       | UnOp u c
                       | NullaryOp n
                         deriving (Ord, Eq, Show, Read, Generic)
```

`algebra-sql` library defines a concrete _table algebra_
(`Database.Algebra.Table.Lang`), which represents _"table algebra operators over
multiset relations"_:

```haskell
type TableAlgebra = Algebra () BinOp UnOp NullOp AlgNode
```

This looks like a language only much closer to SQL, e.g.:

```haskell
data UnOp = --- (...)
          | Project [(Attr, Expr)]
          | Select Expr
          | Distinct ()
          | Aggr ([(AggrType, ResAttr)], [(PartAttr, Expr)])
          -- (...)
          deriving (Ord, Eq, Show)

data NullOp = LitTable ([Tuple], [TypedAttr])
            | TableRef (TableName, [TypedAttr], [Key])
            deriving (Ord, Eq, Show)
```

`TableAlgebra` is made into an instance of `SegmentAlgebra`


These are located in `Database.DSH.Backend` inside DSH (not sure if this is
relevant):

```haskell
type BackendCodeGen v b = QueryPlan v DVec -> Shape b

class (RelationalVector v, Row (BackendRow v)) => BackendVector v where
    data BackendConn v
    data BackendRow v
--  (...)

class Row r where
    -- | The type of single attribute values
    data Scalar r
    col :: String -> r -> (Scalar r)
--  (..)
```
