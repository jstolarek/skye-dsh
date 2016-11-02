Understanding DSH
=================

Below are notes on my attempts to understand how DSH works.  My analysis is
based on a case study of an example query from "Language-integrated Provenance"
paper and reading DSH source code.


DSH internals
-------------

### Encoding expressions

DSH uses GADTs to define its internal expression language.  Its AST is
represented by `Exp` data type in `Database.DSH.Frontend.Internals` module.
Leaves of the `Exp` tree store concrete values, e.g.:

```haskell
data Exp a where
    UnitE       :: Exp ()
    BoolE       :: !Bool    -> Exp Bool
--  (...)
    AppE        :: (Reify a, Reify b)  => Fun a b -> Exp a -> Exp b
    TupleConstE :: !(TupleConst a) -> Exp a
```

Applications are represented by `AppE` using `Fun` GADT from
`Database.DSH.Frontend.Builtins`.  It encodes all allowed functions together
with their types, e.g:

```haskell
data Fun a b where
    Not              :: Fun Bool Bool
    Concat           :: Fun [[a]] [a]
    Null             :: Fun [a] Bool
    TupElem          :: TupElem a b -> Fun a b
```


#### Encoding tuples

The last constructor, `TupElem`, encodes a tuple accessor.  `TupElem` is a GADT
encoding all the tuple accessors (generated by `mkTupElemType` in
`Database.DSH.Frontend.TupleTypes`):

```haskell
data TupElem a b where
    Tup2_1 :: TupElem (a, b) a
    Tup2_2 :: TupElem (a, b) b
    Tup3_1 :: TupElem (a, b, c) a
    Tup3_2 :: TupElem (a, b, c) b
    Tup3_3 :: TupElem (a, b, c) c
```

`TupleConstE` constructor of `Exp` data type represents tuples using
`TupleConst` (generated by `mkTupleAstComponents` in
`Database.DSH.Frontend.TupleTypes`):

```haskell
data TupleConst a where
    Tuple<n>E :: (Reify t1, ..., Reify t<n>) => Exp t1
                                             -> ...
                                             -> Exp t<n>
                                             -> TupleConst (t1, ..., t<n>)
```

There are also other helper functions, eg. `tup2` generated by
`mkTupleConstructors` (`tup2` is used later in the case study):

```haskell
tups :: (QA t1, QA t2) => Q t1 -> Q t2 -> Q (t1, t2)
tup2 (Q v1) (Q v2)= Q (TupleConstE (Tuple2E v1 vn))
```


### Encoding types

There's also `Type` GADT that represents types of expressions encoded with
`Exp` values:

```haskell
data Type a where
    UnitT       :: Type ()
    BoolT       :: Type Bool
--  (...)
    ArrowT      :: (Reify a,Reify b)  => Type a -> Type b -> Type (a -> b)
    TupleT      :: TupleType a -> Type a
```

`TupleType` is analogous to `TupleConst`, except that constructors store `Type`
values rather than `Exp` values.


### Type classes & friends

DSH also defines several type classes in `Database.DSH.Frontend.TupleTypes`


#### Reify

```haskell
class Reify a where
    reify :: a -> Type a
```

Allows to convert expressions into type of their embedding.  For example:

```haskell
instance Reify Bool where
    reify _ = BoolT
```

Which means that `reify true` will return `BoolT`.


#### QA

```haskell
class (Reify (Rep a)) => QA a where
    type Rep a
    toExp :: a -> Exp (Rep a)
    frExp :: Exp (Rep a) -> a
```

`QA` represents types that can be represented using DSH's expression language
(`Exp` data type).  `Rep a` represents a (Haskell, non-embedded) type used to
encode the internal representation of a data type.  For most basic types this is
an identity function (`Bool` is represented by a `Bool`, `()` by a `()` and so
on), but there are some exceptions, eg. `type Rep (Maybe a) = [Rep a]`.
`toExp`/`fromExp` convert Haskell expressions to DSH's expression
representation.  Note the `Reify (Rep a)` constraint on `QA` ensures that
representation type must be reifiable.


#### TA

```haskell
class TA a where
```

Unknown purpose.  This class is empty.  `Database.DSH.Frontend.Externals`
defines instances of `TA` for `()`, `Bool`, `Char`, `Integer`, `Double`, `Text`,
`Decimal`, `Scientific` and `Day`.  Instances of `BasicType` exist for same
types.  `TA` constraint exists mostly on various comparison and ordering
functions, but these also have `Eq` and `Ord` constraints as well.


#### Q newtype

```haskell
newtype Q a = Q (Exp (Rep a))
```

Just a wrapper around `Exp`.  Ensures that internal representation uses `Rep`
type.


#### View

```haskell
class View a where
    type ToView a
    view :: a -> ToView a
```

Ensures a view exists for a given type.  This is later used with view patterns
generated by `generateTableSelectors`.  Instances are generated for types
wrapped in `Q`.  It seems that structure of `ToView a` is identical to `Rep a`,
except that recursion happens in `Q` rather than in `Rep`.


### Table declarations

DSH's expression language contains representation for database tables:

```haskell
data Exp a where
--  (...)
    TableE      :: (Reify a)           => Table -> Exp [a]
--  (...)

data Table = TableDB String (NonEmpty ColName) TableHints

data TableHints = TableHints
    { keysHint     :: NonEmpty Key
    , nonEmptyHint :: Emptiness
    } deriving (Eq, Ord, Show)
```

`nonEmptyHint` hints whether a table might be empty or is known not to be empty.
`keysHint` gives "a combination of column names that form a candidate key",
where `Key` is a non-empty list of strings:

```haskell
newtype Key = Key (NonEmpty String)
```

`table` function from `Database.DSH.Frontend.Externals` is essentialy a smart
constructor for table expressions:

```haskell
table :: (QA a, TA a) => String -> NonEmpty ColName -> TableHints -> Q [a]
table name schema hints = Q (TableE (TableDB name schema hints))
```

Interesting thing about it is that return type does not depend in any way on the
arguments.  In other words, type siganture or table declarations *seems*
mandatory to define what kind of data is actualy stored in a table.


### Query desugaring

GHC desugars a comprehension into calls to `>>=` and `return`.  DSH uses
`RebindableSyntax` to ensure that these desugarings refer to `>>=`, `return` and
other functions defined in `Database.DSH.Frontend.Externals` module.  That
module also redefines many functions from standard Prelude to work on DSH
expressions wrapped in `Q`.  For example:

```haskell
return :: (QA a) => Q a -> Q [a]
return = singleton

(>>=) :: (QA a,QA b) => Q [a] -> (Q a -> Q [b]) -> Q [b]
(>>=) ma f = concatMap f ma

concatMap :: (QA a,QA b) => (Q a -> Q [b]) -> Q [a] -> Q [b]
concatMap f (Q as) = Q (AppE ConcatMap (pairE (LamE (toLam f)) as))
```

So the result of desugaring is a series of calls to functions that construct
DSH's `Exp` values.


### Comprehension language (CL)

Comprehension langauge is the next intermediate representation used by DSH in
its pipeline.  CL is explicitly typed.  Translation from expression language to
comprehension language is performed by `translate` function in
`Database.DSH.Translate.Frontend2CL` module.

#### Expressions

```haskell
data Expr  = Table   Type String BaseTableSchema
           | AppE1   Type Prim1 Expr
           | AppE2   Type Prim2 Expr Expr
           | BinOp   Type ScalarBinOp Expr Expr
           | UnOp    Type ScalarUnOp Expr
           | If      Type Expr Expr Expr
           | Lit     Type Val
           | Var     Type Ident
           | Comp    Type Expr (NL Qual)
           | MkTuple Type [Expr]
           | Let     Type Ident Expr Expr

data Prim1 = Singleton | Only   | Concat | Null  | Reverse
           | Nub       | Number | Sort   | Group | Guard
           | TupElem TupleIndex | Agg AggrFun

data Prim2 = Append | Zip | CartProduct
           | ThetaJoin (JoinPredicate ScalarExpr)
           | NestJoin  (JoinPredicate ScalarExpr)
           | GroupJoin (JoinPredicate ScalarExpr) (NE AggrApp)
           | SemiJoin  (JoinPredicate ScalarExpr)
           | AntiJoin  (JoinPredicate ScalarExpr)

data AggrFun = Length | Avg | Minimum | Maximum | And | Or | Sum
```

One important thing worth noting is that aggregation in CL is denoted explicitly
using dedicated AST data type.

#### Types

```haskell
data Type  = ListT Type
           | TupleT [Type]
           | ScalarT ScalarType

data ScalarType  = IntT | BoolT | DoubleT | StringT | UnitT | DecimalT | DateT
```

#### Translation

The most interesting part of `translate` is translation of function application,
which is in fact delegated to `translateApp`.  Here are some example of handling
translation from `Exp` to `CL.Expr` by `translateApp` (module code cleanup and
abbrevation - actual source code is minimaly different):

```haskell
translateApp ConcatMap (Tuple2E (LamE lam) xs) = do
    xs'                 <- translate xs
    (boundVar, bodyExp) <- lamBody lam
    bodyExp'            <- translate bodyExp
    return $ CP.concat $ CP.singleGenComp bodyExp' boundVar xs'
translateApp Minimum   args = CP.minimum <$> translate args
```

where `minimum` is defined as:

```haskell
minimum :: Expr -> Expr
minimum e = let (ListT t) = typeOf e
            in if isNum t
               then AppE1 t (Agg Minimum) e
               else tyErr "minimum"
```

Note that `minimum` inserts explicit aggregation operator and also propagates
the type of agregated expression.


Case study
----------

Below is a simple case study of a an example query from "Language-integrated
Provenance" paper.  All the TH code below was obtained by building the project
using `cabal build --ghc-options=-ddump-splices` and cleaning up the results
slightly.


### Table row representation and derived definitions

To represent a single table row one needs to define a record type which
structure corresponds to table schema:

```haskell
data Agency = Agency
    { a_id       :: Integer
    , a_name     :: Text
    , a_based_in :: Text
    , a_phone    :: Text
    } deriving (Show)

deriveDSH              ''Agency
deriveTA               ''Agency
generateTableSelectors ''Agency
```

`Text` is a data type from `Data.Text`, that is re-exported by `Database.DSH`.
`deriveDSH`, `deriveTA` and `generateTableSelectors` are defined in
`Database.DSH.Frontend.TH`.


#### deriveDSH ''Agency

```haskell
instance QA Agency where
  type Rep Agency = (Rep Integer, Rep Text, Rep Text, Rep Text)
  toExp (Agency id name based_in phone)
    = TupleConstE (Tuple4E (toExp id) (toExp name) (toExp based_in)
                           (toExp phone))
  frExp (TupleConstE (Tuple4E id name based_in phone))
    = Agency (frExp id) (frExp name) (frExp based_in) (frExp phone)
  frExp _ = error "DSH: Impossible happened at ..."

instance View (Q Agency) where
  type ToView (Q Agency) = (Q Integer, Q Text, Q Text, Q Text)
  view (Q agency)
    = ((Q $ (AppE (TupElem Tup4_1) agency)),
       (Q $ (AppE (TupElem Tup4_2) agency)),
       (Q $ (AppE (TupElem Tup4_3) agency)),
       (Q $ (AppE (TupElem Tup4_4) agency)))

agency :: Q Integer -> Q Text -> Q Text -> Q Text -> Q Agency
agency (Q id) (Q name) (Q based_in) (Q phone)
  = Q (TupleConstE (Tuple4E id name based_in phone))
```

**QA instance**: record of basic types is represented as a tuple.  `Tuple4E` is
  a constructor of `TupleConst` and stores four `Exp` values.  `TupleConstE` is
  a data constructor of `Exp`.  `toExp`/`fromExp` are pretty straightforward.

**View instance**: this is slightly less intuitive, so let's break it down into
  smaller pieces.  By definition of `Q` and `Rep Agency` we get that `Q Agency`
  is `Q (Exp (Integer, Text, Text, Text))`.  So for this instance type of
  `view`is:

```haskell
view :: Q (Exp (Integer, Text, Text, Text)) -> (Q Integer, Q Text, Q Text, Q Text)
```

Remember that, by definition of `Q`, `Q Integer` and `Q Text` stand for `Exp
Integer` and `Exp Text`.  Here are the types of other expressions used in the
definition of `view`:

```haskell
Tup4_1  :: TupElem (a,b,c,d) d
TupElem :: TupElem (a,b,c,d) d -> Fun (a,b,c,d) d
agency  :: Exp (Integer, Text, Text, Text)
AppE    :: Fun a b -> Exp a -> Exp b
```


#### deriveTA ''Agency

```haskell
instance TA Agency
```

I don't know what is the purpose of this instance.


#### generateTableSelectors ''Agency

```haskell
a_idQ :: Q Agency -> Q Integer
a_idQ (view -> (id, _, _, _)) = id

a_nameQ :: Q Agency -> Q Text
a_nameQ (view -> (_, name, _, _)) = name

a_based_inQ :: Q Agency -> Q Text
a_based_inQ (view -> (_, _, based_in, _)) = based_in

a_phoneQ :: Q Agency -> Q Text
a_phoneQ (view -> (_, _, _, phone)) = phone
```

These are accessors to all fields of `Agency` defined within `Q` newtype.  These
can be used directly in the queries.


### Table schema

```haskell
agencies :: Q [Agency]
agencies = table "agencies"
                 ( "a_id" :|
                 [ "a_name"
                 , "a_based_in"
                 , "a_phone"
                 ])
                 (TableHints (pure $ Key (pure "a_id") ) NonEmpty)
```

`pure` refers to `Applicative` instance for `NonEmpty` data type. `:|` is a
constructor of `NonEmpty` list.


Query desugaring
----------------

```haskell
-- | Query from Figure 1
q1 :: Q [(Text, Text)]
q1 = [ tup2 (et_nameQ et) (a_phoneQ a)
     | a  <- agencies
     , et <- externalTours
     , a_nameQ a  == et_nameQ et
     , et_typeQ et == "boat"
     ]
```


Query compilation to SQL
------------------------

**TODO:** What is `runQ` and how does `naturalPgCodeGen` work?

```haskell
execQ :: (QA a, Show a) => BackendConn PgVector -> Q a -> IO ()
execQ c q = runQ naturalPgCodeGen c q >>= print
```