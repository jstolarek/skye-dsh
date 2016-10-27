Understanding DSH: A Case Study
===============================

Below are notes on my attempts to understand how DSH works.  My analysis is
based on a case study of an example query from "Language-integrated Provenance"
paper.


Table declarations
------------------

Query is executed on a database that containes agencies defined like this:

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

`Text` is a datatype from `Data.Text`, that is re-exported by `Database.DSH`.

All the TH code below was obtained by building the project using `cabal build
--ghc-options=-ddump-splices` and cleaning up the result slightly.


### deriveDSH ''Agency

Most definitions below come from module `Database.DSH.Frontend.Internals`.

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

### deriveTA ''Agency

```haskell
instance TA Agency
```

### generateTableSelectors ''Agency

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
