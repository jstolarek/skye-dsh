{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Schema.PPDP2016.Tours.WhereProvPolyKeys where

import           Database.DSH
import           Database.DSH.Provenance
import           Database.Skye.Common ()
import           Data.List.NonEmpty

data Agency = Agency
    { a_id       :: Integer
    , a_name     :: Text
    , a_based_in :: Text
    , a_phone    :: WhereProv Text (Integer, Text)
    } deriving (Show)

deriveTA ''Agency

data ExternalTour = ExternalTour
    { et_id          :: Integer
    , et_name        :: Text
    , et_destination :: Text
    , et_type        :: Text
    , et_price       :: Integer
    } deriving (Show)

deriveDSH              ''ExternalTour
deriveTA               ''ExternalTour
generateTableSelectors ''ExternalTour

agencies :: Q [Agency]
agencies = table "agencies"
                 ( "a_id" :|
                 [ "a_name"
                 , "a_based_in"
                 , "a_phone"
                 ])
                 (TableHints ( Key ( "a_id" :| [ "a_name" ])  :|
                             [ Key (pure "a_id")
                             ] )
                             NonEmpty
                             (WhereProvenance $ pure "a_phone"))

externalTours :: Q [ExternalTour]
externalTours = table "externaltours"
                      ( "et_id" :|
                      [ "et_name"
                      , "et_destination"
                      , "et_type"
                      , "et_price"
                      ])
                      (TableHints (pure $ Key (pure "et_id") ) NonEmpty
                                  NoProvenance)

-- JSTOLAREK: prototype code below.  In the final version of library all these
-- code needs to be generated with TH

instance QA Agency where
  type Rep Agency = (Rep Integer, Rep Text, Rep Text, Rep (WhereProv Text (Integer, Text)))
  toExp (Agency x_a8CS x_a8CT x_a8CU x_a8CV)
    = TupleConstE (Tuple4E (toExp x_a8CS) (toExp x_a8CT) (toExp x_a8CU)
                           (toExp x_a8CV))
  frExp (TupleConstE (Tuple4E x_a8CW x_a8CX x_a8CY x_a8CZ))
    = Agency (frExp x_a8CW) (frExp x_a8CX) (frExp x_a8CY) (frExp x_a8CZ)
  frExp _ = error "DSH: Impossible happened"

instance View (Q Agency) where
  type ToView (Q Agency) = (Q Integer, Q Text, Q Text, Q (WhereProv Text (Integer, Text)))
  view (Q e_a8D0)
    = ((Q $ (AppE (TupElem Tup4_1) e_a8D0)),
       (Q $ (AppE (TupElem Tup4_2) e_a8D0)),
       (Q $ (AppE (TupElem Tup4_3) e_a8D0)),
       (Q $ (AppE (TupElem Tup4_4) e_a8D0)))

-- JSTOLAREK: smart constructor generates empty provenance information.
-- Obviously, the data is not pulled from the database.
agency :: Q Integer -> Q Text -> Q Text -> Q Text -> Q Agency
agency (Q e_a8D1) (Q e_a8D2) (Q e_a8D3) e_a8D4
  = Q (TupleConstE (Tuple4E e_a8D1 e_a8D2 e_a8D3
                    (unQ ((emptyProvQ e_a8D4) :: Q (WhereProv Text (Integer, Text))))))

-- JSTOLAREK: but we might also want to reconstruct an agency with some known
-- provenance
agencyP :: Q Integer -> Q Text -> Q Text -> Q (WhereProv Text (Integer, Text)) -> Q Agency
agencyP (Q e_a8D1) (Q e_a8D2) (Q e_a8D3) (Q e_a8D4)
  = Q (TupleConstE (Tuple4E e_a8D1 e_a8D2 e_a8D3 e_a8D4))

-- An open question is what functions to generate if we have more than one field
-- with provenance tracking?  I would say that there should be two constructors
-- at most: one without provenance information and another with all possible
-- provenance information.  Then we can either discard where-provenance
-- information or add empty provenance to a value.

a_idQ :: Q Agency -> Q Integer
a_idQ (view -> (x_aap2, _, _, _)) = x_aap2
a_nameQ :: Q Agency -> Q Text
a_nameQ (view -> (_, x_aap3, _, _)) = x_aap3
a_based_inQ :: Q Agency -> Q Text
a_based_inQ (view -> (_, _, x_aap4, _)) = x_aap4

-- JSTOLAREK: a different type signature
a_phoneQ :: Q Agency -> Q (WhereProv Text (Integer, Text))
a_phoneQ (view -> (_, _, _, x_aap5)) = x_aap5

-- JSTOLAREK: these two have extra calls to dataQ/provQ
a_phone_dataQ :: Q Agency -> Q Text
a_phone_dataQ (view -> (_, _, _, x_aap5)) = dataQ x_aap5

a_phone_provQ :: Q Agency -> Q (WhereProvInfo (Integer, Text))
a_phone_provQ (view -> (_, _, _, x_aap5)) = provQ x_aap5
