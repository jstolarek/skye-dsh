{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
module Schema.PPDP2016ToursProv where

import           Database.DSH
import           Database.DSH.Provenance
import           Data.List.NonEmpty
import qualified Data.Sequence as S

data Agency = Agency
    { a_id       :: Integer
    , a_name     :: Text
    , a_based_in :: Text
    , a_phone    :: WhereProv Text
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
                 (TableHints (pure $ Key (pure "a_id") ) NonEmpty
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
  type Rep Agency = (Rep Integer, Rep Text, Rep Text, Rep (WhereProv Text))
  toExp (Agency x_a8CS x_a8CT x_a8CU x_a8CV)
    = TupleConstE (Tuple4E (toExp x_a8CS) (toExp x_a8CT) (toExp x_a8CU)
                           (toExp x_a8CV))
  frExp (TupleConstE (Tuple4E x_a8CW x_a8CX x_a8CY x_a8CZ))
    = Agency (frExp x_a8CW) (frExp x_a8CX) (frExp x_a8CY) (frExp x_a8CZ)
  frExp _ = error "DSH: Impossible happened"

instance View (Q Agency) where
  type ToView (Q Agency) = (Q Integer, Q Text, Q Text, Q (WhereProv Text))
  view (Q e_a8D0)
    = ((Q $ (AppE (TupElem Tup4_1) e_a8D0)),
       (Q $ (AppE (TupElem Tup4_2) e_a8D0)),
       (Q $ (AppE (TupElem Tup4_3) e_a8D0)),
       (Q $ (AppE (TupElem Tup4_4) e_a8D0)))

-- JSTOLAREK: smart constructor generates empty provenance information.
-- Obviously, the data is not pulled from the database.
agency :: Q Integer -> Q Text -> Q Text -> Q Text -> Q Agency
agency (Q e_a8D1) (Q e_a8D2) (Q e_a8D3) (Q e_a8D4)
  = Q (TupleConstE (Tuple4E e_a8D1 e_a8D2 e_a8D3
                            (TupleConstE (Tuple2E e_a8D4 (ListE $ S.empty)))))

a_idQ :: Q Agency -> Q Integer
a_idQ (view -> (x_aap2, _, _, _)) = x_aap2
a_nameQ :: Q Agency -> Q Text
a_nameQ (view -> (_, x_aap3, _, _)) = x_aap3
a_based_inQ :: Q Agency -> Q Text
a_based_inQ (view -> (_, _, x_aap4, _)) = x_aap4

-- JSTOLAREK: these two have extra calls to provQ/unprovQ
a_phoneQ :: Q Agency -> Q Text
a_phoneQ (view -> (_, _, _, x_aap5)) = unprovQ x_aap5
a_phone_provQ :: Q Agency -> Q WhereProvInfo
a_phone_provQ (view -> (_, _, _, x_aap5)) = provQ x_aap5
