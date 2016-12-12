{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
module Schema.PPDP2016.Tours.WhereProv where

import           Database.DSH
import           Database.DSH.Provenance
import           Database.Skye.Common
import           Data.List.NonEmpty

data Agency = Agency
    { a_id       :: Integer
    , a_name     :: Text
    , a_based_in :: Text
    , a_phone    :: WhereProvI Text
    } deriving (Show)

deriveDSH ''Agency
deriveTA  ''Agency

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

a_idQ :: Q Agency -> Q Integer
a_idQ (view -> (x_aap2, _, _, _)) = x_aap2
a_nameQ :: Q Agency -> Q Text
a_nameQ (view -> (_, x_aap3, _, _)) = x_aap3
a_based_inQ :: Q Agency -> Q Text
a_based_inQ (view -> (_, _, x_aap4, _)) = x_aap4

-- JSTOLAREK: a different type signature
a_phoneQ :: Q Agency -> Q (WhereProvI Text)
a_phoneQ (view -> (_, _, _, x_aap5)) = x_aap5

-- JSTOLAREK: these two have extra calls to dataQ/provQ
a_phone_dataQ :: Q Agency -> Q (ProvData (WhereProvI Text))
a_phone_dataQ (view -> (_, _, _, x_aap5)) = dataQ x_aap5

a_phone_provQ :: Q Agency -> Q (ProvAnnot (WhereProvI Text))
a_phone_provQ (view -> (_, _, _, x_aap5)) = provQ x_aap5
