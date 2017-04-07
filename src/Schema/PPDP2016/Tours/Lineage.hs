{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-- JSTOLAREK: required for prototyping
{-# LANGUAGE MonadComprehensions  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
module Schema.PPDP2016.Tours.Lineage where

import           Database.DSH
import           Database.DSH.Provenance
import           Data.List.NonEmpty

data Agency = Agency
    { a_id       :: Integer
    , a_name     :: Text
    , a_based_in :: Text
    , a_phone    :: Text
    } deriving (Show)

deriveDSH              ''Agency
deriveTA               ''Agency
generateTableSelectors ''Agency

agencies :: Q [Lineage Agency Integer]
agencies = table "agencies"
                 ( "a_id" :|
                 [ "a_name"
                 , "a_based_in"
                 , "a_phone"
                 ])
                 (TableHints (pure $ Key (pure "a_id") ) NonEmpty LineageHint)

-- PROTOTYPING BY HAND
{-
-- | Extend a given table with lineage tracking
lineageTable :: (TA a, QA a, QA key)
             => Text -> Q [a] -> (Q a -> Q key) -> Q [Lineage a key]
lineageTable name tbl key =
    [ lineageQ a (lineageAnnotQ name (key a)) | a <- tbl ]

-- "Agencies" table with lineage transfsormation performed by hand
agenciesL :: Q [Lineage Agency Integer]
agenciesL = lineageTable "agencies" agencies a_idQ
-}
-- JSTOLAREK: generate these with TH.
{-
a_idLQ :: Q (Lineage Agency Integer) -> Q Integer
a_idLQ = a_idQ . lineageDataQ
a_nameLQ :: Q (Lineage Agency Integer) -> Q Text
a_nameLQ = a_nameQ . lineageDataQ
a_based_inLQ :: Q (Lineage Agency Integer) -> Q Text
a_based_inLQ = a_based_inQ . lineageDataQ
a_phoneLQ :: Q (Lineage Agency Integer) -> Q Text
a_phoneLQ = a_phoneQ . lineageDataQ
-}

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

externalTours :: Q [Lineage ExternalTour Integer]
externalTours = table "externaltours"
                      ( "et_id" :|
                      [ "et_name"
                      , "et_destination"
                      , "et_type"
                      , "et_price"
                      ])
                      (TableHints (pure $ Key (pure "et_id") ) NonEmpty LineageHint)
{-
externalToursL :: Q [Lineage ExternalTour Integer]
externalToursL = lineageTable "externalTours" externalTours et_idQ
-}
