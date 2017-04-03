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

import           Database.DSH.Frontend.Internals

import qualified Data.Sequence as S

data Agency = Agency
    { a_id       :: Integer
    , a_name     :: Text
    , a_based_in :: Text
    , a_phone    :: Text
    } deriving (Show)

deriveDSH              ''Agency
deriveTA               ''Agency
generateTableSelectors ''Agency

-- PROTOTYPING BY HAND

-- some form of this function should probably be placed in the library
add_lineage :: (Reify (Rep a), Reify (Rep key))
            => Q a -> Text -> Q key -> Q (Lineage a key)
add_lineage (Q row) table_name (Q key) =
    Q (TupleConstE (Tuple2E row (ListE (S.singleton
       (TupleConstE (Tuple2E (TextE table_name) key))))))

-- another idea for this function
-- perhaps just lineageQ - a smart Q constructor?
addLineage :: (Reify (Rep a), Reify (Rep key))
           => Q a -> Q (LineageAnnot key) -> Q (Lineage a key)
addLineage (Q row) (Q lineage) = Q (TupleConstE (Tuple2E row lineage))

-- "Agencies" table with lineage transformation performed by hand
agenciesL :: Q [Lineage Agency Integer]
agenciesL = [ add_lineage a "agencies" (a_idQ a) | a <- agencies ]

-- Generate these with TH.  Or perhaps introduce polymorphic selectors?
-- generateLineageSelectors ''Agency
agencies_dataQ :: Q (Lineage Agency Integer) -> Q Agency
agencies_dataQ (view -> (agency_, _)) = agency_

agencies_lineageQ :: Q (Lineage Agency Integer) -> Q (LineageAnnot Integer)
agencies_lineageQ (view -> (_, lineage)) = lineage

-- JSTOLAREK: these should have better names and be placed inside DSH
lineage_dataQ :: (QA a, QA key) => Q (Lineage a key) -> Q a
lineage_dataQ (view -> (a, _)) = a

lineage_provQ :: (QA a, QA key) => Q (Lineage a key) -> Q (LineageAnnot key)
lineage_provQ (view -> (_, lineage)) = lineage


-- JSTOLAREK: generate these with TH.  Perhaps generate lineage selectors at the
-- same time (see above)?
a_idLQ :: Q (Lineage Agency Integer) -> Q Integer
a_idLQ = a_idQ . agencies_dataQ
a_nameLQ :: Q (Lineage Agency Integer) -> Q Text
a_nameLQ = a_nameQ . agencies_dataQ
a_based_inLQ :: Q (Lineage Agency Integer) -> Q Text
a_based_inLQ = a_based_inQ . agencies_dataQ
a_phoneLQ :: Q (Lineage Agency Integer) -> Q Text
a_phoneLQ = a_phoneQ . agencies_dataQ

agencies :: Q [Agency]
agencies = table "agencies"
                 ( "a_id" :|
                 [ "a_name"
                 , "a_based_in"
                 , "a_phone"
                 ])
                 (TableHints (pure $ Key (pure "a_id") ) NonEmpty LineageHint)

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

externalTours :: Q [ExternalTour]
externalTours = table "externaltours"
                      ( "et_id" :|
                      [ "et_name"
                      , "et_destination"
                      , "et_type"
                      , "et_price"
                      ])
                      (TableHints (pure $ Key (pure "et_id") ) NonEmpty NoProvenance)
