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
                 (TableHints (pure $ Key (pure "a_id") ) NonEmpty LineageHint)

externalTours :: Q [ExternalTour]
externalTours = table "externaltours"
                      ( "et_id" :|
                      [ "et_name"
                      , "et_destination"
                      , "et_type"
                      , "et_price"
                      ])
                      (TableHints (pure $ Key (pure "et_id") ) NonEmpty NoProvenance)

-- PROTOTYPING BY HAND

-- some form of this function should probably be places in the library
add_lineage :: (Reify (Rep a), Reify (Rep key))
            => Q a -> Text -> Q key -> Q (Lineage a key)
add_lineage (Q row) table_name (Q key) =
    Q (TupleConstE (Tuple2E row (ListE (S.singleton
       (TupleConstE (Tuple2E (TextE table_name) key))))))

-- "Agencies" table with lineage transformation performed by hand
agenciesL :: Q [Lineage Agency Integer]
agenciesL = [ add_lineage a "agencies" (a_idQ a) | a <- agencies ]

-- Generate these with TH.  Or perhaps introduce polymorphic selectors?
agencies_dataQ :: Q (Lineage Agency Integer) -> Q Agency
agencies_dataQ (view -> (agency_, _)) = agency_

agencies_lineageQ :: Q (Lineage Agency Integer) -> Q (LineageAnnot Integer)
agencies_lineageQ (view -> (_, lineage)) = lineage
