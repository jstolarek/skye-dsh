{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Schema.PPDP2016.Tours.Lineage where

import           Database.DSH
import           Database.DSH.Provenance
import           Data.List.NonEmpty

import           Database.DSH.Frontend.Internals


data Agency = Agency
    { a_id       :: Integer
    , a_name     :: Text
    , a_based_in :: Text
    , a_phone    :: Text
    } deriving (Show)

deriveDSH              ''Agency
deriveTA               ''Agency
generateTableSelectors ''Agency

agencies :: Q [Agency]
agencies = table "agencies"
                 ( "a_id" :|
                 [ "a_name"
                 , "a_based_in"
                 , "a_phone"
                 ])
                 (TableHints (pure $ Key (pure "a_id") ) NonEmpty LineageHint)

instance RowKey (Integer, Text, Text, Text) Integer where
    rowKey ( a) = (AppE (TupElem Tup4_1) a)

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
