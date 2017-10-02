{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
module Schema.PPDP2016.Tours.NoProv where

import           Database.DSH
import           Data.List.NonEmpty

-- JSTOLAREK: needed for QLTable instances
import           Database.DSH.Provenance
import           Data.Type.Equality

data Agency = Agency
    { a_id       :: Integer
    , a_name     :: Text
    , a_based_in :: Text
    , a_phone    :: Text
    } deriving (Show)

deriveDSH              ''Agency
deriveTA               ''Agency
generateTableSelectors ''Agency

-- JSTOLAREK: generate this with TH
instance QLTable Agency where
    type LT Agency k = Agency
    ltEq _ _ = Refl

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

-- JSTOLAREK: generate this with TH
instance QLTable ExternalTour where
    type LT ExternalTour k = ExternalTour
    ltEq _ _ = Refl

agencies :: Q [Agency]
agencies = table "agencies"
                 ( "a_id" :|
                 [ "a_name"
                 , "a_based_in"
                 , "a_phone"
                 ])
                 a_idQ
                 (TableHints (pure $ Key (pure "a_id") ) NonEmpty NoProvenance)

externalTours :: Q [ExternalTour]
externalTours = table "externaltours"
                      ( "et_id" :|
                      [ "et_name"
                      , "et_destination"
                      , "et_type"
                      , "et_price"
                      ])
                      et_idQ
                      (TableHints (pure $ Key (pure "et_id") ) NonEmpty NoProvenance)
