{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Schema.PPDP2016.Tours.PolyKeys where

import           Database.DSH
import           Database.Skye.Common ()
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
                 (\a -> tup2 (a_idQ a) (a_nameQ a))
                 (TableHints ( Key ( "a_id" :| [ "a_name" ])  :|
                             [ Key (pure "a_id")
                             ] )
                             NonEmpty NoProvenance)

externalTours :: Q [ExternalTour]
externalTours = table "externaltours"
                      ( "et_id" :|
                      [ "et_name"
                      , "et_destination"
                      , "et_type"
                      , "et_price"
                      ])
                      (\et -> tup2 (et_idQ et) (et_nameQ et))
                      (TableHints ( Key ( "et_id" :| [ "et_name" ])  :|
                                  [ Key (pure "et_id")
                                  ] )
                                  NonEmpty NoProvenance)
