{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}

-- | This module implements example queries with where-provenance tracking on
-- tour agencies database from the "Language-integrated Provenance" paper by
-- Stefan Fehrenbach and James Cheney.
module Queries.PPDP2016ToursProv2 where

import           Database.DSH
import           Database.DSH.Provenance

import           Schema.PPDP2016Tours
import           Schema.PPDP2016ToursProv2

-- | Query from Figure 1
q1 :: Q [(Text, WhereProv Text)]
q1 = [ tup2 (et_nameQ et) (p_phoneQ a)
     | a  <- agenciesP
     , et <- externalTours
     , p_nameQ a  == et_nameQ et
     , et_typeQ et == "boat"
     ]

-- | Alternative version of q1 presented in Section 2.2 of the paper
q1' :: Q [(Text, WhereProv Text)]
q1' = [ tup2 (et_nameQ et) (p_phoneQ a)
      | et <- externalTours
      , et_typeQ et == "boat"
      , a  <- agenciesP
      , p_nameQ a  == et_nameQ et
      ]

matchingAgencies :: Q Text -> Q [(Text, WhereProv Text)]
matchingAgencies name =
    [ tup2 (p_nameQ a) (p_phoneQ a)
    | a <- agenciesP
    , p_nameQ a == name
    ]

q1'' :: Q [(Text, WhereProv Text)]
q1'' = [ a
       | et <- externalTours
       , et_typeQ et == "boat"
       , a <- matchingAgencies (et_nameQ et)
       ]

q2 :: Q [(Text, (Text, Text))]
q2 = [ tup2 (p_nameQ a) tours
     | a <- agenciesP
     , tours <- [ tup2 (et_destinationQ et) (et_typeQ et)
                | et <- externalTours
                , et_nameQ et == p_nameQ a
                ]
     ]
