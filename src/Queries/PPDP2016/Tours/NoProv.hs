{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}

-- | This module implements example queries on tour agencies database from the
-- "Language-integrated Provenance" paper by Stefan Fehrenbach and James Cheney.
module Queries.PPDP2016.Tours.NoProv where

import           Database.DSH

import           Schema.PPDP2016.Tours.NoProv

-- | Query from Figure 1
q1 :: Q [(Text, Text)]
q1 = [ tup2 (et_nameQ et) (a_phoneQ a)
     | a  <- agencies
     , et <- externalTours
     , a_nameQ a  == et_nameQ et
     , et_typeQ et == "boat"
     ]


-- | Alternative version of q1 presented in Section 2.2 of the paper
q1' :: Q [(Text, Text)]
q1' = [ tup2 (et_nameQ et) (a_phoneQ a)
      | et <- externalTours
      , et_typeQ et == "boat"
      , a  <- agencies
      , a_nameQ a  == et_nameQ et
      ]

matchingAgencies :: Q Text -> Q [(Text, Text)]
matchingAgencies name =
    [ tup2 (a_nameQ a) (a_phoneQ a)
    | a <- agencies
    , a_nameQ a == name
    ]

q1'' :: Q [(Text, Text)]
q1'' = [ a
       | et <- externalTours
       , et_typeQ et == "boat"
       , a <- matchingAgencies (et_nameQ et)
       ]

q2 :: Q [(Text, (Text, Text))]
q2 = [ tup2 (a_nameQ a) tours
     | a <- agencies
     , tours <- [ tup2 (et_destinationQ et) (et_typeQ et)
                | et <- externalTours
                , et_nameQ et == a_nameQ a
                ]
     ]
