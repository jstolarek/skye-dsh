{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}

-- | This module implements example queries on tour agencies database from the
-- "Language-integrated Provenance" paper by Stefan Fehrenbach and James Cheney.
module Queries.PPDP2016.Tours.Lineage where

import           Database.DSH
import           Database.DSH.Provenance

import           Schema.PPDP2016.Tours.Lineage

q0' :: Q [Text]
q0' = [ a_nameQ a
      | a <- agencies
      ]

q0 :: Q [Lineage Text Integer]
q0 = [ lineageQ (lineageDataQ z_a) (lineageProvQ al `lineageAppendQ` lineageProvQ z_a)
     | al <- agenciesL
     , let a = lineageDataQ al
     , z_a <- [ emptyLineageQ result :: Q (Lineage Text Integer)
              | let result = a_nameQ a
              ]
     ]

-- | Query from Figure 1
{-
Original query with transformed result
q1 :: Q [(Text, Text)]
q1 = [ result
     | a  <- agencies
     , et <- externalTours
     , a_nameQ a  == et_nameQ et
     , et_typeQ et == "boat"
     , let result = tup2 (et_nameQ et) (a_phoneQ a)
     ]
-}

{-
First step: lineage only requested from agencies table
q1 :: Q [Lineage (Text, Text) Integer]
q1 = [ lineageQ (lineageDataQ z_a) (lineageProvQ al `lineageAppendQ` lineageProvQ z_a)
     | al <- agenciesL
     , let a = lineageDataQ al
     , z_a <- [ emptyLineageQ result :: Q (Lineage (Text, Text) Integer)
              | et <- externalTours
              , a_nameQ a  == et_nameQ et
              , et_typeQ et == "boat"
              , let result = tup2 (et_nameQ et) (a_phoneQ a) ]
     ]
-}

q1 :: Q [Lineage (Text, Text) Integer]
q1 = [ lineageQ (lineageDataQ z_a) (lineageProvQ al `lineageAppendQ` lineageProvQ z_a)
     | al <- agenciesL
     , let a = lineageDataQ al
     , z_a <- [ lineageQ (lineageDataQ z_et)
                         (lineageProvQ etl `lineageAppendQ` lineageProvQ z_et)
              | etl <- externalToursL
              , let et = lineageDataQ etl
              , z_et <- [ emptyLineageQ result :: Q (Lineage (Text, Text) Integer)
                        | a_nameQ a  == et_nameQ et
                        , et_typeQ et == "boat"
                        , let result  = tup2 (et_nameQ et) (a_phoneQ a) ]
              ]
     ]


{-
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
-}
