{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}

-- | This module implements example queries on tour agencies database from the
-- "Language-integrated Provenance" paper by Stefan Fehrenbach and James Cheney.
module Queries.PPDP2016.Tours.Lineage where

import           Database.DSH
import           Database.DSH.Provenance

import qualified Queries.PPDP2016.Tours.NoProv as NP
import           Schema.PPDP2016.Tours.NoProv

q0 :: Q [Lineage Agency Integer]
q0 = lineage [ a | a <- agencies ]

q1 :: Q [Lineage (Text, Text) Integer]
q1 = lineage NP.q1

-- | Alternative version of q1 presented in Section 2.2 of the paper
q1' :: Q [Lineage (Text, Text) Integer]
q1' = lineage NP.q1'

q1'' :: Q [Lineage (Text, Text) Integer]
q1'' = lineage NP.q1''

q2 :: Q [Lineage (Text, (Text, Text)) Integer]
q2 = lineage NP.q2

-- Nested lineage

matchingAgencies :: Q Text -> Q [(Text, Text)]
matchingAgencies name =
    [ tup2 (a_nameQ a) (a_phoneQ a)
    | a <- agencies
    , a_nameQ a == name
    ]

qNested :: Q [Lineage (Text, [Lineage (Text, Text) Integer]) Integer]
qNested = lineage [ tup2 (et_nameQ et) a
                  | et <- externalTours
                  , et_typeQ et == "boat"
                  , let a = lineage (matchingAgencies (et_nameQ et))
                  ]

-- FL primitives

q1map :: Q [Lineage Text Integer]
q1map = lineage (map fst NP.q1)

q1append :: Q [Lineage (Text, Text) Integer]
q1append = lineage (append NP.q1 NP.q1)

q1reverse :: Q [Lineage (Text, Text) Integer]
q1reverse = lineage (reverse NP.q1)

-- Exposing lineage bug

agenciesNames :: Q [Text]
agenciesNames = [ a_nameQ a | a <- agencies ]

lineageBug :: Q [Lineage Text Integer]
lineageBug = lineage [ et_nameQ et
                     | et <- externalTours
                     , et_nameQ et `elem` agenciesNames ]

brokenLet :: Q [Lineage Agency Integer]
brokenLet = lineage [ a | a <- agencies, a_nameQ a == "EdinTours" ]
