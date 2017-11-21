{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}

-- | This module implements example queries on tour agencies database from the
-- "Language-integrated Provenance" paper by Stefan Fehrenbach and James Cheney.
module Queries.PPDP2016.Tours.Lineage where

import           Database.DSH
import           Database.DSH.Provenance
import           Data.Proxy

import qualified Queries.PPDP2016.Tours.NoProv as NP
import           Schema.PPDP2016.Tours.NoProv

lineageKey :: Proxy Integer
lineageKey = Proxy

q0 :: Q (LT [Agency] Integer)
q0 = lineage lineageKey [ a | a <- agencies ]

q1 :: Q [Lineage (Text, Text) Integer]
q1 = lineage lineageKey NP.q1

-- | Alternative version of q1 presented in Section 2.2 of the paper
q1' :: Q [Lineage (Text, Text) Integer]
q1' = lineage lineageKey NP.q1'

q1'' :: Q [Lineage (Text, Text) Integer]
q1'' = lineage lineageKey NP.q1''

q2 :: Q [Lineage (Text, (Text, Text)) Integer]
q2 = lineage lineageKey NP.q2

-- FL primitives

mapFL :: Q [Lineage Text Integer]
mapFL = lineage lineageKey (map fst NP.q1)

appendFL :: Q [Lineage (Text, Text) Integer]
appendFL = lineage lineageKey (append NP.q1 NP.q1)

-- Does not work because new DSH backend does not implement reverse primitive
-- reverseFL :: Q [Lineage (Text, Text) Integer]
-- reverseFL = lineage lineageKey (reverse NP.q1)

-- Does not work because new DSH backend does not implement reverse primitive
-- zipFL :: Q (LT [((Text, Text), (Text, Text))] Integer)
-- zipFL = lineage lineageKey (zip NP.q1 NP.q1)

-- Exposing lineage limitation

agenciesNames :: Q [Text]
agenciesNames = [ a_nameQ a | a <- agencies ]

lineageBug :: Q [Lineage Text Integer]
lineageBug = lineage lineageKey
                     [ et_nameQ et
                     | et <- externalTours
                     , et_nameQ et `elem` agenciesNames ]

brokenLet :: Q (LT [Agency] Integer)
brokenLet = lineage lineageKey
                    [ a | a <- agencies, a_nameQ a == "EdinTours" ]
