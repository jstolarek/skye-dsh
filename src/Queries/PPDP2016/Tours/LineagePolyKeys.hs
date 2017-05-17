{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}

-- | This module implements example queries on tour agencies database from the
-- "Language-integrated Provenance" paper by Stefan Fehrenbach and James Cheney.
module Queries.PPDP2016.Tours.LineagePolyKeys where

import           Database.DSH
import           Database.DSH.Provenance

import qualified Queries.PPDP2016.Tours.WhereProvPolyKeys as PK

q1 :: Q [Lineage (Text, WhereProv Text (Integer, Text)) (Integer, Text)]
q1 = lineage PK.q1

-- | Alternative version of q1 presented in Section 2.2 of the paper
q1' :: Q [Lineage (Text, WhereProv Text (Integer, Text)) (Integer, Text)]
q1' = lineage PK.q1'

q1'' :: Q [Lineage (Text, WhereProv Text (Integer, Text)) (Integer, Text)]
q1'' = lineage PK.q1''

q2 :: Q [Lineage (Text, (Text, WhereProv Text (Integer, Text))) (Integer, Text)]
q2 = lineage PK.q2
