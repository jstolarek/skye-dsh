{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}

-- | This module implements example queries on tour agencies database from the
-- "Language-integrated Provenance" paper by Stefan Fehrenbach and James Cheney.
module Queries.PPDP2016.Tours.Lineage where

import           Database.DSH
import           Database.DSH.Provenance

import qualified Queries.PPDP2016.Tours.NoProv as NP

q1 :: Q [Lineage (Text, Text) Integer]
q1 = lineage NP.q1

-- | Alternative version of q1 presented in Section 2.2 of the paper
q1' :: Q [Lineage (Text, Text) Integer]
q1' = lineage NP.q1'

q1'' :: Q [Lineage (Text, Text) Integer]
q1'' = lineage NP.q1''

q2 :: Q [Lineage (Text, (Text, Text)) Integer]
q2 = lineage NP.q2
