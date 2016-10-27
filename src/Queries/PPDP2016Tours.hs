{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}

-- | This module implements an example query on tour agencies database from the
-- "Language-integrated Provenance" paper by Stefan Fehrenbach and James Cheney.
module Queries.PPDP2016Tours where

import           Database.DSH

import           Schema.PPDP2016Tours

-- | Query from Figure 1
q1 :: Q [(Text, Text)]
q1 = [ tup2 (et_nameQ et) (a_phoneQ a)
     | a  <- agencies
     , et <- externalTours
     , a_nameQ a  == et_nameQ et
     , et_typeQ et == "boat"
     ]
