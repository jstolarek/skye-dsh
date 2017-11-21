{-# OPTIONS_GHC -Wno-orphans      #-}
module Database.Skye.Common where

import           Database.DSH.Provenance
import           Data.Default
import           Data.Text

-- We typically track where-provenance using Integer keys
type WhereProvI a = WhereProv a Integer

instance Default Text where
    def = pack ""
