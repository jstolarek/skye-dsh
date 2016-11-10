{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-- | Below are my experiments on encoding where-provenance in DSH
module Database.DSH.Provenance.Where where

import          Database.DSH

-- | Data type that represents where-provenance as a record containing table
-- name, column name and row number. Note: in the final design this type should
-- be abstract.
data WhereProvData = WhereProvData
    { where_prov_table  :: Text
    , where_prov_column :: Text
    , where_prov_row    :: Integer
    } deriving ( Show )

-- Derives all the internal DSH machinery for WhereProvData.  Note that this
-- also generates instance of View that I will most likely not need
deriveDSH ''WhereProvData

-- | Type family that defines representation of a type with where-provenance
-- information attached.  Note that where-provenance can only be tracked for
-- basic database types (strings, ints and bools) and the three equations
-- reflect just that.
type family WhereProv a where
     WhereProv Text    = ( Text   , Maybe WhereProvData )
     WhereProv Integer = ( Integer, Maybe WhereProvData )
     WhereProv Bool    = ( Bool   , Maybe WhereProvData )
