-- |
-- Module:      Data.ProtoBuf.Required
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Required typeclass.

module Data.ProtoBuf.Required
    ( Required(..)
    ) where


import Data.ProtoBuf.WireTag (WireTag)
import Data.Set (Set)


-- | Typeclass to retrieve required WireTags.
class Required a where

    -- | The required WireTags for the data type.
    reqTags :: a -> Set WireTag
