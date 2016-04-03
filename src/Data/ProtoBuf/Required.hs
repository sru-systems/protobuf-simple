-- |
-- Module:      Data.ProtoBuf.Required
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Required type class.

module Data.ProtoBuf.Required
    ( Required(..)
    ) where


import Data.ProtoBuf.WireTag (WireTag)
import Data.Set (Set)


class Required a where
    reqTags :: a -> Set WireTag
