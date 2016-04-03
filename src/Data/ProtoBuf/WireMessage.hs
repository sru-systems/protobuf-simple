-- |
-- Module:      Data.ProtoBuf.WireMessage
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- WireMessage type class.

module Data.ProtoBuf.WireMessage
    ( WireMessage(..)
    ) where


import Data.Binary.Get (Get)
import Data.Binary.Put (Put)
import Data.ProtoBuf.WireTag (WireTag)


class WireMessage a where
    fieldToValue    :: WireTag -> a -> Get a
    messageToFields :: a -> Put
