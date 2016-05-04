-- |
-- Module:      Data.ProtoBuf.WireMessage
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- WireMessage typeclass.

module Data.ProtoBuf.WireMessage
    ( WireMessage(..)
    ) where


import Data.Binary.Get (Get)
import Data.Binary.Put (Put)
import Data.ProtoBuf.WireTag (WireTag)


-- | Typeclass to handle encoding and decoding of messages.
class WireMessage a where

    -- | Decode a field and merge it with the existing value in the message.
    fieldToValue    :: WireTag -> a -> Get a

    -- | Encode all the fields of the message.
    messageToFields :: a -> Put
