-- |
-- Module:      Data.ProtoBuf.WireTag
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- WireTag type and functions.

module Data.ProtoBuf.WireTag
    ( WireTag(..)
    , fromWireTag
    , toWireTag
    ) where


import Data.Bits ((.|.))
import Data.ProtoBuf.FieldNumber
import Data.ProtoBuf.WireType
import Data.Word (Word32)


-- | Type to represent a wire tag.
data WireTag = WireTag FieldNumber WireType
    deriving (Show, Eq, Ord)


-- | Convert a WireTag into a Word32.
fromWireTag :: WireTag -> Word32
fromWireTag (WireTag fn wt) = fromFieldNumber fn .|. fromWireType wt


-- | Convert a Word32 into a WireTag or an error.
toWireTag :: Word32 -> Either String WireTag
toWireTag i = do
    fieldNumber <- toFieldNumber i
    wireType    <- toWireType i
    return $ WireTag fieldNumber wireType

