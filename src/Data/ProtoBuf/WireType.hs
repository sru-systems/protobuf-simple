-- |
-- Module:      Data.ProtoBuf.WireType
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- WireType type and functions.

module Data.ProtoBuf.WireType
    ( WireType(..)
    , fromWireType
    , toWireType
    ) where


import Data.Bits ((.&.))
import Data.Word (Word32)


-- | Type to represent the Protocol Buffers wire type.
data WireType
    -- | The varint type: int32, int64, uint32, sint32, sint64, bool enum
    = VarInt
    -- | The 64-bit type: fixed64, sfixed64, double
    | Bit64
    -- | The length-delimited: string, bytes, embedded messages, packed repeated fields
    | LenDelim
    -- | The 32-bit type: fixed32, sfixed32, float
    | Bit32
    deriving (Show, Eq, Ord)


-- | Convert a WireType into a Word32.
fromWireType :: WireType -> Word32
fromWireType VarInt   = 0
fromWireType Bit64    = 1
fromWireType LenDelim = 2
fromWireType Bit32    = 5


-- | Convert a Word32 into a WireType or an error.
toWireType :: Word32 -> Either String WireType
toWireType i
    | wType == 0 = Right VarInt
    | wType == 1 = Right Bit64
    | wType == 2 = Right LenDelim
    | wType == 3 = Left "Deprecated WireType: 3 (Start Group)"
    | wType == 4 = Left "Deprecated WireType: 4 (End Group)"
    | wType == 5 = Right Bit32
    | otherwise  = Left $ "Invalid WireType: " ++ show wType
  where wType = i .&. 7
