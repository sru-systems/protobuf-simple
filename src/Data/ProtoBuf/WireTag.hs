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


data WireTag = WireTag FieldNumber WireType
    deriving (Show, Eq, Ord)


fromWireTag :: WireTag -> Word32
fromWireTag (WireTag fn wt) = fromFieldNumber fn .|. fromWireType wt


toWireTag :: Word32 -> Either String WireTag
toWireTag i = do
    fieldNumber <- toFieldNumber i
    wireType    <- toWireType i
    return $ WireTag fieldNumber wireType
