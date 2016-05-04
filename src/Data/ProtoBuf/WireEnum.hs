-- |
-- Module:      Data.ProtoBuf.WireEnum
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- WireEnum typeclass.

module Data.ProtoBuf.WireEnum
    ( WireEnum(..)
    ) where


import Data.Int (Int32)


-- | Typeclass to handle encoding en decoding of enums.
class WireEnum a where

    -- | Convert an Int32 to an enum value.
    intToEnum :: Int32 -> a


    -- | Convert a enum value to an Int32.
    enumToInt :: a -> Int32
