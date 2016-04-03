-- |
-- Module:      Data.ProtoBuf.WireEnum
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- WireEnum type class.

module Data.ProtoBuf.WireEnum
    ( WireEnum(..)
    ) where


import Data.Int (Int32)


class WireEnum a where
    intToEnum :: Int32 -> a
    enumToInt :: a -> Int32
