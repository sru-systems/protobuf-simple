-- |
-- Module:      Data.ProtoBuf.ZigZag
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Functions to ZigZag encode and decode Int32 and Int64.

module Data.ProtoBuf.ZigZag
    ( decode32
    , decode64
    , encode32
    , encode64
    ) where

import Data.Bits ((.&.), shiftL, shiftR, xor)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)


-- | Decode a ZigZag encoded Int32.
decode32 :: Word32 -> Int32
decode32 x = xor (fromIntegral $ shiftR x 1) (negate (fromIntegral $ x .&. 1))


-- | Decode a ZigZag encoded Int64.
decode64 :: Word64 -> Int64
decode64 x = xor (fromIntegral $ shiftR x 1) (negate (fromIntegral $ x .&. 1))


-- | ZigZag encode an Int32.
encode32 :: Int32 -> Word32
encode32 x = fromIntegral $ xor (shiftL x 1) (shiftR x 31)


-- | ZigZag encode an Int64.
encode64 :: Int64 -> Word64
encode64 x = fromIntegral $ xor (shiftL x 1) (shiftR x 63)
