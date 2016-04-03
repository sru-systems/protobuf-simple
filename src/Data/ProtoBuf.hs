-- |
-- Module:      Data.ProtoBuf
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Functions for encoding and decoding Protocol Buffers data.

module Data.ProtoBuf
    ( decode
    , encode
    ) where

import Data.ProtoBuf.WireFormat (decode, encode)
