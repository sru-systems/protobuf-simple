-- |
-- Module:      Data.ProtoBufInt
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Internal functions used by the types for encoding and decoding.

module Data.ProtoBufInt
    ( module Data.ProtoBuf.Default
    , module Data.ProtoBuf.FieldNumber
    , module Data.ProtoBuf.Mergeable
    , module Data.ProtoBuf.Required
    , module Data.ProtoBuf.WireEnum
    , module Data.ProtoBuf.WireFormat
    , module Data.ProtoBuf.WireMessage
    , module Data.ProtoBuf.WireTag
    , module Data.ProtoBuf.WireType
    , module Export
    , append
    ) where


import Data.ProtoBuf.Default
import Data.ProtoBuf.FieldNumber
import Data.ProtoBuf.Mergeable
import Data.ProtoBuf.Required
import Data.ProtoBuf.WireEnum
import Data.ProtoBuf.WireFormat
import Data.ProtoBuf.WireMessage
import Data.ProtoBuf.WireTag
import Data.ProtoBuf.WireType

import Data.Sequence (Seq, (|>))

import Data.Bool             as Export (Bool)
import Data.ByteString.Lazy  as Export (ByteString)
import Data.Int              as Export (Int32, Int64)
import Data.Maybe            as Export (Maybe(..))
import Data.Sequence         as Export (Seq)
import Data.Set              as Export (fromList)
import Data.Text.Lazy        as Export (Text)
import Data.Word             as Export (Word32, Word64)
import Prelude               as Export (Double, Eq, Float, Ord, Show)


append :: Seq a -> a -> Seq a
append seq val = seq |> val
