-- |
-- Module:      Data.ProtoBuf.Mergeable
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Mergeable type class

module Data.ProtoBuf.Mergeable
    ( Mergeable(..)
    ) where


import Data.ByteString.Lazy (ByteString)
import Data.Int (Int32, Int64)
import Data.Sequence (Seq, (><))
import Data.Text.Lazy (Text)
import Data.Word (Word32, Word64)


class Mergeable a where
    merge :: a -> a -> a
    merge _ b = b


instance Mergeable Bool
instance Mergeable ByteString
instance Mergeable Double
instance Mergeable Float
instance Mergeable Int32
instance Mergeable Int64
instance Mergeable Text
instance Mergeable Word32
instance Mergeable Word64


instance Mergeable a => Mergeable (Maybe a) where
    merge Nothing  b        = b
    merge a        Nothing  = a
    merge (Just a) (Just b) = Just (merge a b)


instance Mergeable (Seq a) where
    merge = (><)
