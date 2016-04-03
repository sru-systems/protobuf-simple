-- |
-- Module:      Data.ProtoBuf.Default
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Default type class.

module Data.ProtoBuf.Default
    ( Default(..)
    ) where


import Data.ByteString.Lazy (ByteString)
import Data.Int (Int32, Int64)
import Data.Sequence (Seq)
import Data.Text.Lazy (Text)
import Data.Word (Word32, Word64)

import qualified Data.ByteString.Lazy as B
import qualified Data.Sequence        as S
import qualified Data.Text.Lazy       as T


class Default a where
    defaultVal :: a


instance Default Bool       where defaultVal = False
instance Default ByteString where defaultVal = B.empty
instance Default Double     where defaultVal = 0
instance Default Float      where defaultVal = 0
instance Default Int32      where defaultVal = 0
instance Default Int64      where defaultVal = 0
instance Default Text       where defaultVal = T.empty
instance Default Word32     where defaultVal = 0
instance Default Word64     where defaultVal = 0
instance Default (Maybe a)  where defaultVal = Nothing
instance Default (Seq a)    where defaultVal = S.empty
