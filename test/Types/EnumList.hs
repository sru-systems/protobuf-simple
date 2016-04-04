-- Generated by protobuf-simple. DO NOT EDIT!
module Types.EnumList where

import Control.Applicative ((<$>))
import Prelude ()
import qualified Data.ProtoBufInt as PB
import qualified Types.Enum

newtype EnumList = EnumList
  { value :: PB.Seq Types.Enum.Enum
  } deriving (PB.Show, PB.Eq, PB.Ord)

instance PB.Default EnumList where
  defaultVal = EnumList
    { value = PB.defaultVal
    }

instance PB.Mergeable EnumList where
  merge a b = EnumList
    { value = PB.merge (value a) (value b)
    }

instance PB.Required EnumList where
  reqTags _ = PB.fromList []

instance PB.WireMessage EnumList where
  fieldToValue (PB.WireTag 1 PB.VarInt) self = (\v -> self{value = PB.append (value self) v}) <$> PB.getEnum
  fieldToValue tag self = PB.getUnknown tag self

  messageToFields self = do
    PB.putEnumList (PB.WireTag 1 PB.VarInt) (value self)

