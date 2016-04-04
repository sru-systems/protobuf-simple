-- Generated by protobuf-simple. DO NOT EDIT!
module Types.Int32OptMsg where

import Control.Applicative ((<$>))
import Prelude ()
import qualified Data.ProtoBufInt as PB

newtype Int32OptMsg = Int32OptMsg
  { value :: PB.Maybe PB.Int32
  } deriving (PB.Show, PB.Eq, PB.Ord)

instance PB.Default Int32OptMsg where
  defaultVal = Int32OptMsg
    { value = PB.defaultVal
    }

instance PB.Mergeable Int32OptMsg where
  merge a b = Int32OptMsg
    { value = PB.merge (value a) (value b)
    }

instance PB.Required Int32OptMsg where
  reqTags _ = PB.fromList []

instance PB.WireMessage Int32OptMsg where
  fieldToValue (PB.WireTag 1 PB.VarInt) self = (\v -> self{value = PB.merge (value self) v}) <$> PB.getInt32Opt
  fieldToValue tag self = PB.getUnknown tag self

  messageToFields self = do
    PB.putInt32Opt (PB.WireTag 1 PB.VarInt) (value self)

