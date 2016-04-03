-- |
-- Module:      Data.ProtoBuf.FieldNumber
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- FieldNumber type and functions.

module Data.ProtoBuf.FieldNumber
    ( FieldNumber(..)
    , fromFieldNumber
    , toFieldNumber
    ) where


import Data.Bits (shiftL, shiftR)
import Data.Word (Word32)


newtype FieldNumber = FieldNumber Word32
    deriving (Show, Eq, Ord)


instance Num FieldNumber where
    (FieldNumber x) + (FieldNumber y) = FieldNumber (x + y)
    (FieldNumber x) * (FieldNumber y) = FieldNumber (x * y)
    abs (FieldNumber x)               = FieldNumber (abs x)
    negate (FieldNumber x)            = FieldNumber (negate x)
    signum (FieldNumber x)            = FieldNumber (signum x)
    fromInteger i                     = FieldNumber (fromInteger i)


fromFieldNumber :: FieldNumber -> Word32
fromFieldNumber (FieldNumber n) = shiftL n 3


toFieldNumber :: Word32 -> Either String FieldNumber
toFieldNumber i
    | fn == 0                    = Left "Invalid FieldNumber 0"
    | fn >= 19000 && fn <= 19999 = Left "Reserved FieldNumber"
    | otherwise                  = Right $ FieldNumber fn
  where fn = shiftR i 3
