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


-- | Type to represent a field number (unique numbered tag).
newtype FieldNumber = FieldNumber Word32
    deriving (Show, Eq, Ord)


instance Num FieldNumber where
    (FieldNumber x) + (FieldNumber y) = FieldNumber (x + y)
    (FieldNumber x) * (FieldNumber y) = FieldNumber (x * y)
    abs (FieldNumber x)               = FieldNumber (abs x)
    negate (FieldNumber x)            = FieldNumber (negate x)
    signum (FieldNumber x)            = FieldNumber (signum x)
    fromInteger i                     = FieldNumber (fromInteger i)


-- | Convert a FieldNumber into a Word32.
fromFieldNumber :: FieldNumber -> Word32
fromFieldNumber (FieldNumber n) = shiftL n 3


-- | Convert a Word32 into a FieldNumber or an error.
toFieldNumber :: Word32 -> Either String FieldNumber
toFieldNumber i
    | fn == 0                    = Left "Invalid FieldNumber 0"
    | fn >= 19000 && fn <= 19999 = Left "Reserved FieldNumber"
    | otherwise                  = Right $ FieldNumber fn
  where fn = shiftR i 3
