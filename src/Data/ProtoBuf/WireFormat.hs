-- |
-- Module:      Data.ProtoBuf.WireFormat
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Functions for converting Haskell to wire format and back.

module Data.ProtoBuf.WireFormat
    ( module Data.ProtoBuf.Default
    , module Data.ProtoBuf.FieldNumber
    , module Data.ProtoBuf.Mergeable
    , module Data.ProtoBuf.Required
    , module Data.ProtoBuf.WireEnum
    , module Data.ProtoBuf.WireMessage
    , module Data.ProtoBuf.WireTag
    , module Data.ProtoBuf.WireType
    , decode
    , encode

    , getBool
    , getBoolOpt
    , getBoolPacked
    , getBytes
    , getBytesOpt
    , getDouble
    , getDoubleOpt
    , getDoublePacked
    , getEnum
    , getEnumOpt
    , getEnumPacked
    , getFixed32
    , getFixed32Opt
    , getFixed32Packed
    , getFixed64
    , getFixed64Opt
    , getFixed64Packed
    , getFloat
    , getFloatOpt
    , getFloatPacked
    , getGroup
    , getGroupOpt
    , getInt32
    , getInt32Opt
    , getInt32Packed
    , getInt64
    , getInt64Opt
    , getInt64Packed
    , getMessage
    , getMessageOpt
    , getSFixed32
    , getSFixed32Opt
    , getSFixed32Packed
    , getSFixed64
    , getSFixed64Opt
    , getSFixed64Packed
    , getSInt32
    , getSInt32Opt
    , getSInt32Packed
    , getSInt64
    , getSInt64Opt
    , getSInt64Packed
    , getString
    , getStringOpt
    , getUInt32
    , getUInt32Opt
    , getUInt32Packed
    , getUInt64
    , getUInt64Opt
    , getUInt64Packed
    , getUnknown
    , getWireTag

    , putBool
    , putBoolList
    , putBoolOpt
    , putBoolPacked
    , putBytes
    , putBytesList
    , putBytesOpt
    , putDouble
    , putDoubleList
    , putDoubleOpt
    , putDoublePacked
    , putEnum
    , putEnumList
    , putEnumOpt
    , putEnumPacked
    , putFixed32
    , putFixed32List
    , putFixed32Opt
    , putFixed32Packed
    , putFixed64
    , putFixed64List
    , putFixed64Opt
    , putFixed64Packed
    , putFloat
    , putFloatList
    , putFloatOpt
    , putFloatPacked
    , putGroup
    , putGroupOpt
    , putInt32
    , putInt32List
    , putInt32Opt
    , putInt32Packed
    , putInt64
    , putInt64List
    , putInt64Opt
    , putInt64Packed
    , putSFixed32
    , putSFixed32List
    , putSFixed32Opt
    , putSFixed32Packed
    , putSFixed64
    , putSFixed64List
    , putSFixed64Opt
    , putSFixed64Packed
    , putSInt32
    , putSInt32List
    , putSInt32Opt
    , putSInt32Packed
    , putSInt64
    , putSInt64List
    , putSInt64Opt
    , putSInt64Packed
    , putMessage
    , putMessageList
    , putMessageOpt
    , putString
    , putStringList
    , putStringOpt
    , putUInt32
    , putUInt32List
    , putUInt32Opt
    , putUInt32Packed
    , putUInt64
    , putUInt64List
    , putUInt64Opt
    , putUInt64Packed
    , putWireTag
    ) where


import Data.Binary.Get (Get, getLazyByteString, getWord8, getWord32le, getWord64le, isEmpty, runGetOrFail)
import Data.Binary.IEEE754 (getFloat32le, getFloat64le, putFloat32le, putFloat64le)
import Data.Binary.Put ( Put, putLazyByteString, putWord8, putWord32le, putWord64le, runPut)
import Data.Bits (Bits, (.|.), (.&.), shiftL, shiftR, setBit, testBit)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (forM_)
import Data.Int (Int32, Int64)
import Data.ProtoBuf.Default (Default(..))
import Data.ProtoBuf.FieldNumber (fromFieldNumber, toFieldNumber, FieldNumber(..))
import Data.ProtoBuf.Mergeable (Mergeable(..))
import Data.ProtoBuf.Required (Required(..))
import Data.ProtoBuf.WireEnum (WireEnum(..))
import Data.ProtoBuf.WireMessage (WireMessage(..))
import Data.ProtoBuf.WireTag (fromWireTag, toWireTag, WireTag(..))
import Data.ProtoBuf.WireType (fromWireType, toWireType, WireType(..))
import Data.Sequence (Seq, (|>))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word8, Word32, Word64)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ProtoBuf.ZigZag as ZZ
import qualified Data.Sequence        as Seq
import qualified Data.Set             as Set


-- | Decode a ByteString into either the data-type or an error message.
--
-- Decode CustomType:
--
-- > decCustomType :: ByteString -> Either String CustomType
-- > decCustomType = decode
decode :: (Default a, Required a, WireMessage a) => ByteString -> Either String a
decode bytes = case runGetOrFail getGroup bytes of
    Left  (_, _, err) -> Left err
    Right (_, _, obj) -> Right obj


-- | Encode a data-type into a ByteString.
--
-- Encode CustomType:
--
-- > encCustomType :: CustomType -> ByteString
-- > encCustomType = encode
encode :: (WireMessage a) => a -> ByteString
encode obj = runPut (putGroup obj)


-- | Decode a required bool field.
getBool :: Get Bool
getBool = do
    val <- getVarInt :: Get Word8
    if val == 0
    then return False
    else return True


-- | Decode an optional bool field.
getBoolOpt :: Get (Maybe Bool)
getBoolOpt = getOpt getBool


-- | Decode a packed repeated bool field.
getBoolPacked :: Get (Seq Bool)
getBoolPacked = getPacked getBool


-- | Decode a required bytes field.
getBytes :: Get ByteString
getBytes = do
    len <- getVarInt
    getLazyByteString len


-- | Decode an optional bytes field.
getBytesOpt :: Get (Maybe ByteString)
getBytesOpt = getOpt getBytes


-- | Decode a required double field.
getDouble :: Get Double
getDouble = getFloat64le


-- | Decode an optional double field.
getDoubleOpt :: Get (Maybe Double)
getDoubleOpt = getOpt getDouble


-- | Decode a packed repeated double field.
getDoublePacked :: Get (Seq Double)
getDoublePacked = getPacked getDouble


-- | Decode a required enum field.
getEnum :: (WireEnum a) => Get a
getEnum = intToEnum <$> getInt32


-- | Decode an optional enum field.
getEnumOpt :: (WireEnum a) => Get (Maybe a)
getEnumOpt = getOpt getEnum


-- | Decode a packed repeated enum field.
getEnumPacked :: (WireEnum a) => Get (Seq a)
getEnumPacked = getPacked getEnum


-- | Decode a required fixed32 field.
getFixed32 :: Get Word32
getFixed32 = getWord32le


-- | Decode an optional fixed32 field.
getFixed32Opt :: Get (Maybe Word32)
getFixed32Opt = getOpt getFixed32


-- | Decode a packed repeated fixed32 field.
getFixed32Packed :: Get (Seq Word32)
getFixed32Packed = getPacked getFixed32


-- | Decode a required fixed64 field.
getFixed64 :: Get Word64
getFixed64 = getWord64le


-- | Decode an optional fixed64 field.
getFixed64Opt :: Get (Maybe Word64)
getFixed64Opt = getOpt getFixed64


-- | Decode a packed repeated fixed64 field.
getFixed64Packed :: Get (Seq Word64)
getFixed64Packed = getPacked getFixed64


-- | Decode a required float field.
getFloat :: Get Float
getFloat = getFloat32le


-- | Decode an optional float field.
getFloatOpt :: Get (Maybe Float)
getFloatOpt = getOpt getFloat


-- | Decode a packed repeated float field.
getFloatPacked :: Get (Seq Float)
getFloatPacked = getPacked getFloat


-- | Decode a required group field.
getGroup :: (Default a, Required a, WireMessage a) => Get a
getGroup = loop defaultMsg reqTagSet
    where
        loop msg reqs | Set.null reqs = do
            done <- isEmpty
            if done
            then return msg
            else do
                tag <- getWireTag
                msg' <- fieldToValue tag msg
                loop msg' reqs
        loop msg reqs = do
            done <- isEmpty
            if done
            then fail "Missing required field(s)"
            else do
                tag <- getWireTag
                msg' <- fieldToValue tag msg
                loop msg' (Set.delete tag reqs)
        defaultMsg = defaultVal
        reqTagSet = reqTags defaultMsg


-- | Decode an optional group field.
getGroupOpt :: (Default a, Required a, WireMessage a) => Get (Maybe a)
getGroupOpt = getOpt getGroup


-- | Decode a required int32 field.
getInt32 :: Get Int32
getInt32 = getVarInt


-- | Decode an optional int32 field.
getInt32Opt :: Get (Maybe Int32)
getInt32Opt = getOpt getInt32


-- | Decode a packed repeated int32 field.
getInt32Packed :: Get (Seq Int32)
getInt32Packed = getPacked getInt32


-- | Decode a required int64 field.
getInt64 :: Get Int64
getInt64 = getVarInt


-- | Decode an optional int64 field.
getInt64Opt :: Get (Maybe Int64)
getInt64Opt = getOpt getInt64


-- | Decode a packed repeated int64 field.
getInt64Packed :: Get (Seq Int64)
getInt64Packed = getPacked getInt64


-- | Decode a required message field.
getMessage :: (Default a, Required a, WireMessage a) => Get a
getMessage = do
    bytes <- getBytes
    case runGetOrFail getGroup bytes of
        Left  (_, _, err) -> fail err
        Right (_, _, obj) -> return obj


-- | Decode an optional message field.
getMessageOpt :: (Default a, Required a, WireMessage a) => Get (Maybe a)
getMessageOpt = getOpt getMessage


-- | Decode a required sfixed32 field.
getSFixed32 :: Get Int32
getSFixed32 = fromIntegral <$> getWord32le


-- | Decode an optional sfixed32 field.
getSFixed32Opt :: Get (Maybe Int32)
getSFixed32Opt = getOpt getSFixed32


-- | Decode a packed repeated sfixed32 field.
getSFixed32Packed :: Get (Seq Int32)
getSFixed32Packed = getPacked getSFixed32


-- | Decode a required sfixed64 field.
getSFixed64 :: Get Int64
getSFixed64 = fromIntegral <$> getWord64le


-- | Decode an optional sfixed64 field.
getSFixed64Opt :: Get (Maybe Int64)
getSFixed64Opt = getOpt getSFixed64


-- | Decode a packed repeated sfixed64 field.
getSFixed64Packed :: Get (Seq Int64)
getSFixed64Packed = getPacked getSFixed64


-- | Decode a required sint32 field.
getSInt32 :: Get Int32
getSInt32 = ZZ.decode32 <$> getVarInt


-- | Decode an optional sint32 field.
getSInt32Opt :: Get (Maybe Int32)
getSInt32Opt = getOpt getSInt32


-- | Decode a packed repeated sint32 field.
getSInt32Packed :: Get (Seq Int32)
getSInt32Packed = getPacked getSInt32


-- | Decode a required sint64 field.
getSInt64 :: Get Int64
getSInt64 = ZZ.decode64 <$> getVarInt


-- | Decode an optional sint64 field.
getSInt64Opt :: Get (Maybe Int64)
getSInt64Opt = getOpt getSInt64


-- | Decode a packed repeated sint64 field.
getSInt64Packed :: Get (Seq Int64)
getSInt64Packed = getPacked getSInt64


-- | Decode a required string.
getString :: Get Text
getString = do
    bytes <- getBytes
    case decodeUtf8' bytes of
        Right text -> return text
        Left  err  -> fail  $ "Invalid UTF8: " ++ show err


-- | Decode an optional string.
getStringOpt :: Get (Maybe Text)
getStringOpt = getOpt getString


-- | Decode a required uint32 field.
getUInt32 :: Get Word32
getUInt32 = getVarInt


-- | Decode an optional uint32 field.
getUInt32Opt :: Get (Maybe Word32)
getUInt32Opt = getOpt getUInt32


-- | Decode a packed repeated uint32 field.
getUInt32Packed :: Get (Seq Word32)
getUInt32Packed = getPacked getUInt32


-- | Decode a required uint64 field.
getUInt64 :: Get Word64
getUInt64 = getVarInt


-- | Decode an optional uint64 field.
getUInt64Opt :: Get (Maybe Word64)
getUInt64Opt = getOpt getUInt64


-- | Decode a packed repeated uint64 field.
getUInt64Packed :: Get (Seq Word64)
getUInt64Packed = getPacked getUInt64


-- | Skip an unknown field.
getUnknown :: WireTag -> a -> Get a
getUnknown (WireTag _ VarInt)   a = (getVarInt :: Get Int64) >> return a
getUnknown (WireTag _ Bit64)    a = getWord64le >> return a
getUnknown (WireTag _ LenDelim) a = getBytes    >> return a
getUnknown (WireTag _ Bit32)    a = getWord32le >> return a


-- | Decode a wire tag.
getWireTag :: Get WireTag
getWireTag = do
    int <- getVarInt
    case toWireTag int of
        Right tag -> return tag
        Left  err -> fail err


-- | Encode a required bool field.
putBool :: WireTag -> Bool -> Put
putBool tag val = do
    putWireTag tag
    putVarInt $ boolToWord8 val


-- | Encode a repeated bool field.
putBoolList :: WireTag -> Seq Bool -> Put
putBoolList = putList putBool


-- | Encode an optional bool field.
putBoolOpt :: WireTag -> Maybe Bool -> Put
putBoolOpt = putOpt putBool


-- | Encode a packed repeated bool field.
putBoolPacked :: WireTag -> Seq Bool -> Put
putBoolPacked = putPacked (putVarInt . boolToWord8)


-- | Encode a required bytes field.
putBytes :: WireTag -> ByteString -> Put
putBytes tag bs = do
    putWireTag tag
    putVarInt $ BSL.length bs
    putLazyByteString bs


-- | Encode a repeated bytes field.
putBytesList :: WireTag -> Seq ByteString -> Put
putBytesList = putList putBytes


-- | Encode an optional bytes field.
putBytesOpt :: WireTag -> Maybe ByteString -> Put
putBytesOpt = putOpt putBytes


-- | Encode a required double field.
putDouble :: WireTag -> Double -> Put
putDouble tag d = do
    putWireTag tag
    putFloat64le d


-- | Encode a repeated double field.
putDoubleList :: WireTag -> Seq Double -> Put
putDoubleList = putList putDouble


-- | Encode an optional double field.
putDoubleOpt :: WireTag -> Maybe Double -> Put
putDoubleOpt = putOpt putDouble


-- | Encode a packed repeated double field.
putDoublePacked :: WireTag -> Seq Double -> Put
putDoublePacked = putPacked putFloat64le


-- | Encode a required enum field.
putEnum :: WireEnum a => WireTag -> a -> Put
putEnum tag a = putInt32 tag $ enumToInt a


-- | Encode a repeated enum field.
putEnumList :: WireEnum a => WireTag -> Seq a -> Put
putEnumList = putList putEnum


-- | Encode an optional enum field.
putEnumOpt :: WireEnum a => WireTag -> Maybe a -> Put
putEnumOpt = putOpt putEnum


-- | Encode a packed repeated enum field.
putEnumPacked :: WireEnum a => WireTag -> Seq a -> Put
putEnumPacked = putPacked (putVarInt . enumToInt)


-- | Encode a required fixed32 field.
putFixed32 :: WireTag -> Word32 -> Put
putFixed32 tag f = do
    putWireTag tag
    putWord32le f


-- | Encode a repeated fixed32 field.
putFixed32List :: WireTag -> Seq Word32 -> Put
putFixed32List = putList putFixed32


-- | Encode an optional fixed32 field.
putFixed32Opt :: WireTag -> Maybe Word32 -> Put
putFixed32Opt = putOpt putFixed32


-- | Encode a packed repeated fixed32 field.
putFixed32Packed :: WireTag -> Seq Word32 -> Put
putFixed32Packed = putPacked putWord32le


-- | Encode a required fixed64 field.
putFixed64 :: WireTag -> Word64 -> Put
putFixed64 tag f = do
    putWireTag tag
    putWord64le f


-- | Encode a repeated fixed64 field.
putFixed64List :: WireTag -> Seq Word64 -> Put
putFixed64List = putList putFixed64


-- | Encode an optional fixed64 field.
putFixed64Opt :: WireTag -> Maybe Word64 -> Put
putFixed64Opt = putOpt putFixed64


-- | Encode a packed repeated fixed64 field.
putFixed64Packed :: WireTag -> Seq Word64 -> Put
putFixed64Packed = putPacked putWord64le


-- | Encode a required float field.
putFloat :: WireTag -> Float -> Put
putFloat tag f = do
    putWireTag tag
    putFloat32le f


-- | Encode a repeated float field.
putFloatList :: WireTag -> Seq Float -> Put
putFloatList = putList putFloat


-- | Encode an optional float field.
putFloatOpt :: WireTag -> Maybe Float -> Put
putFloatOpt = putOpt putFloat


-- | Encode a packed repeated float field.
putFloatPacked :: WireTag -> Seq Float -> Put
putFloatPacked = putPacked putFloat32le


-- | Encode a required group field.
putGroup :: WireMessage a => a -> Put
putGroup = messageToFields


-- | Encode an optional group field.
putGroupOpt :: WireMessage a => Maybe a -> Put
putGroupOpt (Just val) = putGroup val
putGroupOpt Nothing    = return ()


-- | Encode a required int32 field.
putInt32 :: WireTag -> Int32 -> Put
putInt32 tag i = do
    putWireTag tag
    putVarInt i


-- | Encode a repeated int32 field.
putInt32List :: WireTag -> Seq Int32 -> Put
putInt32List = putList putInt32


-- | Encode an optional int32 field.
putInt32Opt :: WireTag -> Maybe Int32 -> Put
putInt32Opt = putOpt putInt32


-- | Encode a packed repeated int32 field.
putInt32Packed :: WireTag -> Seq Int32 -> Put
putInt32Packed = putPacked putVarInt


-- | Encode a required int64 field.
putInt64 :: WireTag -> Int64 -> Put
putInt64 tag i = do
    putWireTag tag
    putVarInt i


-- | Encode a repeated int64 field.
putInt64List :: WireTag -> Seq Int64 -> Put
putInt64List = putList putInt64


-- | Encode an optional int64 field.
putInt64Opt :: WireTag -> Maybe Int64 -> Put
putInt64Opt = putOpt putInt64


-- | Encode a packed repeated int64 field.
putInt64Packed :: WireTag -> Seq Int64 -> Put
putInt64Packed = putPacked putVarInt


-- | Encode a required sfixed32 field.
putSFixed32 :: WireTag -> Int32 -> Put
putSFixed32 tag f = do
    putWireTag tag
    putWord32le $ fromIntegral f


-- | Encode a repeated sfixed32 field.
putSFixed32List :: WireTag -> Seq Int32 -> Put
putSFixed32List = putList putSFixed32


-- | Encode an optional sfixed32 field.
putSFixed32Opt :: WireTag -> Maybe Int32 -> Put
putSFixed32Opt = putOpt putSFixed32


-- | Encode a packed repeated sfixed32 field.
putSFixed32Packed :: WireTag -> Seq Int32 -> Put
putSFixed32Packed = putPacked (putWord32le . fromIntegral)


-- | Encode a required sfixed64 field.
putSFixed64 :: WireTag -> Int64 -> Put
putSFixed64 tag f = do
    putWireTag tag
    putWord64le $ fromIntegral f


-- | Encode a repeated sfixed64 field.
putSFixed64List :: WireTag -> Seq Int64 -> Put
putSFixed64List = putList putSFixed64


-- | Encode an optional sfixed64 field.
putSFixed64Opt :: WireTag -> Maybe Int64 -> Put
putSFixed64Opt = putOpt putSFixed64


-- | Encode a packed repeated sfixed64 field.
putSFixed64Packed :: WireTag -> Seq Int64 -> Put
putSFixed64Packed = putPacked (putWord64le . fromIntegral)


-- | Encode a required sint32 field.
putSInt32 :: WireTag -> Int32 -> Put
putSInt32 tag i = do
    putWireTag tag
    putVarInt $ ZZ.encode32 i


-- | Encode a repeated sint32 field.
putSInt32List :: WireTag -> Seq Int32 -> Put
putSInt32List = putList putSInt32


-- | Encode an optional sint32 field.
putSInt32Opt :: WireTag -> Maybe Int32 -> Put
putSInt32Opt = putOpt putSInt32


-- | Encode a packed repeated sint32 field.
putSInt32Packed :: WireTag -> Seq Int32 -> Put
putSInt32Packed = putPacked (putVarInt . ZZ.encode32)


-- | Encode a required sint64 field.
putSInt64 :: WireTag -> Int64 -> Put
putSInt64 tag i = do
    putWireTag tag
    putVarInt $ ZZ.encode64 i


-- | Encode a repeated sint64 field.
putSInt64List :: WireTag -> Seq Int64 -> Put
putSInt64List = putList putSInt64


-- | Encode an optional sint64 field.
putSInt64Opt :: WireTag -> Maybe Int64 -> Put
putSInt64Opt = putOpt putSInt64


-- | Encode a packed repeated sint64 field.
putSInt64Packed :: WireTag -> Seq Int64 -> Put
putSInt64Packed = putPacked (putVarInt . ZZ.encode64)


-- | Encode a required message field.
putMessage :: WireMessage a => WireTag -> a -> Put
putMessage tag a = putBytes tag $ runPut (putGroup a)


-- | Encode a repeated message field.
putMessageList :: WireMessage a => WireTag -> Seq a -> Put
putMessageList = putList putMessage


-- | Encode an optional message field.
putMessageOpt :: WireMessage a => WireTag -> Maybe a -> Put
putMessageOpt = putOpt putMessage


-- | Encode a required string field.
putString :: WireTag -> Text -> Put
putString tag txt = putBytes tag $ encodeUtf8 txt


-- | Encode a repeated string field.
putStringList :: WireTag -> Seq Text -> Put
putStringList = putList putString


-- | Encode an optional string field.
putStringOpt :: WireTag -> Maybe Text -> Put
putStringOpt = putOpt putString


-- | Encode a required uint32 field.
putUInt32 :: WireTag -> Word32 -> Put
putUInt32 tag i = do
    putWireTag tag
    putVarInt i


-- | Encode a repeated uint32 field.
putUInt32List :: WireTag -> Seq Word32 -> Put
putUInt32List = putList putUInt32


-- | Encode an optional uint32 field.
putUInt32Opt :: WireTag -> Maybe Word32 -> Put
putUInt32Opt = putOpt putUInt32


-- | Encode a packed repeated uint32 field.
putUInt32Packed :: WireTag -> Seq Word32 -> Put
putUInt32Packed = putPacked putVarInt


-- | Encode a required uint64 field.
putUInt64 :: WireTag -> Word64 -> Put
putUInt64 tag i = do
    putWireTag tag
    putVarInt i


-- | Encode a repeated uint64 field.
putUInt64List :: WireTag -> Seq Word64 -> Put
putUInt64List = putList putUInt64


-- | Encode an optional uint64 field.
putUInt64Opt :: WireTag -> Maybe Word64 -> Put
putUInt64Opt = putOpt putUInt64


-- | Encode a packed repeated uint64 field.
putUInt64Packed :: WireTag -> Seq Word64 -> Put
putUInt64Packed = putPacked putVarInt


-- | Encode a wire tag.
putWireTag :: WireTag -> Put
putWireTag = putVarInt . fromWireTag



{- Internal -}


getOpt :: Get a -> Get (Maybe a)
getOpt = fmap Just


getPacked :: Get a -> Get (Seq a)
getPacked f = do
    bytes <- getBytes
    case runGetOrFail (loop Seq.empty) bytes of
      Left  (_, _, err) -> fail err
      Right (_, _, obj) -> return obj
    where
      loop xs = do
        done <- isEmpty
        if done
          then return xs
          else do
            val <- f
            loop (xs |> val)


getVarInt :: (Bits a, Integral a) => Get a
getVarInt = getVarIntBytes 0 0


type Shift = Int


getVarIntBytes :: (Bits a, Integral a) => a -> Shift -> Get a
getVarIntBytes prev shift = do
    (isLast, newVal, newShift) <- getVarIntByte prev shift
    if isLast
      then return newVal
      else getVarIntBytes newVal newShift


getVarIntByte :: (Bits a, Integral a) => a -> Shift -> Get (Bool, a, Shift)
getVarIntByte prev shift = do
    byte  <- getWord8
    let isLast = not $ testBit byte 7
    let newVal = prev .|. (fromIntegral (byte .&. 0x7F) `shiftL` shift)
    let newShift = shift + 7
    return (isLast, newVal, newShift)


putVarInt :: (Bits a, Integral a) => a -> Put
putVarInt i
    | i < 0     = putNegVarInt i
    | otherwise = putPosVarInt i


putNegVarInt :: (Bits a, Integral a) => a -> Put
putNegVarInt i = putNegVarInt' i 10


putNegVarInt' :: (Bits a, Integral a) => a -> Int -> Put
putNegVarInt' i 1 = putWord8 $ fromIntegral $ i .&. 1
putNegVarInt' i n = do
    putWord8 (fromIntegral $ setBit (i .&. 0x7F) 7)
    putNegVarInt' (shiftR i 7) (n - 1)


putPosVarInt :: (Bits a, Integral a) => a -> Put
putPosVarInt i
    | i < 128   = putWord8 $ fromIntegral i
    | otherwise = do
        putWord8 (fromIntegral $ setBit (i .&. 0x7F) 7)
        putPosVarInt (shiftR i 7)


putList :: (WireTag -> a -> Put) -> WireTag -> Seq a -> Put
putList f tag = mapM_ (f tag)


putPacked :: (a -> Put) -> WireTag -> Seq a -> Put
putPacked f tag xs
    | len == 0  = return ()
    | otherwise = putBytes tag $ runPut (forM_ xs f)
    where len = Seq.length xs


putOpt :: (WireTag -> a -> Put) -> WireTag -> Maybe a -> Put
putOpt f tag (Just val) = f tag val
putOpt _ _   Nothing    = return ()


boolToWord8 :: Bool -> Word8
boolToWord8 True  = 1
boolToWord8 False = 0
