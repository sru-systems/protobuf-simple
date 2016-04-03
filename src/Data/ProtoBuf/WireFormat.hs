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
import Data.ProtoBuf.Default
import Data.ProtoBuf.FieldNumber
import Data.ProtoBuf.Mergeable
import Data.ProtoBuf.Required
import Data.ProtoBuf.WireEnum
import Data.ProtoBuf.WireMessage
import Data.ProtoBuf.WireTag
import Data.ProtoBuf.WireType
import Data.Sequence (Seq, (|>))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word8, Word32, Word64)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ProtoBuf.ZigZag as ZZ
import qualified Data.Sequence        as Seq
import qualified Data.Set             as Set


decode :: (Default a, Required a, WireMessage a) => ByteString -> Either String a
decode bytes = case runGetOrFail getGroup bytes of
    Left  (_, _, err) -> Left err
    Right (_, _, obj) -> Right obj


encode :: (WireMessage a) => a -> ByteString
encode obj = runPut (putGroup obj)


getBool :: Get Bool
getBool = do
    val <- getVarInt :: Get Word8
    if val == 0
    then return False
    else return True


getBoolOpt :: Get (Maybe Bool)
getBoolOpt = getOpt getBool


getBoolPacked :: Get (Seq Bool)
getBoolPacked = getPacked getBool


getBytes :: Get ByteString
getBytes = do
    len <- getVarInt
    getLazyByteString len


getBytesOpt :: Get (Maybe ByteString)
getBytesOpt = getOpt getBytes


getDouble :: Get Double
getDouble = getFloat64le


getDoubleOpt :: Get (Maybe Double)
getDoubleOpt = getOpt getDouble


getDoublePacked :: Get (Seq Double)
getDoublePacked = getPacked getDouble


getEnum :: (WireEnum a) => Get a
getEnum = intToEnum <$> getInt32


getEnumOpt :: (WireEnum a) => Get (Maybe a)
getEnumOpt = getOpt getEnum


getEnumPacked :: (WireEnum a) => Get (Seq a)
getEnumPacked = getPacked getEnum


getFixed32 :: Get Word32
getFixed32 = getWord32le


getFixed32Opt :: Get (Maybe Word32)
getFixed32Opt = getOpt getFixed32


getFixed32Packed :: Get (Seq Word32)
getFixed32Packed = getPacked getFixed32


getFixed64 :: Get Word64
getFixed64 = getWord64le


getFixed64Opt :: Get (Maybe Word64)
getFixed64Opt = getOpt getFixed64


getFixed64Packed :: Get (Seq Word64)
getFixed64Packed = getPacked getFixed64


getFloat :: Get Float
getFloat = getFloat32le


getFloatOpt :: Get (Maybe Float)
getFloatOpt = getOpt getFloat


getFloatPacked :: Get (Seq Float)
getFloatPacked = getPacked getFloat


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
        loop msg reqs | otherwise = do
            done <- isEmpty
            if done
            then fail "Missing required field(s)"
            else do
                tag <- getWireTag
                msg' <- fieldToValue tag msg
                loop msg' (Set.delete tag reqs)
        defaultMsg = defaultVal
        reqTagSet = reqTags defaultMsg


getGroupOpt :: (Default a, Required a, WireMessage a) => Get (Maybe a)
getGroupOpt = getOpt getGroup


getInt32 :: Get Int32
getInt32 = getVarInt


getInt32Opt :: Get (Maybe Int32)
getInt32Opt = getOpt getInt32


getInt32Packed :: Get (Seq Int32)
getInt32Packed = getPacked getInt32


getInt64 :: Get Int64
getInt64 = getVarInt


getInt64Opt :: Get (Maybe Int64)
getInt64Opt = getOpt getInt64


getInt64Packed :: Get (Seq Int64)
getInt64Packed = getPacked getInt64


getMessage :: (Default a, Required a, WireMessage a) => Get a
getMessage = do
    bytes <- getBytes
    case runGetOrFail getGroup bytes of
        Left  (_, _, err) -> fail err
        Right (_, _, obj) -> return obj


getMessageOpt :: (Default a, Required a, WireMessage a) => Get (Maybe a)
getMessageOpt = getOpt getMessage


getSFixed32 :: Get Int32
getSFixed32 = fromIntegral <$> getWord32le


getSFixed32Opt :: Get (Maybe Int32)
getSFixed32Opt = getOpt getSFixed32


getSFixed32Packed :: Get (Seq Int32)
getSFixed32Packed = getPacked getSFixed32


getSFixed64 :: Get Int64
getSFixed64 = fromIntegral <$> getWord64le


getSFixed64Opt :: Get (Maybe Int64)
getSFixed64Opt = getOpt getSFixed64


getSFixed64Packed :: Get (Seq Int64)
getSFixed64Packed = getPacked getSFixed64


getSInt32 :: Get Int32
getSInt32 = ZZ.decode32 <$> getVarInt


getSInt32Opt :: Get (Maybe Int32)
getSInt32Opt = getOpt getSInt32


getSInt32Packed :: Get (Seq Int32)
getSInt32Packed = getPacked getSInt32


getSInt64 :: Get Int64
getSInt64 = ZZ.decode64 <$> getVarInt


getSInt64Opt :: Get (Maybe Int64)
getSInt64Opt = getOpt getSInt64


getSInt64Packed :: Get (Seq Int64)
getSInt64Packed = getPacked getSInt64


getString :: Get Text
getString = do
    bytes <- getBytes
    case decodeUtf8' bytes of
        Right text -> return text
        Left  err  -> fail  $ "Invalid UTF8: " ++ show err


getStringOpt :: Get (Maybe Text)
getStringOpt = getOpt getString


getUInt32 :: Get Word32
getUInt32 = getVarInt


getUInt32Opt :: Get (Maybe Word32)
getUInt32Opt = getOpt getUInt32


getUInt32Packed :: Get (Seq Word32)
getUInt32Packed = getPacked getUInt32


getUInt64 :: Get Word64
getUInt64 = getVarInt


getUInt64Opt :: Get (Maybe Word64)
getUInt64Opt = getOpt getUInt64


getUInt64Packed :: Get (Seq Word64)
getUInt64Packed = getPacked getUInt64


getUnknown :: WireTag -> a -> Get a
getUnknown (WireTag _ VarInt)   a = (getVarInt :: Get Int64) >> return a
getUnknown (WireTag _ Bit64)    a = getWord64le >> return a
getUnknown (WireTag _ LenDelim) a = getBytes    >> return a
getUnknown (WireTag _ Bit32)    a = getWord32le >> return a


getWireTag :: Get WireTag
getWireTag = do
    int <- getVarInt
    case toWireTag int of
        Right tag -> return tag
        Left  err -> fail err


putBool :: WireTag -> Bool -> Put
putBool tag val = do
    putWireTag tag
    putVarInt $ boolToWord8 val


putBoolList :: WireTag -> Seq Bool -> Put
putBoolList = putList putBool


putBoolOpt :: WireTag -> Maybe Bool -> Put
putBoolOpt = putOpt putBool


putBoolPacked :: WireTag -> Seq Bool -> Put
putBoolPacked = putPacked (putVarInt . boolToWord8)


putBytes :: WireTag -> ByteString -> Put
putBytes tag bs = do
    putWireTag tag
    putVarInt $ BSL.length bs
    putLazyByteString bs


putBytesList :: WireTag -> Seq ByteString -> Put
putBytesList = putList putBytes


putBytesOpt :: WireTag -> Maybe ByteString -> Put
putBytesOpt = putOpt putBytes


putDouble :: WireTag -> Double -> Put
putDouble tag d = do
    putWireTag tag
    putFloat64le d


putDoubleList :: WireTag -> Seq Double -> Put
putDoubleList = putList putDouble


putDoubleOpt :: WireTag -> Maybe Double -> Put
putDoubleOpt = putOpt putDouble


putDoublePacked :: WireTag -> Seq Double -> Put
putDoublePacked = putPacked putFloat64le


putEnum :: WireEnum a => WireTag -> a -> Put
putEnum tag a = putInt32 tag $ enumToInt a


putEnumList :: WireEnum a => WireTag -> Seq a -> Put
putEnumList = putList putEnum


putEnumOpt :: WireEnum a => WireTag -> Maybe a -> Put
putEnumOpt = putOpt putEnum


putEnumPacked :: WireEnum a => WireTag -> Seq a -> Put
putEnumPacked = putPacked (putVarInt . enumToInt)


putFixed32 :: WireTag -> Word32 -> Put
putFixed32 tag f = do
    putWireTag tag
    putWord32le f


putFixed32List :: WireTag -> Seq Word32 -> Put
putFixed32List = putList putFixed32


putFixed32Opt :: WireTag -> Maybe Word32 -> Put
putFixed32Opt = putOpt putFixed32


putFixed32Packed :: WireTag -> Seq Word32 -> Put
putFixed32Packed = putPacked putWord32le


putFixed64 :: WireTag -> Word64 -> Put
putFixed64 tag f = do
    putWireTag tag
    putWord64le f


putFixed64List :: WireTag -> Seq Word64 -> Put
putFixed64List = putList putFixed64


putFixed64Opt :: WireTag -> Maybe Word64 -> Put
putFixed64Opt = putOpt putFixed64


putFixed64Packed :: WireTag -> Seq Word64 -> Put
putFixed64Packed = putPacked putWord64le


putFloat :: WireTag -> Float -> Put
putFloat tag f = do
    putWireTag tag
    putFloat32le f


putFloatList :: WireTag -> Seq Float -> Put
putFloatList = putList putFloat


putFloatOpt :: WireTag -> Maybe Float -> Put
putFloatOpt = putOpt putFloat


putFloatPacked :: WireTag -> Seq Float -> Put
putFloatPacked = putPacked putFloat32le


putGroup :: WireMessage a => a -> Put
putGroup = messageToFields


putGroupOpt :: WireMessage a => Maybe a -> Put
putGroupOpt (Just val) = putGroup val
putGroupOpt Nothing    = return ()


putInt32 :: WireTag -> Int32 -> Put
putInt32 tag i = do
    putWireTag tag
    putVarInt i


putInt32List :: WireTag -> Seq Int32 -> Put
putInt32List = putList putInt32


putInt32Opt :: WireTag -> Maybe Int32 -> Put
putInt32Opt = putOpt putInt32


putInt32Packed :: WireTag -> Seq Int32 -> Put
putInt32Packed = putPacked putVarInt


putInt64 :: WireTag -> Int64 -> Put
putInt64 tag i = do
    putWireTag tag
    putVarInt i


putInt64List :: WireTag -> Seq Int64 -> Put
putInt64List = putList putInt64


putInt64Opt :: WireTag -> Maybe Int64 -> Put
putInt64Opt = putOpt putInt64


putInt64Packed :: WireTag -> Seq Int64 -> Put
putInt64Packed = putPacked putVarInt


putSFixed32 :: WireTag -> Int32 -> Put
putSFixed32 tag f = do
    putWireTag tag
    putWord32le $ fromIntegral f


putSFixed32List :: WireTag -> Seq Int32 -> Put
putSFixed32List = putList putSFixed32


putSFixed32Opt :: WireTag -> Maybe Int32 -> Put
putSFixed32Opt = putOpt putSFixed32


putSFixed32Packed :: WireTag -> Seq Int32 -> Put
putSFixed32Packed = putPacked (putWord32le . fromIntegral)


putSFixed64 :: WireTag -> Int64 -> Put
putSFixed64 tag f = do
    putWireTag tag
    putWord64le $ fromIntegral f


putSFixed64List :: WireTag -> Seq Int64 -> Put
putSFixed64List = putList putSFixed64


putSFixed64Opt :: WireTag -> Maybe Int64 -> Put
putSFixed64Opt = putOpt putSFixed64


putSFixed64Packed :: WireTag -> Seq Int64 -> Put
putSFixed64Packed = putPacked (putWord64le . fromIntegral)


putSInt32 :: WireTag -> Int32 -> Put
putSInt32 tag i = do
    putWireTag tag
    putVarInt $ ZZ.encode32 i


putSInt32List :: WireTag -> Seq Int32 -> Put
putSInt32List = putList putSInt32


putSInt32Opt :: WireTag -> Maybe Int32 -> Put
putSInt32Opt = putOpt putSInt32


putSInt32Packed :: WireTag -> Seq Int32 -> Put
putSInt32Packed = putPacked (putVarInt . ZZ.encode32)


putSInt64 :: WireTag -> Int64 -> Put
putSInt64 tag i = do
    putWireTag tag
    putVarInt $ ZZ.encode64 i


putSInt64List :: WireTag -> Seq Int64 -> Put
putSInt64List = putList putSInt64


putSInt64Opt :: WireTag -> Maybe Int64 -> Put
putSInt64Opt = putOpt putSInt64


putSInt64Packed :: WireTag -> Seq Int64 -> Put
putSInt64Packed = putPacked (putVarInt . ZZ.encode64)


putMessage :: WireMessage a => WireTag -> a -> Put
putMessage tag a = putBytes tag $ runPut (putGroup a)


putMessageList :: WireMessage a => WireTag -> Seq a -> Put
putMessageList = putList putMessage


putMessageOpt :: WireMessage a => WireTag -> Maybe a -> Put
putMessageOpt = putOpt putMessage


putString :: WireTag -> Text -> Put
putString tag txt = putBytes tag $ encodeUtf8 txt


putStringList :: WireTag -> Seq Text -> Put
putStringList = putList putString


putStringOpt :: WireTag -> Maybe Text -> Put
putStringOpt = putOpt putString


putUInt32 :: WireTag -> Word32 -> Put
putUInt32 tag i = do
    putWireTag tag
    putVarInt i


putUInt32List :: WireTag -> Seq Word32 -> Put
putUInt32List = putList putUInt32


putUInt32Opt :: WireTag -> Maybe Word32 -> Put
putUInt32Opt = putOpt putUInt32


putUInt32Packed :: WireTag -> Seq Word32 -> Put
putUInt32Packed = putPacked putVarInt


putUInt64 :: WireTag -> Word64 -> Put
putUInt64 tag i = do
    putWireTag tag
    putVarInt i


putUInt64List :: WireTag -> Seq Word64 -> Put
putUInt64List = putList putUInt64


putUInt64Opt :: WireTag -> Maybe Word64 -> Put
putUInt64Opt = putOpt putUInt64


putUInt64Packed :: WireTag -> Seq Word64 -> Put
putUInt64Packed = putPacked putVarInt


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
