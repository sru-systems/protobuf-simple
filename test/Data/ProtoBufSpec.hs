{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.ProtoBufSpec
    ( main
    , spec
    ) where

import Data.ByteString.Lazy (ByteString, pack, readFile)
import Data.ProtoBuf (decode, encode)
import Prelude hiding (Enum, readFile)
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Types.BoolList (BoolList(..))
import Types.BoolListPacked (BoolListPacked(..))
import Types.BoolMsg (BoolMsg(..))
import Types.BoolOptMsg (BoolOptMsg(..))

import Types.BytesList (BytesList(..))
import Types.BytesMsg (BytesMsg(..))
import Types.BytesOptMsg (BytesOptMsg(..))

import Types.DoubleList (DoubleList(..))
import Types.DoubleListPacked (DoubleListPacked(..))
import Types.DoubleMsg (DoubleMsg(..))
import Types.DoubleOptMsg (DoubleOptMsg(..))

import Types.Enum (Enum(..))
import Types.EnumList (EnumList(..))
import Types.EnumListPacked (EnumListPacked(..))
import Types.EnumMsg (EnumMsg(..))
import Types.EnumOptMsg (EnumOptMsg(..))

import Types.Fixed32List (Fixed32List(..))
import Types.Fixed32ListPacked (Fixed32ListPacked(..))
import Types.Fixed32Msg (Fixed32Msg(..))
import Types.Fixed32OptMsg (Fixed32OptMsg(..))

import Types.Fixed64List (Fixed64List(..))
import Types.Fixed64ListPacked (Fixed64ListPacked(..))
import Types.Fixed64Msg (Fixed64Msg(..))
import Types.Fixed64OptMsg (Fixed64OptMsg(..))

import Types.FloatList (FloatList(..))
import Types.FloatListPacked (FloatListPacked(..))
import Types.FloatMsg (FloatMsg(..))
import Types.FloatOptMsg (FloatOptMsg(..))

import Types.Int32List (Int32List(..))
import Types.Int32ListPacked (Int32ListPacked(..))
import Types.Int32Msg (Int32Msg(..))
import Types.Int32OptMsg (Int32OptMsg(..))

import Types.Int64List (Int64List(..))
import Types.Int64ListPacked (Int64ListPacked(..))
import Types.Int64Msg (Int64Msg(..))
import Types.Int64OptMsg (Int64OptMsg(..))

import Types.Message (Message(..))
import Types.MessageList (MessageList(..))
import Types.MessageMsg (MessageMsg(..))
import Types.MessageOptMsg (MessageOptMsg(..))

import Types.SFixed32List (SFixed32List(..))
import Types.SFixed32ListPacked (SFixed32ListPacked(..))
import Types.SFixed32Msg (SFixed32Msg(..))
import Types.SFixed32OptMsg (SFixed32OptMsg(..))

import Types.SFixed64List (SFixed64List(..))
import Types.SFixed64ListPacked (SFixed64ListPacked(..))
import Types.SFixed64Msg (SFixed64Msg(..))
import Types.SFixed64OptMsg (SFixed64OptMsg(..))

import Types.SInt32List (SInt32List(..))
import Types.SInt32ListPacked (SInt32ListPacked(..))
import Types.SInt32Msg (SInt32Msg(..))
import Types.SInt32OptMsg (SInt32OptMsg(..))

import Types.SInt64List (SInt64List(..))
import Types.SInt64ListPacked (SInt64ListPacked(..))
import Types.SInt64Msg (SInt64Msg(..))
import Types.SInt64OptMsg (SInt64OptMsg(..))

import Types.StringList (StringList(..))
import Types.StringMsg (StringMsg(..))
import Types.StringOptMsg (StringOptMsg(..))

import Types.UInt32List (UInt32List(..))
import Types.UInt32ListPacked (UInt32ListPacked(..))
import Types.UInt32Msg (UInt32Msg(..))
import Types.UInt32OptMsg (UInt32OptMsg(..))

import Types.UInt64List (UInt64List(..))
import Types.UInt64ListPacked (UInt64ListPacked(..))
import Types.UInt64Msg (UInt64Msg(..))
import Types.UInt64OptMsg (UInt64OptMsg(..))

import qualified Data.ProtoBufInt  as PB
import qualified Data.Sequence     as Seq


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Bool" $ do
    typeTests boolList "BoolList"
    typeTests boolListPacked "BoolListPacked"
    typeTests boolMsg "BoolMsg"
    typeTests boolOptMsg "BoolOptMsg"

  describe "Bytes" $ do
    typeTests bytesList "BytesList"
    typeTests bytesMsg "BytesMsg"
    typeTests bytesOptMsg "BytesOptMsg"

  describe "Double" $ do
    typeTests doubleList "DoubleList"
    typeTests doubleListPacked "DoubleListPacked"
    typeTests doubleMsg "DoubleMsg"
    typeTests doubleOptMsg "DoubleOptMsg"

  describe "Enum" $ do
    typeTests enumList "EnumList"
    typeTests enumListPacked "EnumListPacked"
    typeTests enumMsg "EnumMsg"
    typeTests enumOptMsg "EnumOptMsg"

  describe "Fixed32" $ do
    typeTests fixed32List "Fixed32List"
    typeTests fixed32ListPacked "Fixed32ListPacked"
    typeTests fixed32Msg "Fixed32Msg"
    typeTests fixed32OptMsg "Fixed32OptMsg"

  describe "Fixed64" $ do
    typeTests fixed64List "Fixed64List"
    typeTests fixed64ListPacked "Fixed64ListPacked"
    typeTests fixed64Msg "Fixed64Msg"
    typeTests fixed64OptMsg "Fixed64OptMsg"

  describe "Float" $ do
    typeTests floatList "FloatList"
    typeTests floatListPacked "FloatListPacked"
    typeTests floatMsg "FloatMsg"
    typeTests floatOptMsg "FloatOptMsg"

  describe "Int32" $ do
    typeTests int32List "Int32List"
    typeTests int32ListPacked "Int32ListPacked"
    typeTests int32Msg "Int32Msg"
    typeTests int32OptMsg "Int32OptMsg"

  describe "Int64" $ do
    typeTests int64List "Int64List"
    typeTests int64ListPacked "Int64ListPacked"
    typeTests int64Msg "Int64Msg"
    typeTests int64OptMsg "Int64OptMsg"

  describe "Message" $ do
    typeTests messageList "MessageList"
    typeTests messageMsg "MessageMsg"
    typeTests messageOptMsg "MessageOptMsg"

  describe "SFixed32" $ do
    typeTests sfixed32List "SFixed32List"
    typeTests sfixed32ListPacked "SFixed32ListPacked"
    typeTests sfixed32Msg "SFixed32Msg"
    typeTests sfixed32OptMsg "SFixed32OptMsg"

  describe "SFixed64" $ do
    typeTests sfixed64List "SFixed64List"
    typeTests sfixed64ListPacked "SFixed64ListPacked"
    typeTests sfixed64Msg "SFixed64Msg"
    typeTests sfixed64OptMsg "SFixed64OptMsg"

  describe "SInt32" $ do
    typeTests sint32List "SInt32List"
    typeTests sint32ListPacked "SInt32ListPacked"
    typeTests sint32Msg "SInt32Msg"
    typeTests sint32OptMsg "SInt32OptMsg"

  describe "SInt64" $ do
    typeTests sint64List "SInt64List"
    typeTests sint64ListPacked "SInt64ListPacked"
    typeTests sint64Msg "SInt64Msg"
    typeTests sint64OptMsg "SInt64OptMsg"

  describe "String" $ do
    typeTests stringList "StringList"
    typeTests stringMsg "StringMsg"
    typeTests stringOptMsg "StringOptMsg"

  describe "UInt32" $ do
    typeTests uint32List "UInt32List"
    typeTests uint32ListPacked "UInt32ListPacked"
    typeTests uint32Msg "UInt32Msg"
    typeTests uint32OptMsg "UInt32OptMsg"

  describe "UInt64" $ do
    typeTests uint64List "UInt64List"
    typeTests uint64ListPacked "UInt64ListPacked"
    typeTests uint64Msg "UInt64Msg"
    typeTests uint64OptMsg "UInt64OptMsg"


typeTests :: (Eq a, Show a, PB.Required a, PB.WireMessage a, PB.Default a, Arbitrary a) => a -> String -> SpecWith()
typeTests t name =
    context name $ do
      it "decode inverses encode" $ property $
        \x -> (decode . encode) x == Right (x `asTypeOf` t)

      it ("decode " ++ filename ++ " returns correct data") $ do
        file <- readBinFile filename
        decode file `shouldBe` Right t

      it ("encode " ++ name ++ " returns correct data") $ do
        file <- readBinFile filename
        encode t `shouldBe` file
  where
    filename = name ++ ".bin"


readBinFile :: String -> IO ByteString
readBinFile file = readFile ("data" </> file)


-- Bool

boolList :: BoolList
boolList = BoolList (Seq.fromList [False, True])

boolListPacked :: BoolListPacked
boolListPacked = BoolListPacked (Seq.fromList [False, True])

boolMsg :: BoolMsg
boolMsg = BoolMsg True

boolOptMsg :: BoolOptMsg
boolOptMsg = BoolOptMsg Nothing


instance Arbitrary BoolList where
  arbitrary = BoolList <$> arbitrary

instance Arbitrary BoolListPacked where
  arbitrary = BoolListPacked <$> arbitrary

instance Arbitrary BoolMsg where
  arbitrary = BoolMsg <$> arbitrary

instance Arbitrary BoolOptMsg where
  arbitrary = BoolOptMsg <$> arbitrary


-- Bytes

bytesList :: BytesList
bytesList = BytesList (Seq.fromList [pack [0,1], pack [127,255]])

bytesMsg :: BytesMsg
bytesMsg = BytesMsg (pack [0,1,255])

bytesOptMsg :: BytesOptMsg
bytesOptMsg = BytesOptMsg Nothing


instance Arbitrary BytesList where
  arbitrary = BytesList <$> arbitrary

instance Arbitrary BytesMsg where
  arbitrary = BytesMsg <$> arbitrary

instance Arbitrary BytesOptMsg where
  arbitrary = BytesOptMsg <$> arbitrary


-- Double

doubleList :: DoubleList
doubleList = DoubleList (Seq.fromList [-10.5, -1, 0, 1, 10.5])

doubleListPacked :: DoubleListPacked
doubleListPacked = DoubleListPacked (Seq.fromList [-10.5, -1, 0, 1, 10.5])

doubleMsg :: DoubleMsg
doubleMsg = DoubleMsg 1.1

doubleOptMsg :: DoubleOptMsg
doubleOptMsg = DoubleOptMsg Nothing


instance Arbitrary DoubleList where
  arbitrary = DoubleList <$> arbitrary

instance Arbitrary DoubleListPacked where
  arbitrary = DoubleListPacked <$> arbitrary

instance Arbitrary DoubleMsg where
  arbitrary = DoubleMsg <$> arbitrary

instance Arbitrary DoubleOptMsg where
  arbitrary = DoubleOptMsg <$> arbitrary


-- Enum

enumList :: EnumList
enumList = EnumList (Seq.fromList [Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine])

enumListPacked :: EnumListPacked
enumListPacked = EnumListPacked (Seq.fromList [Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine])

enumMsg :: EnumMsg
enumMsg = EnumMsg Nine

enumOptMsg :: EnumOptMsg
enumOptMsg = EnumOptMsg Nothing


instance Arbitrary Enum where
  arbitrary = elements [Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

instance Arbitrary EnumList where
  arbitrary = EnumList <$> arbitrary

instance Arbitrary EnumListPacked where
  arbitrary = EnumListPacked <$> arbitrary

instance Arbitrary EnumMsg where
  arbitrary = EnumMsg <$> arbitrary

instance Arbitrary EnumOptMsg where
  arbitrary = EnumOptMsg <$> arbitrary


-- Fixed32

fixed32List :: Fixed32List
fixed32List = Fixed32List (Seq.fromList [0, 1, 3000000000])

fixed32ListPacked :: Fixed32ListPacked
fixed32ListPacked = Fixed32ListPacked (Seq.fromList [0, 1, 3000000000])

fixed32Msg :: Fixed32Msg
fixed32Msg = Fixed32Msg 0

fixed32OptMsg :: Fixed32OptMsg
fixed32OptMsg = Fixed32OptMsg Nothing


instance Arbitrary Fixed32List where
  arbitrary = Fixed32List <$> arbitrary

instance Arbitrary Fixed32ListPacked where
  arbitrary = Fixed32ListPacked <$> arbitrary

instance Arbitrary Fixed32Msg where
  arbitrary = Fixed32Msg <$> arbitrary

instance Arbitrary Fixed32OptMsg where
  arbitrary = Fixed32OptMsg <$> arbitrary


-- Fixed64

fixed64List :: Fixed64List
fixed64List = Fixed64List (Seq.fromList [0, 1, 9000000000])

fixed64ListPacked :: Fixed64ListPacked
fixed64ListPacked = Fixed64ListPacked (Seq.fromList [0, 1, 9000000000])

fixed64Msg :: Fixed64Msg
fixed64Msg = Fixed64Msg 0

fixed64OptMsg :: Fixed64OptMsg
fixed64OptMsg = Fixed64OptMsg Nothing


instance Arbitrary Fixed64List where
  arbitrary = Fixed64List <$> arbitrary

instance Arbitrary Fixed64ListPacked where
  arbitrary = Fixed64ListPacked <$> arbitrary

instance Arbitrary Fixed64Msg where
  arbitrary = Fixed64Msg <$> arbitrary

instance Arbitrary Fixed64OptMsg where
  arbitrary = Fixed64OptMsg <$> arbitrary


-- Float

floatList :: FloatList
floatList = FloatList (Seq.fromList [-10.5, -1, 0, 1, 10.5])

floatListPacked :: FloatListPacked
floatListPacked = FloatListPacked (Seq.fromList [-10.5, -1, 0, 1, 10.5])

floatMsg :: FloatMsg
floatMsg = FloatMsg 1.1

floatOptMsg :: FloatOptMsg
floatOptMsg = FloatOptMsg Nothing


instance Arbitrary FloatList where
  arbitrary = FloatList <$> arbitrary

instance Arbitrary FloatListPacked where
  arbitrary = FloatListPacked <$> arbitrary

instance Arbitrary FloatMsg where
  arbitrary = FloatMsg <$> arbitrary

instance Arbitrary FloatOptMsg where
  arbitrary = FloatOptMsg <$> arbitrary


-- Int32

int32List :: Int32List
int32List = Int32List (Seq.fromList [-2000000000, -1, 0, 1, 2000000000])

int32ListPacked :: Int32ListPacked
int32ListPacked = Int32ListPacked (Seq.fromList [-2000000000, -1, 0, 1, 2000000000])

int32Msg :: Int32Msg
int32Msg = Int32Msg 1

int32OptMsg :: Int32OptMsg
int32OptMsg = Int32OptMsg Nothing


instance Arbitrary Int32List where
  arbitrary = Int32List <$> arbitrary

instance Arbitrary Int32ListPacked where
  arbitrary = Int32ListPacked <$> arbitrary

instance Arbitrary Int32Msg where
  arbitrary = Int32Msg <$> arbitrary

instance Arbitrary Int32OptMsg where
  arbitrary = Int32OptMsg <$> arbitrary


-- Int64

int64List :: Int64List
int64List = Int64List (Seq.fromList [-8000000000, -1, 0, 1, 8000000000])

int64ListPacked :: Int64ListPacked
int64ListPacked = Int64ListPacked (Seq.fromList [-8000000000, -1, 0, 1, 8000000000])

int64Msg :: Int64Msg
int64Msg = Int64Msg 1

int64OptMsg :: Int64OptMsg
int64OptMsg = Int64OptMsg Nothing


instance Arbitrary Int64List where
  arbitrary = Int64List <$> arbitrary

instance Arbitrary Int64ListPacked where
  arbitrary = Int64ListPacked <$> arbitrary

instance Arbitrary Int64Msg where
  arbitrary = Int64Msg <$> arbitrary

instance Arbitrary Int64OptMsg where
  arbitrary = Int64OptMsg <$> arbitrary


-- Message

message :: Message
message = Message False bs is
  where
    bs = Just $ pack [1,2]
    is = Seq.singleton int32Msg

messageList :: MessageList
messageList = MessageList (Seq.singleton message)

messageMsg :: MessageMsg
messageMsg = MessageMsg message

messageOptMsg :: MessageOptMsg
messageOptMsg = MessageOptMsg Nothing


instance Arbitrary Message where
  arbitrary = Message <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary MessageList where
  arbitrary = MessageList <$> arbitrary

instance Arbitrary MessageMsg where
  arbitrary = MessageMsg <$> arbitrary

instance Arbitrary MessageOptMsg where
  arbitrary = MessageOptMsg <$> arbitrary


-- SFixed32

sfixed32List :: SFixed32List
sfixed32List = SFixed32List (Seq.fromList [-2000000000, -1, 0, 1, 2000000000])

sfixed32ListPacked :: SFixed32ListPacked
sfixed32ListPacked = SFixed32ListPacked (Seq.fromList [-2000000000, -1, 0, 1, 2000000000])

sfixed32Msg :: SFixed32Msg
sfixed32Msg = SFixed32Msg 0

sfixed32OptMsg :: SFixed32OptMsg
sfixed32OptMsg = SFixed32OptMsg Nothing


instance Arbitrary SFixed32List where
  arbitrary = SFixed32List <$> arbitrary

instance Arbitrary SFixed32ListPacked where
  arbitrary = SFixed32ListPacked <$> arbitrary

instance Arbitrary SFixed32Msg where
  arbitrary = SFixed32Msg <$> arbitrary

instance Arbitrary SFixed32OptMsg where
  arbitrary = SFixed32OptMsg <$> arbitrary


-- SFixed64

sfixed64List :: SFixed64List
sfixed64List = SFixed64List (Seq.fromList [-9000000000, -1, 0, 1, 9000000000])

sfixed64ListPacked :: SFixed64ListPacked
sfixed64ListPacked = SFixed64ListPacked (Seq.fromList [-9000000000, -1, 0, 1, 9000000000])

sfixed64Msg :: SFixed64Msg
sfixed64Msg = SFixed64Msg 0

sfixed64OptMsg :: SFixed64OptMsg
sfixed64OptMsg = SFixed64OptMsg Nothing


instance Arbitrary SFixed64List where
  arbitrary = SFixed64List <$> arbitrary

instance Arbitrary SFixed64ListPacked where
  arbitrary = SFixed64ListPacked <$> arbitrary

instance Arbitrary SFixed64Msg where
  arbitrary = SFixed64Msg <$> arbitrary

instance Arbitrary SFixed64OptMsg where
  arbitrary = SFixed64OptMsg <$> arbitrary


-- SInt32

sint32List :: SInt32List
sint32List = SInt32List (Seq.fromList [-2000000000, -1, 0, 1, 2000000000])

sint32ListPacked :: SInt32ListPacked
sint32ListPacked = SInt32ListPacked (Seq.fromList [-2000000000, -1, 0, 1, 2000000000])

sint32Msg :: SInt32Msg
sint32Msg = SInt32Msg 1

sint32OptMsg :: SInt32OptMsg
sint32OptMsg = SInt32OptMsg Nothing


instance Arbitrary SInt32List where
  arbitrary = SInt32List <$> arbitrary

instance Arbitrary SInt32ListPacked where
  arbitrary = SInt32ListPacked <$> arbitrary

instance Arbitrary SInt32Msg where
  arbitrary = SInt32Msg <$> arbitrary

instance Arbitrary SInt32OptMsg where
  arbitrary = SInt32OptMsg <$> arbitrary


-- SInt64

sint64List :: SInt64List
sint64List = SInt64List (Seq.fromList [-8000000000, -1, 0, 1, 8000000000])

sint64ListPacked :: SInt64ListPacked
sint64ListPacked = SInt64ListPacked (Seq.fromList [-8000000000, -1, 0, 1, 8000000000])

sint64Msg :: SInt64Msg
sint64Msg = SInt64Msg 1

sint64OptMsg :: SInt64OptMsg
sint64OptMsg = SInt64OptMsg Nothing


instance Arbitrary SInt64List where
  arbitrary = SInt64List <$> arbitrary

instance Arbitrary SInt64ListPacked where
  arbitrary = SInt64ListPacked <$> arbitrary

instance Arbitrary SInt64Msg where
  arbitrary = SInt64Msg <$> arbitrary

instance Arbitrary SInt64OptMsg where
  arbitrary = SInt64OptMsg <$> arbitrary


-- String

stringList :: StringList
stringList = StringList (Seq.fromList ["Foo", "Bar", "Baz"])

stringMsg :: StringMsg
stringMsg = StringMsg "Foo"

stringOptMsg :: StringOptMsg
stringOptMsg = StringOptMsg Nothing


instance Arbitrary StringList where
  arbitrary = StringList <$> arbitrary

instance Arbitrary StringMsg where
  arbitrary = StringMsg <$> arbitrary

instance Arbitrary StringOptMsg where
  arbitrary = StringOptMsg <$> arbitrary


-- UInt32

uint32List :: UInt32List
uint32List = UInt32List (Seq.fromList [0, 1, 3000000000])

uint32ListPacked :: UInt32ListPacked
uint32ListPacked = UInt32ListPacked (Seq.fromList [0, 1, 3000000000])

uint32Msg :: UInt32Msg
uint32Msg = UInt32Msg 0

uint32OptMsg :: UInt32OptMsg
uint32OptMsg = UInt32OptMsg Nothing


instance Arbitrary UInt32List where
  arbitrary = UInt32List <$> arbitrary

instance Arbitrary UInt32ListPacked where
  arbitrary = UInt32ListPacked <$> arbitrary

instance Arbitrary UInt32Msg where
  arbitrary = UInt32Msg <$> arbitrary

instance Arbitrary UInt32OptMsg where
  arbitrary = UInt32OptMsg <$> arbitrary


-- UInt64

uint64List :: UInt64List
uint64List = UInt64List (Seq.fromList [0, 1, 9000000000])

uint64ListPacked :: UInt64ListPacked
uint64ListPacked = UInt64ListPacked (Seq.fromList [0, 1, 9000000000])

uint64Msg :: UInt64Msg
uint64Msg = UInt64Msg 0

uint64OptMsg :: UInt64OptMsg
uint64OptMsg = UInt64OptMsg Nothing


instance Arbitrary UInt64List where
  arbitrary = UInt64List <$> arbitrary

instance Arbitrary UInt64ListPacked where
  arbitrary = UInt64ListPacked <$> arbitrary

instance Arbitrary UInt64Msg where
  arbitrary = UInt64Msg <$> arbitrary

instance Arbitrary UInt64OptMsg where
  arbitrary = UInt64OptMsg <$> arbitrary
