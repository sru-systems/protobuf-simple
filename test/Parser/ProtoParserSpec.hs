module Parser.ProtoParserSpec
    ( main
    , spec
    ) where

import Parser.ProtoParser  (parseProto)
import Test.Hspec (Spec, hspec, describe, it, shouldBe, Expectation)

import qualified Parser.EnumDesc      as EnumDesc
import qualified Parser.EnumValueDesc as EnumValueDesc
import qualified Parser.FieldDesc     as FieldDesc
import qualified Parser.FileDesc      as FileDesc
import qualified Parser.Label         as Label
import qualified Parser.MessageDesc   as MessageDesc

main :: IO ()
main = hspec spec


spec :: Spec
spec = describe "parseProto" $ do

    it "sets the file name" $
        parseProto "misc/Filename.proto" ""
        `shouldParse`
        FileDesc.new "Filename"


    it "parses empty input" $
        parseProto "Test.proto" ""
        `shouldParse`
        FileDesc.new "Test"


    it "parses leading comments" $
        parseProto "Test.proto" "// Comment test"
        `shouldParse`
        FileDesc.new "Test"


    it "parses block comments" $
        parseProto "Test.proto" (unlines
          [ "/* Line 1 "
          , " * Line 2 "
          , " */"
          ])
        `shouldParse`
        FileDesc.new "Test"


    it "parses syntax statement" $
        parseProto "Test.proto" "syntax = \"proto2\";"
        `shouldParse`
        FileDesc.new "Test"


    it "parses package name" $
        parseProto "Test.proto" "package google.protoBuf;"
        `shouldParse`
        FileDesc.setPackage "Google.ProtoBuf" (FileDesc.new "Test")


    it "ignores file option: cc_generic_services" $
        parseProto "Test.proto" "option cc_generic_services = True;"
        `shouldParse`
        FileDesc.new "Test"


    it "ignores file option: deprecated" $
        parseProto "Test.proto" "option deprecated = TRUE;"
        `shouldParse`
        FileDesc.new "Test"


    it "ignores file option: go_package" $
        parseProto "Test.proto" "option go_package = \"Google.ProtoBuf\";"
        `shouldParse`
        FileDesc.new "Test"


    it "ignores file option: java_generate_equals_and_hash" $
        parseProto "Test.proto" "option java_generate_equals_and_hash = true;"
        `shouldParse`
        FileDesc.new "Test"


    it "ignores file option: java_generic_services" $
        parseProto "Test.proto" "option java_generic_services = False;"
        `shouldParse`
        FileDesc.new "Test"


    it "ignores file option: java_multiple_files" $
        parseProto "Test.proto" "option java_multiple_files = FALSE;"
        `shouldParse`
        FileDesc.new "Test"


    it "ignores file option: java_outer_classname" $
        parseProto "Test.proto" "option java_outer_classname = \"Parser\";"
        `shouldParse`
        FileDesc.new "Test"


    it "ignores file option: java_package" $
        parseProto "Test.proto" "option java_package = \"com.google.protobuf\";"
        `shouldParse`
        FileDesc.new "Test"


    it "ignores file option: java_string_check_utf8" $
        parseProto "Test.proto" "option java_string_check_utf8 = True;"
        `shouldParse`
        FileDesc.new "Test"


    it "ignores file option: optimize_for with SPEED" $
        parseProto "Test.proto" "option optimize_for = SPEED;"
        `shouldParse`
        FileDesc.new "Test"


    it "ignores file option: optimize_for with CODE_SIZE" $
        parseProto "Test.proto" "option optimize_for = CODE_SIZE;"
        `shouldParse`
        FileDesc.new "Test"


    it "ignores file option: optimize_for with LITE_RUNTIME" $
        parseProto "Test.proto" "option optimize_for = LITE_RUNTIME;"
        `shouldParse`
        FileDesc.new "Test"


    it "ignores file option: py_generic_services" $
        parseProto "Test.proto" "option py_generic_services = True;"
        `shouldParse`
        FileDesc.new "Test"


    it "parses an empty message" $
        parseProto "Test.proto" "message Empty {}"
        `shouldParse`
        FileDesc.addMessageDesc
            (MessageDesc.new "Empty")
            (FileDesc.new "Test")


    it "parses a message with scalar fields" $
        parseProto "Test.proto" (unlines
            [ "message Scalars {"
            , "    optional bool f01 = 1;"
            , "    optional bytes f02 = 2;"
            , "    optional double f03 = 3;"
            , "    optional fixed32 f04 = 4;"
            , "    optional fixed64 f05 = 5;"
            , "    optional float f06 = 6;"
            , "    optional int32 f07 = 0o7;"
            , "    optional int64 f08 = 0o10;"
            , "    optional sfixed32 f09 = 9;"
            , "    optional sfixed64 f10 = 10;"
            , "    optional sint32 f11 = 11;"
            , "    optional sint64 f12 = 12;"
            , "    optional string f13 = 13;"
            , "    optional uint32 f14 = 0xe;"
            , "    optional uint64 f15 = 0xF;"
            , "}"
            ])
        `shouldParse`
        FileDesc.addMessageDesc
            (MessageDesc.addFields
                [ FieldDesc.new "f01"  1 Label.Optional "bool"
                , FieldDesc.new "f02"  2 Label.Optional "bytes"
                , FieldDesc.new "f03"  3 Label.Optional "double"
                , FieldDesc.new "f04"  4 Label.Optional "fixed32"
                , FieldDesc.new "f05"  5 Label.Optional "fixed64"
                , FieldDesc.new "f06"  6 Label.Optional "float"
                , FieldDesc.new "f07"  7 Label.Optional "int32"
                , FieldDesc.new "f08"  8 Label.Optional "int64"
                , FieldDesc.new "f09"  9 Label.Optional "sfixed32"
                , FieldDesc.new "f10" 10 Label.Optional "sfixed64"
                , FieldDesc.new "f11" 11 Label.Optional "sint32"
                , FieldDesc.new "f12" 12 Label.Optional "sint64"
                , FieldDesc.new "f13" 13 Label.Optional "string"
                , FieldDesc.new "f14" 14 Label.Optional "uint32"
                , FieldDesc.new "f15" 15 Label.Optional "uint64"
                ]
                (MessageDesc.new "Scalars")
            )
            (FileDesc.new "Test")


    it "parses a message with a message field" $
        parseProto "Test.proto" (unlines
            [ "message OtherMessageTypes {"
            , "    repeated OtherType result = 1;"
            , "}"
            , "message OtherType {"
            , "    required string name = 1;"
            , "    required string type = 2;"
            , "}"
            ])
        `shouldParse`
        FileDesc.addMessageDescs
            [ MessageDesc.addField
              (FieldDesc.new "result" 1 Label.Repeated "OtherType")
              (MessageDesc.new "OtherMessageTypes")
            , MessageDesc.addFields
              [ (FieldDesc.new "name" 1 Label.Required "string")
              , (FieldDesc.new "type" 2 Label.Required "string")
              ]
              (MessageDesc.new "OtherType")
            ]
            (FileDesc.new "Test")


    it "parses a message with a enum field" $
        parseProto "Test.proto" (unlines
            [ "message EnumMessage {"
            , "    optional EnumType value = 0x1;"
            , "}"
            , "enum EnumType {"
            , "    UNKNOWN = 0;"
            , "    INIT = 0x1;"
            , "}"
            ])
        `shouldParse`
        FileDesc.addMessageDesc
            (MessageDesc.addField
                (FieldDesc.new "value" 1 Label.Optional "EnumType")
                (MessageDesc.new "EnumMessage")
            )
            (FileDesc.addEnumDesc
                (EnumDesc.addValueDescs
                    [ EnumValueDesc.new "Unknown" 0
                    , EnumValueDesc.new "Init" 1
                    ]
                    (EnumDesc.new "EnumType")
                )
                (FileDesc.new "Test")
            )


    it "parses a message with a nested message" $
        parseProto "Test.proto" (unlines
            [ "message OtherMessageTypes {"
            , "    repeated OtherType result = 1;"
            , "    message OtherType {"
            , "        required string name = 1;"
            , "        required string type = 2;"
            , "    }"
            , "}"
            ])
        `shouldParse`
        FileDesc.addMessageDesc
            (MessageDesc.addField
                (FieldDesc.new "result" 1 Label.Repeated "OtherType")
                (MessageDesc.addMessageDesc
                    (MessageDesc.addFields
                        [ (FieldDesc.new "name" 1 Label.Required "string")
                        , (FieldDesc.new "type" 2 Label.Required "string")
                        ]
                        (MessageDesc.new "OtherType")
                    )
                    (MessageDesc.new "OtherMessageTypes")
                )
            )
            (FileDesc.new "Test")


    it "parses a message with a nested enum field" $
        parseProto "Test.proto" (unlines
            [ "message EnumMessage {"
            , "    optional EnumType value = 1;"
            , "    enum EnumType {"
            , "        UNKNOWN = 0;"
            , "        INIT = 1;"
            , "    }"
            , "}"
            ])
        `shouldParse`
        FileDesc.addMessageDesc
            (MessageDesc.addField
                (FieldDesc.new "value" 1 Label.Optional "EnumType")
                (MessageDesc.addEnumDesc
                    ( EnumDesc.addValueDescs
                        [ EnumValueDesc.new "Unknown" 0
                        , EnumValueDesc.new "Init" 1
                        ]
                        (EnumDesc.new "EnumType")
                    )
                    (MessageDesc.new "EnumMessage")
                )
            )
            (FileDesc.new "Test")


    it "ignores message option: message_set_wire_format" $
        parseProto "Test.proto" (unlines
            [ "message Option {"
            , "    option message_set_wire_format = true;"
            , "}"
            ])
        `shouldParse`
        FileDesc.addMessageDesc
            (MessageDesc.new "Option")
            (FileDesc.new "Test")


    it "ignores message option: no_standard_descriptor_accessor" $
        parseProto "Test.proto" (unlines
            [ "message Option {"
            , "    option no_standard_descriptor_accessor = true;"
            , "}"
            ])
        `shouldParse`
        FileDesc.addMessageDesc
            (MessageDesc.new "Option")
            (FileDesc.new "Test")


    it "ignores message option: deprecated" $
        parseProto "Test.proto" (unlines
            [ "message Option {"
            , "    option deprecated = true;"
            , "}"
            ])
        `shouldParse`
        FileDesc.addMessageDesc
            (MessageDesc.new "Option")
            (FileDesc.new "Test")


    it "parses field option: default" $
        parseProto "Test.proto" (unlines
            [ "message Msg {"
            , "    optional int32 field = 1 [default=10];"
            , "}"
            ])
        `shouldParse`
        FileDesc.addMessageDesc
            (MessageDesc.addField
                (FieldDesc.setDefaultValue
                    "10"
                    (FieldDesc.new "field"  1 Label.Optional "int32")
                )
                (MessageDesc.new "Msg")
            )
            (FileDesc.new "Test")


    it "parses field option: packed" $
        parseProto "Test.proto" (unlines
            [ "message Msg {"
            , "    optional int32 field = 1 [packed=true];"
            , "}"
            ])
        `shouldParse`
        FileDesc.addMessageDesc
            (MessageDesc.addField
                (FieldDesc.setPacked
                    True
                    (FieldDesc.new "field"  1 Label.Optional "int32")
                )
                (MessageDesc.new "Msg")
            )
            (FileDesc.new "Test")


    it "ignores field option: deprecated" $
        parseProto "Test.proto" (unlines
            [ "message Msg {"
            , "    optional int32 field = 1 [deprecated=true];"
            , "}"
            ])
        `shouldParse`
        FileDesc.addMessageDesc
            (MessageDesc.addField
                (FieldDesc.new "field"  1 Label.Optional "int32")
                (MessageDesc.new "Msg")
            )
            (FileDesc.new "Test")


    it "ignores field option: lazy" $
        parseProto "Test.proto" (unlines
            [ "message Msg {"
            , "    optional int32 field = 1 [lazy=true];"
            , "}"
            ])
        `shouldParse`
        FileDesc.addMessageDesc
            (MessageDesc.addField
                (FieldDesc.new "field"  1 Label.Optional "int32")
                (MessageDesc.new "Msg")
            )
            (FileDesc.new "Test")


    it "parses enum option: allow_alias" $
        parseProto "Test.proto" (unlines
            [ "enum EnumType {"
            , "    option allow_alias = true;"
            , "    UNKNOWN = 0;"
            , "}"
            ])
        `shouldParse`
        FileDesc.addEnumDesc
            (EnumDesc.setAllowAlias
                True
                (EnumDesc.addValueDesc
                    (EnumValueDesc.new "Unknown" 0)
                    (EnumDesc.new "EnumType")
                )
            )
            (FileDesc.new "Test")


    it "ignores enum option: deprecated" $
        parseProto "Test.proto" (unlines
            [ "enum EnumType {"
            , "    option deprecated = true;"
            , "    UNKNOWN = 0;"
            , "}"
            ])
        `shouldParse`
        FileDesc.addEnumDesc
            (EnumDesc.addValueDesc
                (EnumValueDesc.new "Unknown" 0)
                (EnumDesc.new "EnumType")
            )
            (FileDesc.new "Test")


shouldParse :: (Show a, Show l, Eq a, Eq l) => Either l a -> a -> Expectation
shouldParse actual expected = actual `shouldBe` Right expected
