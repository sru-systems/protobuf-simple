-- |
-- Module:      Parser.FieldDesc
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Field Descriptor type and functions.

module Parser.FieldDesc
    ( FieldDesc
    , new
    , getName
    , getNumber
    , getLabel
    , getType
    , setType
    , getTypeName
    , getDefaultValue
    , setDefaultValue
    , getPacked
    , setPacked
    , isCustom
    , isRequired
    ) where


import Data.Maybe (isNothing)
import Parser.Label (Label(..))
import Prelude (Maybe(..), Bool(..), (==))

import qualified Data.Int
import qualified Parser.Label  as Parser
import qualified Parser.Type   as Parser
import qualified Parser.Type   as Type
import qualified Prelude


data FieldDesc = FieldDesc
    { name          :: Prelude.String
    , number        :: Data.Int.Int32
    , label         :: Parser.Label
    , fieldType     :: Prelude.Maybe Parser.Type
    , fieldTypeName :: Prelude.String
    , defaultValue  :: Prelude.Maybe Prelude.String
    , packed        :: Prelude.Maybe Prelude.Bool
    } deriving (Prelude.Show, Prelude.Eq)


type FieldName     = Prelude.String
type FieldNumber   = Data.Int.Int32
type FieldTypeName = Prelude.String
type FieldDefault  = Prelude.String


new :: FieldName -> FieldNumber -> Parser.Label -> FieldTypeName -> FieldDesc
new n num lbl "bool"     = FieldDesc n num lbl (Just Type.Bool)     "bool"     Nothing Nothing
new n num lbl "bytes"    = FieldDesc n num lbl (Just Type.Bytes)    "bytes"    Nothing Nothing
new n num lbl "double"   = FieldDesc n num lbl (Just Type.Double)   "double"   Nothing Nothing
new n num lbl "fixed32"  = FieldDesc n num lbl (Just Type.Fixed32)  "fixed32"  Nothing Nothing
new n num lbl "fixed64"  = FieldDesc n num lbl (Just Type.Fixed64)  "fixed64"  Nothing Nothing
new n num lbl "float"    = FieldDesc n num lbl (Just Type.Float)    "float"    Nothing Nothing
new n num lbl "int32"    = FieldDesc n num lbl (Just Type.Int32)    "int32"    Nothing Nothing
new n num lbl "int64"    = FieldDesc n num lbl (Just Type.Int64)    "int64"    Nothing Nothing
new n num lbl "sfixed32" = FieldDesc n num lbl (Just Type.SFixed32) "sfixed32" Nothing Nothing
new n num lbl "sfixed64" = FieldDesc n num lbl (Just Type.SFixed64) "sfixed64" Nothing Nothing
new n num lbl "sint32"   = FieldDesc n num lbl (Just Type.SInt32)   "sint32"   Nothing Nothing
new n num lbl "sint64"   = FieldDesc n num lbl (Just Type.SInt64)   "sint64"   Nothing Nothing
new n num lbl "string"   = FieldDesc n num lbl (Just Type.String)   "string"   Nothing Nothing
new n num lbl "uint32"   = FieldDesc n num lbl (Just Type.UInt32)   "uint32"   Nothing Nothing
new n num lbl "uint64"   = FieldDesc n num lbl (Just Type.UInt64)   "uint64"   Nothing Nothing
new n num lbl ft         = FieldDesc n num lbl Nothing              ft         Nothing Nothing


getName :: FieldDesc -> FieldName
getName = name


getNumber :: FieldDesc -> FieldNumber
getNumber = number


getLabel :: FieldDesc -> Parser.Label
getLabel = label


getType :: FieldDesc -> Maybe Parser.Type
getType = fieldType


setType :: Parser.Type -> FieldDesc -> FieldDesc
setType val self = self{fieldType = Just val}


getTypeName :: FieldDesc -> FieldTypeName
getTypeName = fieldTypeName


getDefaultValue :: FieldDesc -> Maybe FieldDefault
getDefaultValue = defaultValue


setDefaultValue :: FieldDefault -> FieldDesc -> FieldDesc
setDefaultValue val self = self{defaultValue = Just val}


getPacked :: FieldDesc -> Maybe Prelude.Bool
getPacked = packed


setPacked :: Prelude.Bool -> FieldDesc -> FieldDesc
setPacked val self = self{packed = Just val}


isCustom :: FieldDesc -> Bool
isCustom FieldDesc { fieldType = f } = isNothing f


isRequired :: FieldDesc -> Bool
isRequired FieldDesc { label = l } = l == Required
