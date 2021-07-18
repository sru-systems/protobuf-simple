-- |
-- Module:      Parser.GeneratorUtils
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Utility functions for the code generators.

module Parser.GeneratorUtils where

import Control.Monad.State (State)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text.Lazy.Builder (Builder, fromString, singleton)
import Parser.CodeInfo (CodeInfo)
import Parser.FieldDesc (FieldDesc)
import Parser.FileDesc (FileDesc)

import qualified Data.Set          as Set
import qualified Parser.FieldDesc  as FieldDesc
import qualified Parser.FileDesc   as FileDesc
import qualified Parser.Type       as Type


data GenState = GenState
    { fileDescs  :: ![FileDesc]
    , codeInfos  :: ![CodeInfo]
    , messageSet :: !MessageSet
    , enumSet    :: !EnumSet
    }
    deriving (Show)


type EnumName    = String
type EnumSet     = Set EnumName
type MessageName = String
type MessageSet  = Set MessageName
type Packed      = Bool


isEnum :: FileDesc -> FieldDesc -> GenState -> Bool
isEnum f fd state = Set.member fqTypeName (enumSet state)
  where
    fqTypeName = getNamespace f ++ "." ++ FieldDesc.getTypeName fd


isPackable :: FileDesc -> FieldDesc -> GenState -> Bool
isPackable f fd state = case FieldDesc.getType fd of
    (Just Type.Bool)     -> True
    (Just Type.Bytes)    -> False
    (Just Type.Double)   -> True
    (Just Type.Fixed32)  -> True
    (Just Type.Fixed64)  -> True
    (Just Type.Float)    -> True
    (Just Type.Int32)    -> True
    (Just Type.Int64)    -> True
    (Just Type.SFixed32) -> True
    (Just Type.SFixed64) -> True
    (Just Type.SInt32)   -> True
    (Just Type.SInt64)   -> True
    (Just Type.String)   -> False
    (Just Type.UInt32)   -> True
    (Just Type.UInt64)   -> True
    _                    -> isEnum f fd state


isPacked :: FileDesc -> FieldDesc -> GenState -> Bool
isPacked f fd state = isPackable f fd state && optPack
  where
    optPack = fromMaybe False (FieldDesc.getPacked fd)


getComment :: State GenState Builder
getComment = return $ fromString "-- Generated by protobuf-simple. DO NOT EDIT!"


getNamespace :: FileDesc -> String
getNamespace fd = fromMaybe (FileDesc.getName fd) (FileDesc.getPackage fd)


getUnqualifiedImport :: String -> String -> Builder
getUnqualifiedImport name funcs =
    fromString "import " <>
    fromString name <>
    fromString " (" <>
    fromString funcs <>
    fromString ")" <>
    nl


getQualifiedImport :: String -> Builder
getQualifiedImport name =
    fromString "import qualified " <>
    fromString name <>
    nl


getQualifiedAsImport :: String -> String -> Builder
getQualifiedAsImport name alias =
    fromString "import qualified " <>
    fromString name <>
    fromString " as " <>
    fromString alias <>
    nl


empty :: Builder
empty = fromString ""


nl :: Builder
nl = singleton '\n'


space :: Builder
space = singleton ' '


tab :: Builder
tab = fromString "  "
