-- |
-- Module:      Parser.EnumGenerator
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Generate code for Enum types.

module Parser.EnumGenerator
    ( getEnumCode
    ) where

import Control.Monad.State (State)
import Data.List (foldl', groupBy, intersperse, sortOn)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import Parser.EnumDesc (EnumDesc)
import Parser.EnumValueDesc (EnumValueDesc)
import Parser.FileDesc (FileDesc)
import Parser.GeneratorUtils
    ( GenState,
      getComment,
      getUnqualifiedImport,
      getQualifiedAsImport,
      empty,
      nl,
      tab
    )

import qualified Parser.EnumDesc       as EnumDesc
import qualified Parser.EnumValueDesc  as EnumValueDesc
import qualified Parser.FileDesc       as FileDesc


getEnumCode :: FileDesc -> EnumDesc -> State GenState Text
getEnumCode f ed = do
    commentLine <- getComment
    moduleLine <- getModule f ed
    importLines <- getImports f ed
    typeLines <- getType f ed
    defaultInst <- getDefaultInst ed
    mergeableInst <- getMergeableInst ed
    wireEnumInst <- getWireEnumInst ed
    return $ toLazyText $
      commentLine <> nl <>
      moduleLine <> nl <>
      importLines <> nl <>
      typeLines <> nl <>
      defaultInst <> nl <>
      mergeableInst <> nl <>
      wireEnumInst <> nl


getModule :: FileDesc -> EnumDesc -> State GenState Builder
getModule f ed = do
    modName <- getModuleName f ed
    return $
      fromString "module " <>
      fromString modName <>
      fromString " where" <> nl


getModuleName :: FileDesc -> EnumDesc -> State GenState String
getModuleName f ed = return $ case FileDesc.getPackage f of
    Just package -> package ++ "." ++ enumName
    Nothing      -> fileName ++ "." ++ enumName
  where
    fileName = FileDesc.getName f
    enumName = EnumDesc.getName ed


getImports :: FileDesc -> EnumDesc -> State GenState Builder
getImports _ _ = return $
    getUnqualifiedImport "Prelude" "" <>
    getQualifiedAsImport "Data.ProtoBufInt" "PB"


getType :: FileDesc -> EnumDesc -> State GenState Builder
getType f ed = do
    enumValues <- getEnumValues f ed
    return $
      fromString "data " <>
      fromString name <>
      fromString " = " <>
      enumValues <>
      nl <>
      tab <>
      fromString "deriving (PB.Show, PB.Eq, PB.Ord)" <> nl
  where name = EnumDesc.getName ed


getEnumValues :: FileDesc -> EnumDesc -> State GenState Builder
getEnumValues _ ed = fmap (foldl' (<>) empty) ivalues
  where
    ivalues = fmap (intersperse separator) fvalues
    fvalues = mapM getEnumValue (EnumDesc.getValueDescs ed)
    separator = fromString " | "


getEnumValue :: EnumValueDesc -> State GenState Builder
getEnumValue = return . fromString . EnumValueDesc.getName


getDefaultInst :: EnumDesc -> State GenState Builder
getDefaultInst ed = return $
    fromString "instance PB.Default " <> fromString name <> fromString " where" <> nl <>
    tab <> fromString "defaultVal = " <> fromString first <> nl
  where
    name = EnumDesc.getName ed
    first = EnumValueDesc.getName $ head $ EnumDesc.getValueDescs ed


getMergeableInst :: EnumDesc -> State GenState Builder
getMergeableInst ed = return $
    fromString "instance PB.Mergeable " <> fromString name <> fromString " where" <> nl
  where
    name = EnumDesc.getName ed


getWireEnumInst :: EnumDesc -> State GenState Builder
getWireEnumInst ed = return $
    fromString "instance PB.WireEnum " <> fromString name <> fromString " where" <> nl <>
    intToEnumLines <> nl <>
    enumToIntLines <> nl
  where
    name = EnumDesc.getName ed
    intToEnumLines = getIntToEnumLines (EnumDesc.getValueDescs ed)
    enumToIntLines = getEnumToIntLines (EnumDesc.getValueDescs ed)


getIntToEnumLines :: [EnumValueDesc] -> Builder
getIntToEnumLines es =
    foldl' (<>) empty (map getIntToEnumLine $ uniqEs es) <>
    tab <> fromString "intToEnum _ = PB.defaultVal" <> nl
  where
    uniqEs = map head . groupBy areEq . sortOn EnumValueDesc.getNumber
    areEq a b = EnumValueDesc.getNumber a == EnumValueDesc.getNumber b


getIntToEnumLine :: EnumValueDesc -> Builder
getIntToEnumLine e = tab <> fromString "intToEnum " <> number <> fromString " = " <> name <> nl
  where
    name = fromString $ EnumValueDesc.getName e
    number = fromString $ show $ EnumValueDesc.getNumber e


getEnumToIntLines :: [EnumValueDesc] -> Builder
getEnumToIntLines es = foldl' (<>) empty (map getEnumToIntLine es)


getEnumToIntLine :: EnumValueDesc -> Builder
getEnumToIntLine e = tab <> fromString "enumToInt " <> name <> fromString " = " <> number <> nl
  where
    name = fromString $ EnumValueDesc.getName e
    number = fromString $ show $ EnumValueDesc.getNumber e
