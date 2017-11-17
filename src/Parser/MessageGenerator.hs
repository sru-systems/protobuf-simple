-- |
-- Module:      Parser.MessageGenerator
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Generate code for Message types.

module Parser.MessageGenerator
    ( getMessageCode
    ) where

import Control.Monad.State (State, get)
import Data.List (foldl', intersperse)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import Parser.CaseUtils (toPascal)
import Parser.FieldDesc (FieldDesc)
import Parser.FileDesc (FileDesc)
import Parser.GeneratorUtils
import Parser.MessageDesc (MessageDesc)

import qualified Parser.FieldDesc    as FieldDesc
import qualified Parser.FileDesc     as FileDesc
import qualified Parser.Label        as Label
import qualified Parser.MessageDesc  as MessageDesc
import qualified Parser.Type         as Type


getMessageCode :: FileDesc -> MessageDesc -> State GenState Text
getMessageCode f md = do
    commentLine <- getComment
    moduleLine <- getModule f md
    importLines <- getImports f md
    typeLines <- getType f md
    defaultInst <- getDefaultInst f md
    mergeableInst <- getMergeableInst md
    requiredInst <- getRequiredInst f md
    wireMessageInst <- getWireMessageInst f md
    return $ toLazyText $
      commentLine <> nl <>
      moduleLine <> nl <>
      importLines <> nl <>
      typeLines <> nl <>
      defaultInst <> nl <>
      mergeableInst <> nl <>
      requiredInst <> nl <>
      wireMessageInst <> nl


getModule :: FileDesc -> MessageDesc -> State GenState Builder
getModule f md = do
    modName <- getModuleName f md
    return $
        fromString "module " <>
        fromString modName <>
        fromString " where" <> nl


getModuleName :: FileDesc -> MessageDesc -> State GenState String
getModuleName f md = return $ case FileDesc.getPackage f of
    Just package -> package ++ "." ++ messageName
    Nothing      -> fileName ++ "." ++ messageName
  where
    fileName = FileDesc.getName f
    messageName = MessageDesc.getName md


getImports :: FileDesc -> MessageDesc -> State GenState Builder
getImports f md = return $
    (if null (MessageDesc.getFields md)
       then fromString ""
       else getUnqualifiedImport "Control.Applicative" "(<$>)") <>
    getUnqualifiedImport "Prelude" "" <>
    getQualifiedAsImport "Data.ProtoBufInt" "PB" <>
    getExtraImports f md


getExtraImports :: FileDesc -> MessageDesc -> Builder
getExtraImports f md = foldl' (<>) empty imports
  where
    imports = map (getExtraImport f) cfields
    cfields = filter FieldDesc.isCustom $ MessageDesc.getFields md


getExtraImport :: FileDesc -> FieldDesc -> Builder
getExtraImport f fd = getQualifiedImport $ getNamespace f ++ "." ++ typeName
  where
    typeName = FieldDesc.getTypeName fd


getType :: FileDesc -> MessageDesc -> State GenState Builder
getType f md = case MessageDesc.getFields md of
    []   -> getUnitType f md
    [fd] -> getNewtypeType f md fd
    fds  -> getDataType f md fds


getUnitType :: FileDesc -> MessageDesc -> State GenState Builder
getUnitType _ md = return $
    fromString "data " <>
    fromString name <>
    fromString " = " <>
    fromString name <>
    nl <>
    tab <>
    fromString "deriving (PB.Show, PB.Eq, PB.Ord)" <>
    nl
  where name = MessageDesc.getName md


getNewtypeType :: FileDesc -> MessageDesc -> FieldDesc -> State GenState Builder
getNewtypeType f md fd = do
    fieldLine <- getFieldLine f md fd
    return $
        fromString "newtype " <>
        fromString name <>
        fromString " = " <>
        fromString name <>
        nl <>
        tab <>
        fromString "{ " <>
        fieldLine <>
        nl <>
        tab <>
        fromString "} deriving (PB.Show, PB.Eq, PB.Ord)" <>
        nl
  where
    name = MessageDesc.getName md


getDataType :: FileDesc -> MessageDesc -> [FieldDesc] -> State GenState Builder
getDataType f md fds = do
    fieldLines <- getFieldLines f md fds
    return $
        fromString "data " <>
        fromString name <>
        fromString " = " <>
        fromString name <>
        nl <> tab <>
        fromString "{ " <>
        fieldLines <>
        nl <>
        tab <>
        fromString "} deriving (PB.Show, PB.Eq, PB.Ord)" <> nl
  where name = MessageDesc.getName md


getDefaultInst :: FileDesc -> MessageDesc -> State GenState Builder
getDefaultInst f md = do
    defaultLines <- getDefaultLines f fields
    return $
      fromString "instance PB.Default " <> fromString name <> fromString " where" <> nl <>
      tab <> fromString "defaultVal = " <> fromString name <> nl <>
      tab <> tab <> fromString "{ " <>
      defaultLines <> nl <>
      tab <> tab <> fromString "}" <> nl
  where
    name = MessageDesc.getName md
    fields = MessageDesc.getFields md


getDefaultLines :: FileDesc -> [FieldDesc] -> State GenState Builder
getDefaultLines f fds = fmap (foldl' (<>) empty) ilines
  where
    ilines = fmap (intersperse separator) flines
    flines = mapM (getDefaultLine f) fds
    separator = nl <> tab <> tab <> fromString ", "


getDefaultLine :: FileDesc -> FieldDesc -> State GenState Builder
getDefaultLine f fd = do
    dval <- getDefaultValue f fd
    return $ fname <> fromString " = " <> dval
  where fname = fromString $ FieldDesc.getName fd


getDefaultValue :: FileDesc -> FieldDesc -> State GenState Builder
getDefaultValue f fd = do
    state <- get
    return $ fromString $ case FieldDesc.getLabel fd of
      Label.Required -> "PB.defaultVal"
      Label.Repeated -> "PB.defaultVal"
      Label.Optional -> case FieldDesc.getDefaultValue fd of
        Nothing  -> "PB.defaultVal"
        Just val -> case FieldDesc.getType fd of
          (Just Type.Bool)     -> if val == "true"
                                    then "PB.Just PB.True"
                                    else "PB.Just PB.False"
          (Just Type.Bytes)    -> "PB.DefaultVal"
          (Just Type.String)   -> "PB.Just (PB.pack " ++ val ++ ")"
          (Just Type.Double)   -> "PB.Just " ++ val
          (Just Type.Fixed32)  -> "PB.Just " ++ val
          (Just Type.Fixed64)  -> "PB.Just " ++ val
          (Just Type.Float)    -> "PB.Just " ++ val
          (Just Type.Int32)    -> "PB.Just " ++ val
          (Just Type.Int64)    -> "PB.Just " ++ val
          (Just Type.SFixed32) -> "PB.Just " ++ val
          (Just Type.SFixed64) -> "PB.Just " ++ val
          (Just Type.SInt32)   -> "PB.Just " ++ val
          (Just Type.SInt64)   -> "PB.Just " ++ val
          (Just Type.UInt32)   -> "PB.Just " ++ val
          (Just Type.UInt64)   -> "PB.Just " ++ val
          _                    -> if isEnum f fd state
                                    then "PB.Just " ++ enumValue val
                                    else "PB.DefaultVal"
  where
    enumValue v = namespace ++ "." ++ enumType ++ "." ++ enumName v
    namespace = getNamespace f
    enumType = FieldDesc.getTypeName fd
    enumName v = toPascal [v]


getFieldLines :: FileDesc -> MessageDesc -> [FieldDesc] -> State GenState Builder
getFieldLines f md fds = fmap (foldl' (<>) empty) ilines
  where
    ilines = fmap (intersperse separator) flines
    flines = mapM (getFieldLine f md) fds
    separator = nl <> tab <> fromString ", "


getFieldLine :: FileDesc -> MessageDesc -> FieldDesc -> State GenState Builder
getFieldLine f md fd = do
    fname <- getFieldName fd
    ftype <- getFieldType f md fd
    return $ fname <> fromString " :: " <> ftype


getFieldName :: FieldDesc -> State GenState Builder
getFieldName = return . fromString . FieldDesc.getName


getFieldType :: FileDesc -> MessageDesc -> FieldDesc -> State GenState Builder
getFieldType f md fd = return $ case FieldDesc.getLabel fd of
    Label.Optional ->
      if fieldCount > 1
        then fromString "!(PB.Maybe " <> innerType <> fromString ")"
        else fromString "PB.Maybe " <> innerType
    Label.Required ->
      if fieldCount > 1
        then fromString "!" <> innerType
        else innerType
    Label.Repeated ->
      if fieldCount > 1
        then fromString "!(PB.Seq " <> innerType <> fromString ")"
        else fromString "PB.Seq " <> innerType
  where
    fieldCount = length $ MessageDesc.getFields md
    innerType = getFieldInnerType f fd


getFieldInnerType :: FileDesc -> FieldDesc -> Builder
getFieldInnerType f fd = fromString $ case FieldDesc.getType fd of
    (Just Type.Bool)     -> "PB.Bool"
    (Just Type.Bytes)    -> "PB.ByteString"
    (Just Type.Double)   -> "PB.Double"
    (Just Type.Fixed32)  -> "PB.Word32"
    (Just Type.Fixed64)  -> "PB.Word64"
    (Just Type.Float)    -> "PB.Float"
    (Just Type.Int32)    -> "PB.Int32"
    (Just Type.Int64)    -> "PB.Int64"
    (Just Type.SFixed32) -> "PB.Int32"
    (Just Type.SFixed64) -> "PB.Int64"
    (Just Type.SInt32)   -> "PB.Int32"
    (Just Type.SInt64)   -> "PB.Int64"
    (Just Type.String)   -> "PB.Text"
    (Just Type.UInt32)   -> "PB.Word32"
    (Just Type.UInt64)   -> "PB.Word64"
    _                    -> namespace ++ "." ++ typeName ++ "." ++ typeName
  where
    namespace = getNamespace f
    typeName = FieldDesc.getTypeName fd


getMergeableInst :: MessageDesc -> State GenState Builder
getMergeableInst md = do
    mergeableLines <- getMergeableLines fields
    return $
      fromString "instance PB.Mergeable " <> fromString name <> fromString " where" <> nl <>
      tab <> (if mergeableLines == fromString ""
                then fromString "merge _ _ = "
                else fromString "merge a b = ") <> fromString name <> nl <>
      tab <> tab <> fromString "{ " <>
      mergeableLines <> nl <>
      tab <> tab <> fromString "}" <> nl
  where
    name = MessageDesc.getName md
    fields = MessageDesc.getFields md


getMergeableLines :: [FieldDesc] -> State GenState Builder
getMergeableLines fds = fmap (foldl' (<>) empty) ilines
  where
    ilines = fmap (intersperse separator) flines
    flines = mapM getMergeableLine fds
    separator = nl <> tab <> tab <> fromString ", "


getMergeableLine :: FieldDesc -> State GenState Builder
getMergeableLine fd = return $
    fieldName <>
    fromString " = PB.merge (" <>
    fieldName <>
    fromString " a) (" <>
    fieldName <>
    fromString " b)"
  where fieldName = fromString $ FieldDesc.getName fd


getRequiredInst :: FileDesc -> MessageDesc -> State GenState Builder
getRequiredInst f md = do
    reqTags <- getRequiredWireTags f fields
    return $
      fromString "instance PB.Required " <> name <> fromString " where" <> nl <>
      tab <> fromString "reqTags _ = PB.fromList [" <>
      reqTags <>
      fromString "]" <> nl
  where
    name = fromString $ MessageDesc.getName md
    fields = MessageDesc.getFields md


getRequiredWireTags :: FileDesc -> [FieldDesc] -> State GenState Builder
getRequiredWireTags f fds = do
    tags <- mapM (getWireTag False f) rfields
    return $ foldl' (<>) empty (intersperse separator tags)
  where
    separator = fromString ", "
    rfields = filter FieldDesc.isRequired fds


getWireTag :: Packed -> FileDesc -> FieldDesc -> State GenState Builder
getWireTag packed f fd = do
    wireType <- getWireType packed f fd
    return $ header <> space <> fieldNumber <> space <> wireType
  where
    header = fromString "PB.WireTag"
    fieldNumber = fromString $ show $ FieldDesc.getNumber fd


getWireType :: Packed -> FileDesc -> FieldDesc -> State GenState Builder
getWireType packed f fd = do
    state <- get
    return $ fromString $ if packed
      then "PB.LenDelim"
      else case FieldDesc.getType fd of
        (Just Type.Bool)     -> "PB.VarInt"
        (Just Type.Bytes)    -> "PB.LenDelim"
        (Just Type.Double)   -> "PB.Bit64"
        (Just Type.Fixed32)  -> "PB.Bit32"
        (Just Type.Fixed64)  -> "PB.Bit64"
        (Just Type.Float)    -> "PB.Bit32"
        (Just Type.Int32)    -> "PB.VarInt"
        (Just Type.Int64)    -> "PB.VarInt"
        (Just Type.SFixed32) -> "PB.Bit32"
        (Just Type.SFixed64) -> "PB.Bit64"
        (Just Type.SInt32)   -> "PB.VarInt"
        (Just Type.SInt64)   -> "PB.VarInt"
        (Just Type.String)   -> "PB.LenDelim"
        (Just Type.UInt32)   -> "PB.VarInt"
        (Just Type.UInt64)   -> "PB.VarInt"
        _                    -> if isEnum f fd state
                                  then "PB.VarInt"
                                  else "PB.LenDelim"


getWireMessageInst :: FileDesc -> MessageDesc -> State GenState Builder
getWireMessageInst f md = do
    mlines <- getMessageToFieldLines f fields
    flines <- getFieldToValueLines f fields
    return $
      fromString "instance PB.WireMessage " <> fromString name <> fromString " where" <> nl <>
      flines <> nl <>
      tab <> if mlines == fromString ""
               then fromString "messageToFields _ = PB.return ()" <> nl
               else fromString "messageToFields self = do" <> nl <>
                    mlines <> nl
  where
    name = MessageDesc.getName md
    fields = MessageDesc.getFields md


getMessageToFieldLines :: FileDesc -> [FieldDesc] -> State GenState Builder
getMessageToFieldLines f fds = do
    flines <- mapM (getMessageToFieldLine f) fds
    return $ foldl' (<>) empty flines


getMessageToFieldLine :: FileDesc -> FieldDesc -> State GenState Builder
getMessageToFieldLine f fd = do
    state <- get
    fun <- getMessageToFieldFun f fd
    tag <- getWireTag (isPacked f fd state) f fd
    return $
      tab <> tab <>
      fun <>
      fromString " (" <>
      tag <>
      fromString ") (" <>
      name <>
      fromString " self)" <>
      nl
  where
    name = fromString $ FieldDesc.getName fd


getMessageToFieldFun :: FileDesc -> FieldDesc -> State GenState Builder
getMessageToFieldFun f fd = case FieldDesc.getLabel fd of
    Label.Optional -> getMessageToFieldFunOpt f fd
    Label.Repeated -> getMessageToFieldFunRep f fd
    Label.Required -> getMessageToFieldFunReq f fd


getMessageToFieldFunOpt :: FileDesc -> FieldDesc -> State GenState Builder
getMessageToFieldFunOpt f fd = do
    fun <- getMessageToFieldFunReq f fd
    return $ fun <> fromString "Opt"


getMessageToFieldFunRep :: FileDesc -> FieldDesc -> State GenState Builder
getMessageToFieldFunRep f fd = do
    state <- get
    fun <- getMessageToFieldFunReq f fd
    return $ fun <> if isPacked f fd state
      then fromString "Packed"
      else fromString "List"


getMessageToFieldFunReq :: FileDesc -> FieldDesc -> State GenState Builder
getMessageToFieldFunReq f fd = do
    state <- get
    return $ fromString $ case FieldDesc.getType fd of
      (Just Type.Bool)     -> "PB.putBool"
      (Just Type.Bytes)    -> "PB.putBytes"
      (Just Type.Double)   -> "PB.putDouble"
      (Just Type.Fixed32)  -> "PB.putFixed32"
      (Just Type.Fixed64)  -> "PB.putFixed64"
      (Just Type.Float)    -> "PB.putFloat"
      (Just Type.Int32)    -> "PB.putInt32"
      (Just Type.Int64)    -> "PB.putInt64"
      (Just Type.SFixed32) -> "PB.putSFixed32"
      (Just Type.SFixed64) -> "PB.putSFixed64"
      (Just Type.SInt32)   -> "PB.putSInt32"
      (Just Type.SInt64)   -> "PB.putSInt64"
      (Just Type.String)   -> "PB.putString"
      (Just Type.UInt32)   -> "PB.putUInt32"
      (Just Type.UInt64)   -> "PB.putUInt64"
      _                    -> if isEnum f fd state
                                then "PB.putEnum"
                                else "PB.putMessage"


getFieldToValueDefault :: Builder
getFieldToValueDefault =
    tab <>
    fromString "fieldToValue tag self = PB.getUnknown tag self" <>
    nl


getFieldToValueLines :: FileDesc -> [FieldDesc] -> State GenState Builder
getFieldToValueLines f fds = do
    flines <- mapM (getFieldToValueLine f) fds
    return $ foldl' (<>) empty flines <> getFieldToValueDefault


getFieldToValueLine :: FileDesc -> FieldDesc -> State GenState Builder
getFieldToValueLine f fd = case FieldDesc.getLabel fd of
    Label.Optional -> (<> nl) <$> getFieldToValueOpt f fd
    Label.Repeated -> (<> nl) <$> getFieldToValueRep f fd
    Label.Required -> (<> nl) <$> getFieldToValueReq f fd


getFieldToValueOpt :: FileDesc -> FieldDesc -> State GenState Builder
getFieldToValueOpt f fd = do
    line <- getFieldToValueReq f fd
    return $ line <> fromString "Opt"


getFieldToValueRep :: FileDesc -> FieldDesc -> State GenState Builder
getFieldToValueRep f fd = do
    state <- get
    if isPacked f fd state
      then do
        packedLine <- getFieldToValueRepPacked f fd
        listLine <- getFieldToValueRepList f fd
        return $ packedLine <> nl <> listLine
      else getFieldToValueRepList f fd


getFieldToValueRepList :: FileDesc -> FieldDesc -> State GenState Builder
getFieldToValueRepList f fd = do
    fun <- getFieldToValueFun f fd
    tag <- getWireTag False f fd
    return $
      tab <>
      fromString "fieldToValue (" <>
      tag <>
      fromString ") self = (\\v -> self{" <>
      name <>
      fromString " = PB.append (" <>
      name <>
      fromString " self) v}) <$> " <>
      fun
  where
    name = fromString $ FieldDesc.getName fd


getFieldToValueRepPacked :: FileDesc -> FieldDesc -> State GenState Builder
getFieldToValueRepPacked f fd = do
    line <- getFieldToValueReq f fd
    return $ line <> fromString "Packed"


getFieldToValueReq :: FileDesc -> FieldDesc -> State GenState Builder
getFieldToValueReq f fd = do
    state <- get
    fun <- getFieldToValueFun f fd
    tag <- getWireTag (isPacked f fd state) f fd
    return $
      tab <>
      fromString "fieldToValue (" <>
      tag <>
      fromString ") self = (\\v -> self{" <>
      name <>
      fromString " = PB.merge (" <>
      name <>
      fromString " self) v}) <$> " <>
      fun
  where
    name = fromString $ FieldDesc.getName fd


getFieldToValueFun :: FileDesc -> FieldDesc -> State GenState Builder
getFieldToValueFun f fd = do
    state <- get
    return $ fromString $ case FieldDesc.getType fd of
      (Just Type.Bool)     -> "PB.getBool"
      (Just Type.Bytes)    -> "PB.getBytes"
      (Just Type.Double)   -> "PB.getDouble"
      (Just Type.Fixed32)  -> "PB.getFixed32"
      (Just Type.Fixed64)  -> "PB.getFixed64"
      (Just Type.Float)    -> "PB.getFloat"
      (Just Type.Int32)    -> "PB.getInt32"
      (Just Type.Int64)    -> "PB.getInt64"
      (Just Type.SFixed32) -> "PB.getSFixed32"
      (Just Type.SFixed64) -> "PB.getSFixed64"
      (Just Type.SInt32)   -> "PB.getSInt32"
      (Just Type.SInt64)   -> "PB.getSInt64"
      (Just Type.String)   -> "PB.getString"
      (Just Type.UInt32)   -> "PB.getUInt32"
      (Just Type.UInt64)   -> "PB.getUInt64"
      _                    -> if isEnum f fd state
                                then "PB.getEnum"
                                else "PB.getMessage"
