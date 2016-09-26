-- |
-- Module:      Parser.Generator
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Generator for the data types.

module Parser.Generator
    ( generate
    ) where


import Control.Monad.State (State, execState, get, put)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Parser.CodeInfo
import Parser.EnumDesc (EnumDesc)
import Parser.EnumGenerator (getEnumCode)
import Parser.FileDesc (FileDesc)
import Parser.GeneratorUtils
import Parser.MessageDesc (MessageDesc)
import Parser.MessageGenerator (getMessageCode)
import System.FilePath (joinPath)

import qualified Data.Set            as Set
import qualified Parser.EnumDesc     as EnumDesc
import qualified Parser.FileDesc     as FileDesc
import qualified Parser.MessageDesc  as MessageDesc


generate :: [FileDesc] -> [CodeInfo]
generate fs = codeInfos $ execState (mapM_ addFileCodeInfos fs) state
  where
    state = GenState fs [] mset eset
    mset  = getMessageSet fs
    eset  = getEnumSet fs


addFileCodeInfos :: FileDesc -> State GenState ()
addFileCodeInfos f = do
    mapM_ (addMessageCodeInfo f) $ getMessageDescs f
    mapM_ (addEnumCodeInfo f) $ getEnumDescs f


addMessageCodeInfo :: FileDesc -> MessageDesc -> State GenState ()
addMessageCodeInfo fd md = do
    state <- get
    fdirs <- getDirectories fd
    fname <- getMessageFilename md
    fcode <- getMessageCode fd md
    put $ state{codeInfos = CodeInfo fdirs fname fcode : codeInfos state}


addEnumCodeInfo :: FileDesc -> EnumDesc -> State GenState ()
addEnumCodeInfo fd ed = do
    state <- get
    fdirs <- getDirectories fd
    fname <- getEnumFilename ed
    fcode <- getEnumCode fd ed
    put $ state{codeInfos = CodeInfo fdirs fname fcode : codeInfos state}


getMessageDescs :: FileDesc -> [MessageDesc]
getMessageDescs f = addMessageDescs [] $ FileDesc.getMessageDescs f


addMessageDescs :: [MessageDesc] -> [MessageDesc] -> [MessageDesc]
addMessageDescs = foldl' addMessageDesc


addMessageDesc :: [MessageDesc] -> MessageDesc -> [MessageDesc]
addMessageDesc mlist m = case MessageDesc.getMessageDescs m of
    [] -> m : mlist
    ns -> addMessageDescs (m : mlist) ns


getEnumDescs :: FileDesc -> [EnumDesc]
getEnumDescs f = insertSubEnums (insertEnums [] f) f
  where
    insertEnums    elist fd = addEnumDescs elist (FileDesc.getEnumDescs fd)
    insertSubEnums elist fd = addSubEnumDescs elist (FileDesc.getMessageDescs fd)


addEnumDescs :: [EnumDesc] -> [EnumDesc] -> [EnumDesc]
addEnumDescs = foldl' addEnumDesc


addEnumDesc :: [EnumDesc] -> EnumDesc -> [EnumDesc]
addEnumDesc elist e = e : elist


addSubEnumDescs :: [EnumDesc] -> [MessageDesc] -> [EnumDesc]
addSubEnumDescs = foldl' addSubEnumDesc


addSubEnumDesc :: [EnumDesc] -> MessageDesc -> [EnumDesc]
addSubEnumDesc elist m = case MessageDesc.getMessageDescs m of
    [] -> case MessageDesc.getEnumDescs m of
        [] -> elist
        es -> addEnumDescs elist es
    ms -> case MessageDesc.getEnumDescs m of
        [] -> addSubEnumDescs elist ms
        es -> addSubEnumDescs (addEnumDescs elist es) ms


getMessageSet :: [FileDesc] -> MessageSet
getMessageSet = foldl' insertMany Set.empty
  where
    insertMany mset fd = foldl' (insertOne fd) mset $ getMessageDescs fd
    insertOne fd mset md = Set.insert (getFullMessageName fd md) mset


getEnumSet :: [FileDesc] -> EnumSet
getEnumSet = foldl' insertMany Set.empty
  where
    insertMany eset fd = foldl' (insertOne fd) eset $ getEnumDescs fd
    insertOne fd eset ed = Set.insert (getFullEnumName fd ed) eset


getDirectories :: FileDesc -> State GenState FilePath
getDirectories fd = case FileDesc.getPackage fd of
    Just val -> return $ joinPath $ splitOn "." val
    Nothing  -> return $ FileDesc.getName fd


getMessageFilename :: MessageDesc -> State GenState FilePath
getMessageFilename md = return $ MessageDesc.getName md ++ ".hs"


getEnumFilename :: EnumDesc -> State GenState FilePath
getEnumFilename ed = return $ EnumDesc.getName ed ++ ".hs"


getFullEnumName :: FileDesc -> EnumDesc -> String
getFullEnumName fd ed = (getNamespace fd) ++ "." ++ (EnumDesc.getName ed)


getFullMessageName :: FileDesc -> MessageDesc -> String
getFullMessageName fd md = (getNamespace fd) ++ "." ++ (MessageDesc.getName md)

