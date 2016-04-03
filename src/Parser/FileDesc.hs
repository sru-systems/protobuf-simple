-- |
-- Module:      Parser.FileDesc
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- File Descriptor type and functions.

module Parser.FileDesc
    ( FileDesc
    , new
    , getName
    , getPackage
    , setPackage
    , addMessageDesc
    , addMessageDescs
    , getMessageDescs
    , addEnumDesc
    , addEnumDescs
    , getEnumDescs
    ) where


import Prelude (Maybe(..), (.))
import Data.Foldable (toList)
import Data.Sequence ((|>), (><))

import qualified Data.Sequence
import qualified Data.Sequence       as Seq
import qualified Parser.EnumDesc     as Parser
import qualified Parser.MessageDesc  as Parser
import qualified Prelude


data FileDesc = FileDesc
    { name               :: Prelude.String
    , package            :: Prelude.Maybe Prelude.String
    , messageDescs       :: Data.Sequence.Seq Parser.MessageDesc
    , enumDescs          :: Data.Sequence.Seq Parser.EnumDesc
    } deriving (Prelude.Show, Prelude.Eq)


type FileName = Prelude.String
type Package  = Prelude.String


new :: FileName -> FileDesc
new fn = FileDesc fn Nothing Seq.empty Seq.empty


getName :: FileDesc -> FileName
getName = name


getPackage :: FileDesc -> Maybe Package
getPackage = package


setPackage :: Package -> FileDesc -> FileDesc
setPackage val self = self{package = Just val}


addMessageDesc :: Parser.MessageDesc -> FileDesc -> FileDesc
addMessageDesc val self = self{messageDescs = messageDescs self |> val}


addMessageDescs :: [Parser.MessageDesc] -> FileDesc -> FileDesc
addMessageDescs vs self = self{messageDescs = messageDescs self >< Seq.fromList vs}


getMessageDescs :: FileDesc -> [Parser.MessageDesc]
getMessageDescs = toList . messageDescs


addEnumDesc :: Parser.EnumDesc -> FileDesc -> FileDesc
addEnumDesc val self = self{enumDescs = enumDescs self |> val}


addEnumDescs :: [Parser.EnumDesc] -> FileDesc -> FileDesc
addEnumDescs vs self = self{enumDescs = enumDescs self >< Seq.fromList vs}


getEnumDescs :: FileDesc -> [Parser.EnumDesc]
getEnumDescs = toList . enumDescs
