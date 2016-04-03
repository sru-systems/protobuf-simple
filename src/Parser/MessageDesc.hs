-- |
-- Module:      Parser.MessgeDesc
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Message Descriptor type and functions.

module Parser.MessageDesc
    ( MessageDesc
    , new
    , getName
    , addField
    , addFields
    , getFields
    , addMessageDesc
    , addMessageDescs
    , getMessageDescs
    , addEnumDesc
    , addEnumDescs
    , getEnumDescs
    ) where


import Data.Foldable  (toList)
import Data.Sequence  ((|>), (><))
import Prelude        ((.))

import qualified Data.Sequence
import qualified Data.Sequence     as Seq
import qualified Parser.EnumDesc   as Parser
import qualified Parser.FieldDesc  as Parser
import qualified Prelude


data MessageDesc = MessageDesc
    { name         :: Prelude.String
    , fields       :: Data.Sequence.Seq Parser.FieldDesc
    , messageDescs :: Data.Sequence.Seq MessageDesc
    , enumDescs    :: Data.Sequence.Seq Parser.EnumDesc
    } deriving (Prelude.Show, Prelude.Eq)


type MessageName = Prelude.String


new :: MessageName -> MessageDesc
new n = MessageDesc n Seq.empty Seq.empty Seq.empty


getName :: MessageDesc -> MessageName
getName = name


addField :: Parser.FieldDesc -> MessageDesc -> MessageDesc
addField val self = self{fields = fields self |> val}


addFields :: [Parser.FieldDesc] -> MessageDesc -> MessageDesc
addFields vs self = self{fields = fields self >< Seq.fromList vs}


getFields :: MessageDesc -> [Parser.FieldDesc]
getFields = toList . fields


addMessageDesc :: MessageDesc -> MessageDesc -> MessageDesc
addMessageDesc val self = self{messageDescs = messageDescs self |> val}


addMessageDescs :: [MessageDesc] -> MessageDesc -> MessageDesc
addMessageDescs vs self = self{messageDescs = messageDescs self >< Seq.fromList vs}


getMessageDescs :: MessageDesc -> [MessageDesc]
getMessageDescs = toList . messageDescs


addEnumDesc :: Parser.EnumDesc -> MessageDesc -> MessageDesc
addEnumDesc val self = self{enumDescs = enumDescs self |> val}


addEnumDescs :: [Parser.EnumDesc] -> MessageDesc -> MessageDesc
addEnumDescs vs self = self{enumDescs = enumDescs self >< Seq.fromList vs}


getEnumDescs :: MessageDesc -> [Parser.EnumDesc]
getEnumDescs = toList . enumDescs
