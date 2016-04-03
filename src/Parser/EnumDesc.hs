-- |
-- Module:      Parser.EnumDesc
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Enum Descriptor type and functions.

module Parser.EnumDesc
    ( EnumDesc
    , new
    , getName
    , addValueDesc
    , addValueDescs
    , getValueDescs
    , getAllowAlias
    , setAllowAlias
    ) where


import Data.Foldable (toList)
import Data.Sequence ((|>), (><))
import Prelude (Maybe(..), (.))

import qualified Data.Sequence
import qualified Data.Sequence         as Seq
import qualified Parser.EnumValueDesc  as Parser
import qualified Prelude


data EnumDesc = EnumDesc
    { name       :: Prelude.String
    , valueDescs :: Data.Sequence.Seq Parser.EnumValueDesc
    , allowAlias :: Prelude.Maybe Prelude.Bool
    } deriving (Prelude.Show, Prelude.Eq)


type EnumName = Prelude.String


new :: EnumName -> EnumDesc
new n = EnumDesc n Seq.empty Nothing


getName :: EnumDesc -> EnumName
getName = name


addValueDesc :: Parser.EnumValueDesc -> EnumDesc -> EnumDesc
addValueDesc val self = self{valueDescs = valueDescs self |> val}


addValueDescs :: [Parser.EnumValueDesc] -> EnumDesc -> EnumDesc
addValueDescs vs self = self{valueDescs = valueDescs self >< Seq.fromList vs}


getValueDescs :: EnumDesc -> [Parser.EnumValueDesc]
getValueDescs = toList . valueDescs


getAllowAlias :: EnumDesc -> Maybe Prelude.Bool
getAllowAlias = allowAlias


setAllowAlias :: Prelude.Bool -> EnumDesc -> EnumDesc
setAllowAlias val self = self{allowAlias = Just val}
