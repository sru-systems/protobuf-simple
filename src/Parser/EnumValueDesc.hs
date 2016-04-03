-- |
-- Module:      Parser.EnumValueDesc
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Enum Value Descriptor type and functions.

module Parser.EnumValueDesc
    ( EnumValueDesc
    , new
    , getName
    , getNumber
    ) where


import qualified Data.Int
import qualified Prelude


type ValueName   = Prelude.String
type ValueNumber = Data.Int.Int32


data EnumValueDesc = EnumValueDesc
    { name   :: Prelude.String
    , number :: Data.Int.Int32
    } deriving (Prelude.Show, Prelude.Eq)


new :: ValueName -> ValueNumber -> EnumValueDesc
new = EnumValueDesc


getName :: EnumValueDesc -> ValueName
getName = name


getNumber :: EnumValueDesc -> ValueNumber
getNumber = number
