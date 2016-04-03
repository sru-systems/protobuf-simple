-- |
-- Module:      Parser.Type
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Protocol Buffers scalar types type.

module Parser.Type where

import qualified Prelude

data Type = Bool
          | Bytes
          | Double
          | Fixed32
          | Fixed64
          | Float
          | Int32
          | Int64
          | SFixed32
          | SFixed64
          | SInt32
          | SInt64
          | String
          | UInt64
          | UInt32
          deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
