-- |
-- Module:      Parser.Label
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Label type.

module Parser.Label where

import qualified Prelude

data Label = Optional
           | Required
           | Repeated
           deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)

