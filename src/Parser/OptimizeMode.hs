-- |
-- Module:      Parser.OptimizeMode
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- OptimizeMode type.

module Parser.OptimizeMode where

import qualified Prelude

data OptimizeMode = Speed
                  | CodeSize
                  | LiteRuntime
                  deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)

