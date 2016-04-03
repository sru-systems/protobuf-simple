-- |
-- Module:      Parser.CodeInfo
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- CodeInfo type.

module Parser.CodeInfo where

import Data.Text.Lazy (Text)
import System.FilePath (FilePath)
import Prelude (Show, String)


data CodeInfo = CodeInfo
    { directories :: !FilePath
    , filename    :: !String
    , code        :: !Text
    } deriving Show

