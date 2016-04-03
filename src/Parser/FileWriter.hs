-- |
-- Module:      Parser.FileWriter
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Functions to create type files.

module Parser.FileWriter
    ( write
    ) where

import Data.Text.Lazy.IO (writeFile)
import Parser.CodeInfo
import System.Directory (createDirectoryIfMissing)
import System.FilePath  ((</>))
import Prelude hiding (writeFile)


write :: [CodeInfo] -> IO ()
write = mapM_ createProtoFile


createProtoFile :: CodeInfo -> IO ()
createProtoFile i = do
    createDirectories i
    writeFile (directories i </> filename i) (code i)


createDirectories :: CodeInfo -> IO ()
createDirectories = createDirectoryIfMissing True . directories
