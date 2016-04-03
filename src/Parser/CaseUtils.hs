-- |
-- Module:      Parser.CaseUtils
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Utility functions for converting string cases.

module Parser.CaseUtils
    ( fromSnake
    , toCamel
    , toPascal
    ) where


import Data.Char (toLower, toUpper)
import Data.List.Split (splitOn)


fromSnake :: String -> [String]
fromSnake = splitOn "_"


toCamel :: [String] -> String
toCamel (w:ws) = toLowerWord w ++ concatMap toPascalWord ws
toCamel []     = ""


toPascal :: [String] -> String
toPascal = concatMap toPascalWord


toLowerWord :: String -> String
toLowerWord = map toLower


toPascalWord :: String -> String
toPascalWord (c:cs) = toUpper c : map toLower cs
toPascalWord []     = ""
