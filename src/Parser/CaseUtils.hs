-- |
-- Module:      Parser.CaseUtils
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Utility functions for converting string cases.

module Parser.CaseUtils where


import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.List.Split (splitOn)


fromDotted :: String -> [String]
fromDotted = splitOn "."


fromSnake :: String -> [String]
fromSnake = splitOn "_"


toCamel :: [String] -> String
toCamel (w:ws) = toLowerWord w ++ concatMap toPascalWord ws
toCamel []     = ""


toDotted :: [String] -> String
toDotted = intercalate "."


toLowerWord :: String -> String
toLowerWord = map toLower


toPascal :: [String] -> String
toPascal = concatMap toPascalWord


toPascalWord :: String -> String
toPascalWord (c:cs) = toUpper c : map toLower cs
toPascalWord []     = ""


toTitleWord :: String -> String
toTitleWord (c:cs) = toUpper c : cs
toTitleWord []     = ""
