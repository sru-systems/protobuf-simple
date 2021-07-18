-- |
-- Module:      Main
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Main module for the protobuf-simple-protoc executable.

module Main where


import Data.Either (lefts, rights)
import Parser.FileDesc (FileDesc)
import Parser.FileWriter (write)
import Parser.Generator (generate)
import Parser.ProtoParser (parseProto)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import System.Environment (getArgs)


data Options = Options
    { optHelp    :: Bool
    , optVersion :: Bool
    } deriving Show


data ProtoInfo = ProtoInfo
    { filePath :: FilePath
    , content  :: String
    } deriving Show


defaultOptions :: Options
defaultOptions = Options
    { optHelp    = False
    , optVersion = False
    }


options :: [OptDescr (Options -> Options)]
options =
    [ Option ['h'] ["help"]
        (NoArg (\opts -> opts{optHelp = True}))
        "show usage"
    , Option ['v'] ["version"]
        (NoArg (\opts -> opts{optVersion = True}))
        "show version number"
    ]


parserOpts :: [String] -> Either String (Options, [String])
parserOpts argv =
    case getOpt Permute options argv of
      (o,n,[]  ) -> Right (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> Left  (concat errs ++ usage)



main :: IO ()
main = do
    args <- getArgs
    case parserOpts args of
      Right opts -> handleOpts opts
      Left  errs -> putStrLn errs


handleOpts :: (Options, [String]) -> IO ()
handleOpts (Options {optHelp=True}, _)    = putStrLn  usage
handleOpts (Options {optVersion=True}, _) = putStrLn  version
handleOpts (Options {}, [])               = putStrLn  noFiles
handleOpts (Options {}, fs)               = printFiles $ getProtoInfos fs


getProtoInfos :: [FilePath] -> IO [ProtoInfo]
getProtoInfos = mapM getProtoInfo


getProtoInfo :: FilePath -> IO ProtoInfo
getProtoInfo fPath = do
    fData <- readFile fPath
    return $ ProtoInfo fPath fData


printFiles :: IO [ProtoInfo] -> IO ()
printFiles infos = do
    is <- infos
    let parsed = parseFiles is
    putStr $ unlines $ lefts parsed
    write $ generate $ rights parsed


parseFiles :: [ProtoInfo] -> [Either String FileDesc]
parseFiles = map parseFile


parseFile :: ProtoInfo -> Either String FileDesc
parseFile info = parseProto (filePath info) (content info)


noFiles :: String
noFiles = "protobuf-simple-protoc: no file given\n" ++ usage


usage :: String
usage = usageInfo "Usage: protobuf-simple-protoc [OPTION]... FILES" options


version :: String
version = "protobuf-simple-protoc 0.1.1.1"
