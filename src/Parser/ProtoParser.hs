-- |
-- Module:      Parser.ProtoParser
-- Copyright:   (c) 2015-2016 Martijn Rijkeboer <mrr@sru-systems.com>
-- License:     MIT
-- Maintainer:  Martijn Rijkeboer <mrr@sru-systems.com>
--
-- Parser for proto files.

module Parser.ProtoParser
    ( parseProto
    ) where


import Control.Monad (void)
import Data.Functor (($>))
import Data.Int (Int32)
import Parser.EnumDesc (EnumDesc)
import Parser.FieldDesc (FieldDesc)
import Parser.FileDesc (FileDesc)
import Parser.Label (Label)
import Parser.MessageDesc (MessageDesc)
import Parser.OptimizeMode (OptimizeMode)
import System.FilePath (dropExtensions, splitFileName)
import Text.Parsec

import qualified Parser.CaseUtils      as CU
import qualified Parser.EnumDesc       as EnumDesc
import qualified Parser.EnumValueDesc  as EnumValueDesc
import qualified Parser.FieldDesc      as FieldDesc
import qualified Parser.FileDesc       as FileDesc
import qualified Parser.Label          as Label
import qualified Parser.MessageDesc    as MessageDesc
import qualified Parser.OptimizeMode   as OptimizeMode


parseProto :: String -> String -> Either String FileDesc
parseProto filePath input =
    case parse (file $ FileDesc.new $ getName filePath) filePath input of
      Right fileDesc   -> Right fileDesc
      Left  parseError -> Left $ show parseError


getName :: String -> String
getName = dropExtensions . snd . splitFileName


-- FileDesc functions
file :: FileDesc -> Parsec String u FileDesc
file s = (eof >> return s) <|> (choice
    [ comments s
    , syntax s
    , package s
    , fileOption s
    , enumDesc s
    , messageDesc s
    ] >>= file)


comments :: FileDesc -> Parsec String u FileDesc
comments s = do
  void $ choice
    [ string "//" *> manyTill anyChar (void eol  <|> eof)
    , many1 (oneOf " \t\n\r")
    ]
  return s


syntax :: FileDesc -> Parsec String u FileDesc
syntax s = do
    void $ keyword "syntax"
    void $ symbol '='
    void $ char '"'
    void $ string "proto2"
    void $ char '"'
    void $ symbol ';'
    return s


package :: FileDesc -> Parsec String u FileDesc
package s = do
    void $  keyword "package"
    val  <- (CU.toDotted . fmap CU.toTitleWord . CU.fromDotted) <$> dString
    void $  symbol ';'
    return $ FileDesc.setPackage val s


fileOption :: FileDesc -> Parsec String u FileDesc
fileOption s = choice
    [ try $ fileCcGenericServices s
    , try $ fileDeprecated s
    , try $ fileGoPackage s
    , try $ fileJavaGenerateEqualsAndHash s
    , try $ fileJavaGenericServices s
    , try $ fileJavaPackage s
    , try $ fileJavaOuterClassname s
    , try $ fileJavaMultipleFiles s
    , try $ fileJavaStringCheckUtf8 s
    , try $ fileOptimizeFor s
    , try $ filePyGenericServices s
    ]


fileCcGenericServices :: FileDesc -> Parsec String u FileDesc
fileCcGenericServices s =
    getFileOption "cc_generic_services" bool >> return s


fileDeprecated :: FileDesc -> Parsec String u FileDesc
fileDeprecated s =
    getFileOption "deprecated" bool >> return s


fileGoPackage :: FileDesc -> Parsec String u FileDesc
fileGoPackage s =
    getFileOption "go_package" qString >> return s


fileJavaGenerateEqualsAndHash :: FileDesc -> Parsec String u FileDesc
fileJavaGenerateEqualsAndHash s =
    getFileOption "java_generate_equals_and_hash" bool >> return s


fileJavaGenericServices :: FileDesc -> Parsec String u FileDesc
fileJavaGenericServices s =
    getFileOption "java_generic_services" bool >> return s


fileJavaMultipleFiles :: FileDesc -> Parsec String u FileDesc
fileJavaMultipleFiles s =
    getFileOption "java_multiple_files" bool >> return s


fileJavaOuterClassname :: FileDesc -> Parsec String u FileDesc
fileJavaOuterClassname s =
    getFileOption "java_outer_classname" qString >> return s


fileJavaPackage :: FileDesc -> Parsec String u FileDesc
fileJavaPackage s =
    getFileOption "java_package" qString >> return s


fileJavaStringCheckUtf8 :: FileDesc -> Parsec String u FileDesc
fileJavaStringCheckUtf8 s =
    getFileOption "java_string_check_utf8" bool >> return s


fileOptimizeFor :: FileDesc -> Parsec String u FileDesc
fileOptimizeFor s =
    getFileOption "optimize_for" optimizeMode >> return s


optimizeMode :: Parsec String u OptimizeMode
optimizeMode = choice
    [ string "SPEED"        $> OptimizeMode.Speed
    , string "CODE_SIZE"    $> OptimizeMode.CodeSize
    , string "LITE_RUNTIME" $> OptimizeMode.LiteRuntime
    ]


filePyGenericServices :: FileDesc -> Parsec String u FileDesc
filePyGenericServices s =
    getFileOption "py_generic_services" bool >> return s


getFileOption :: String -> Parsec String u t -> Parsec String u t
getFileOption name parser = do
    void $ keyword "option"
    void $ keyword name
    void $ symbol '='
    val <- parser
    void $ symbol ';'
    return val


-- MessageDesc functions
messageDesc :: FileDesc -> Parsec String u FileDesc
messageDesc s = do
    void $  keyword "message"
    name <- messageName
    void $  symbol '{'
    desc <- messageContent $ MessageDesc.new name
    return $ FileDesc.addMessageDesc desc s


subMessageDesc :: MessageDesc -> Parsec String u MessageDesc
subMessageDesc s = do
    void $  keyword "message"
    name <- messageName
    void $  symbol '{'
    desc <- messageContent $ MessageDesc.new name
    return $ MessageDesc.addMessageDesc desc s


messageContent :: MessageDesc -> Parsec String u MessageDesc
messageContent s = messageEnd s <|> (choice
    [ try $ fieldDesc s
    , try $ messageOption s
    , try $ subMessageDesc s
    , try $ subEnumDesc s
    ] >>= messageContent)


messageEnd :: MessageDesc -> Parsec String u MessageDesc
messageEnd s = symbol '}' >> return s


messageName :: Parsec String u String
messageName = identifier


messageOption :: MessageDesc -> Parsec String u MessageDesc
messageOption s = choice
        [ try $ messageDeprecated s
        , try $ messageSetWireFormat s
        , try $ noStandardDescriptorAccessor s
        ]


messageDeprecated :: MessageDesc -> Parsec String u MessageDesc
messageDeprecated s =
    getMessageOption "deprecated" bool >> return s


messageSetWireFormat :: MessageDesc -> Parsec String u MessageDesc
messageSetWireFormat s =
    getMessageOption "message_set_wire_format" bool >> return s


noStandardDescriptorAccessor :: MessageDesc -> Parsec String u MessageDesc
noStandardDescriptorAccessor s =
    getMessageOption "no_standard_descriptor_accessor" bool >> return s


getMessageOption :: String -> Parsec String u t -> Parsec String u t
getMessageOption name parser = do
    void $ keyword "option"
    void $ keyword name
    void $ symbol '='
    val <- parser
    void $ symbol ';'
    return val


-- EnumDesc functions
enumDesc :: FileDesc -> Parsec String u FileDesc
enumDesc s = do
    void $  keyword "enum"
    name <- enumName
    void $  symbol '{'
    desc <- enumContent $ EnumDesc.new name
    return $ FileDesc.addEnumDesc desc s


subEnumDesc :: MessageDesc -> Parsec String u MessageDesc
subEnumDesc s = do
    void $  keyword "enum"
    name <- enumName
    void $  symbol '{'
    desc <- enumContent $ EnumDesc.new name
    return $ MessageDesc.addEnumDesc desc s


enumContent :: EnumDesc -> Parsec String u EnumDesc
enumContent s = enumEnd s <|> (choice
    [ try $ enumOption s
    , try $ enumValueDesc s
    ] >>= enumContent)


enumEnd :: EnumDesc -> Parsec String u EnumDesc
enumEnd s = symbol '}' >> return s


enumName :: Parsec String u String
enumName = identifier


enumOption :: EnumDesc -> Parsec String u EnumDesc
enumOption s = choice
    [ try $ enumAllowAlias s
    , try $ enumDeprecated s
    ]


enumAllowAlias :: EnumDesc -> Parsec String u EnumDesc
enumAllowAlias s = do
    val <- getEnumOption "allow_alias" bool
    return $ EnumDesc.setAllowAlias val s


enumDeprecated :: EnumDesc -> Parsec String u EnumDesc
enumDeprecated s =
    getEnumOption "deprecated" bool >> return s


getEnumOption :: String -> Parsec String u t -> Parsec String u t
getEnumOption name parser = do
    void $ keyword "option"
    void $ keyword name
    void $ symbol '='
    val <- parser
    void $ symbol ';'
    return val


enumValueDesc :: EnumDesc -> Parsec String u EnumDesc
enumValueDesc s = do
    name   <- enumValueName
    void   $  symbol '='
    number <- enumValueNumber
    void   $  symbol ';'
    return $ EnumDesc.addValueDesc (EnumValueDesc.new name number) s


enumValueName :: Parsec String u String
enumValueName = fmap (CU.toPascal . CU.fromSnake) identifier


enumValueNumber :: Parsec String u Int32
enumValueNumber = int32


-- FieldDesc functions
fieldDesc :: MessageDesc -> Parsec String u MessageDesc
fieldDesc s = do
    flabel <- fieldLabel
    tname  <- typeName
    name   <- fieldName
    void   $  symbol '='
    fnum   <- fieldNumber
    new    <- fieldOptions (FieldDesc.new name fnum flabel tname)
    void   $  symbol ';'
    return $ MessageDesc.addField new s


fieldLabel :: Parsec String u Label
fieldLabel = choice
    [ try $ keyword "optional" $> Label.Optional
    , try $ keyword "required" $> Label.Required
    , try $ keyword "repeated" $> Label.Repeated
    ]


typeName :: Parsec String u String
typeName = identifier


fieldName :: Parsec String u String
fieldName = fmap (CU.toCamel . CU.fromSnake) identifier


fieldNumber :: Parsec String u Int32
fieldNumber = int32


fieldOptions :: FieldDesc -> Parsec String u FieldDesc
fieldOptions s = option s (do
    void $  symbol '['
    s'   <- fieldOption s
    void $  symbol ']'
    return s'
    )


fieldOption :: FieldDesc -> Parsec String u FieldDesc
fieldOption s = choice
    [ try $ fieldDefault s
    , try $ fieldDeprecated s
    , try $ fieldLazy s
    , try $ fieldPacked s
    ]


fieldDefault :: FieldDesc -> Parsec String u FieldDesc
fieldDefault s = do
    val <- getFieldOption "default" defaultValue
    return $ FieldDesc.setDefaultValue val s


fieldDeprecated :: FieldDesc -> Parsec String u FieldDesc
fieldDeprecated s =
    getFieldOption "deprecated" bool >> return s


fieldLazy :: FieldDesc -> Parsec String u FieldDesc
fieldLazy s =
    getFieldOption "lazy" bool >> return s


fieldPacked :: FieldDesc -> Parsec String u FieldDesc
fieldPacked s = do
    val <- getFieldOption "packed" bool
    return $ FieldDesc.setPacked val s


getFieldOption :: String -> Parsec String u t -> Parsec String u t
getFieldOption name parser = do
    void $ keyword name
    void $ symbol '='
    parser


-- General functions
bool :: Parsec String u Bool
bool = choice
    [try (keyword "FALSE") $> False
    ,keyword "False"       $> False
    ,keyword "false"       $> False
    ,try (keyword "TRUE")  $> True
    ,keyword "True"        $> True
    ,keyword "true"        $> True
    ]


defaultValue :: Parsec String u String
defaultValue = lexeme (manyTill anyChar (lookAhead $ char ']'))


dot :: Parsec String u Char
dot = char '.'


dQuote :: Parsec String u Char
dQuote = lexeme $ char '"'


dString :: Parsec String u String
dString = lexeme ((:) <$> startChar <*> many nextChar)
  where
    startChar = letter <|> underscore
    nextChar  = startChar <|> digit <|> dot


eol :: Parsec String u String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"


identifier :: Parsec String u String
identifier = lexeme ((:) <$> startChar <*> many nextChar)
  where
    startChar = letter <|> underscore
    nextChar  = startChar <|> digit


int32 :: Parsec String u Int32
int32 = read <$> lexeme ((:) <$> char '0' <*> zeroStart
                         <|> many1 digit)
  where
    zeroStart = choice [ (:) <$> oneOf "xX" <*> many1 hexDigit
                       , (:) <$> oneOf "oO" <*> many1 octDigit
                       , many digit
                       ]


keyword :: String -> Parsec String u String
keyword k = lexeme $ string k


lexeme :: Parsec String u a -> Parsec String u a
lexeme p = p <* whitespace


qString :: Parsec String u String
qString = dQuote *> many (noneOf "\"") <* dQuote


symbol :: Char -> Parsec String u Char
symbol s = lexeme $ char s


underscore :: Parsec String u Char
underscore = char '_'


whitespace :: Parsec String u ()
whitespace = choice
    [ whiteChar *> whitespace
    , comment *> whitespace
    , return ()
    ]
  where
    comment = try (string "//") *> manyTill anyChar (void eol  <|> eof)
    whiteChar = void $ many1 (oneOf " \t\n\r")
