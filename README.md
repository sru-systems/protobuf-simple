# Protobuf-simple

Protobuf-simple is an Haskell implementation of Google's Protocol Buffers
version 2 (proto2) with an emphasis on simplicity. The implementation consists
of a library for the encoding and decoding of data and an executable for
generating Haskell types from proto files.



## Supported Data Types

The following data types are supported by protobuf-simple.

|Proto Type |Haskell Type          |
|:----------|:---------------------|
|bool       |Bool                  |
|bytes      |Data.ByteString.Lazy  |
|double     |Double                |
|enum       |Sum type              |
|fixed32    |Word32                |
|fixed64    |Word64                |
|float      |Float                 |
|int32      |Int32                 |
|int64      |Int64                 |
|message    |Product type / newtype|
|sfixed32   |Int32                 |
|sfixed64   |Int64                 |
|sint32     |Int32                 |
|sint64     |Int64                 |
|string     |Data.Text.Lazy        |
|uint32     |Word32                |
|uint64     |Word64                |



## Generate Haskell Types

The `protoc` executable can be used to generate Haskell types from a proto
file. In fact, the types that are used in the tests are generated with the
following command:

```
$ protoc data/Types.proto
```


## Encoding and Decoding

In the example below, the CustomType is a Haskell type that is generated with
the `protoc` executable. The `encCustomType` function encodes a CustomType
into a ByteString. The `decCustomType` function decodes a ByteString into
either a CustomType or an error.

```
module Codec where

import Data.ByteString.Lazy (ByteString)
import Data.ProtoBuf (decode, encode)
import Types.CustomType (CustomType(..))

encCustomType :: CustomType -> ByteString
encCustomType = encode

decCustomType :: ByteString -> Either String CustomType
decCustomType = decode
```



## Compatibility

Besides testing that decoding inverses encoding, the compatibility with other
implementations is tested by decoding binary files that were created with
protobuf-net (C#).



## Not Implemented

The following Protocol Buffers features are currently not implemented:

- Importing definitions.
- Groups.
- Declaring extensions in messages.
- Declaring nested extensions in messages.
- Oneof feature.
- Associative maps.
- Defining services.
- Custom options.



## License

Protobuf-simple is released under the MIT License.

