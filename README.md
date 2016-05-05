# Protobuf-simple

An Haskell implementation of Google's Protocol Buffers version 2 with an
emphasis on simplicity. The implementation consists of a library for encoding
and decoding of data and the `protoc` executable for generating Haskell types
from proto files. In fact, the types that are used in the tests are generated
with the following command:

```
$ protoc data/Types.proto
```

In the example below, the `CustomType` is a Haskell type that was generated
with the `protoc` executable. The `encCustomType` function encodes a CustomType
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

The library exposes two modules, Data.ProtoBuf, which is used for encoding and
decoding and Data.ProtoBufInt, which is an internal module that is used by the
generated types.


## Supported Data Types

The following Protocol Buffer types, with their Haskell counterparts, are
supported:

- bool: Bool
- bytes: ByteString (lazy)
- double: Double
- enum: Sum-type
- fixed32: Word32
- fixed64: Word64
- float: Float
- int32: Int32
- int64: Int64
- message: Product-type or newtype
- sfixed32: Int32
- sfixed64: Int64
- sint32: Int32
- sint64: Int64
- string: Text (lazy)
- uint32: Word32
- uint64: Word64


## Compatibility

Besides testing that decoding inverses encoding, the compatibility with other
implementations is tested by decoding binary files that were created with
protobuf-net (C#).


## Other Implementations

There are currently multiple Protocol Buffers implementations available. This
library was created for the following reasons. Firstly, I wanted to use
Data.Text for the string type instead of Data.ByteString as the
protocol-buffers library does. Secondly, I wanted to use newtypes for messages
with only one field. Finally, I wanted to use simpler data types than the
protobuf library does.

For example, the protobuf library uses the following (example from the manual):

```
data Foo = Foo
  { field1 :: Required 1 (Value Int64)
  , field2 :: Optional 2 (Value Text)
  , field3 :: Repeated 3 (Value Bool)
  } deriving (Generic, Show)
```

Whereas protobuf-simple would use the following:

```
data Foo = Foo
  { field1 :: Int64
  , field2 :: Maybe Text
  , field3 :: Seq Bool
  } deriving (Show, Eq, Ord)
```


## Not Implemented

The following Protocol Buffers features are currently not implemented:

- Importing definitions
- Groups
- Declaring extensions in messages
- Declaring nested extensions in messages
- Oneof feature
- Associative maps
- Defining services
- Custom options


## License

Protobuf-simple is released under the MIT License.

