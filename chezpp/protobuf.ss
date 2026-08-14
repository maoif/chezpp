(library (chezpp protobuf)
  (export protobuf-encode-varint protobuf-decode-varint
          protobuf-encode-zigzag protobuf-decode-zigzag
          protobuf-encode-fixed32 protobuf-decode-fixed32
          protobuf-encode-fixed64 protobuf-decode-fixed64
          protobuf-encode-field protobuf-encode-message)
  (import (chezpp protobuf wire)))
