(**
This modules provides (1) a generic representation (namely `Data.Generic.Types.serialized`) and (2) meta programs that generates encoders and decoders for any non-mutually recursive datatype.

The generic representation carefully uses only types for which there exists [embbedings](https://github.com/FStarLang/FStar/wiki/Compiling-tactics#embeddings-two-way-communication-between-normalizer-and-native-tactics). This is very useful for writing F* plugins or native tactics that deals with custom datatypes. 

@summary Embeddings-compatible generic representation and (de)serializer
*)
module Data.Serialize

open FStar.Tactics

module T = Data.Serialize.Types
module E = Data.Serialize.Encode
module D = Data.Serialize.Decode
module Helpers = Data.Serialize.Helpers
module HS = Data.Serialize.Helpers.Serialized

let serialized = T.serialized
let tserialized a = T.serialized

let name_serialize_decode = HS.readBool
let name_serialize_encode = HS.appendName

let int_serialize_decode = HS.readInt
let int_serialize_encode = HS.appendInt

let string_serialize_decode = HS.readString
let string_serialize_encode = HS.appendString

let bool_serialize_decode = HS.readBool
let bool_serialize_encode = HS.appendBool

let fvOf = Helpers.fvOf
(**
## Deriving encoders and decoders for a given type

For a type parametrized by `n` arguments, the derived encoder and decoders take `n` sub-encoders/decoders.

*)
val generateEncodeSerialize: (name: fv) -> Tac decls
let generateEncodeSerialize = E.generateEncodeSerialize
val generateDecodeSerialize: (name: fv) -> Tac decls
let generateDecodeSerialize = D.generateDecodeSerialize

let generateSerialize (name: fv)
  : Tac decls
  = generateEncodeSerialize name @ generateDecodeSerialize name

