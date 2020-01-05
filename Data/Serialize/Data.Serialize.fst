(**
This modules provides (1) a generic representation (namely `Data.Generic.Types.serialized`) and (2) meta programs that generates encoders and decoders for any non-mutually recursive datatype.

The generic representation carefully uses only types for which there exists [embbedings](https://github.com/FStarLang/FStar/wiki/Compiling-tactics#embeddings-two-way-communication-between-normalizer-and-native-tactics). This is very useful for writing F* plugins or native tactics that deals with custom datatypes. 

@summary Embeddings-compatible generic representation and (de)serializer
*)
module Data.Serialize

open FStar.Tactics
open FStar.Tactics.Typeclasses

module T = Data.Serialize.Types
module E = Data.Serialize.Encode
module D = Data.Serialize.Decode
module Helpers = Data.Serialize.Helpers
module HS = Data.Serialize.Helpers.Serialized

let serialized = T.serialized
let tserialized = T.tserialized

module TC = Data.Serialize.Typeclasses
module Rep = Data.Serialize.Rep

let fvOf = Helpers.fvOf

let name_serialize_decode_chainable = HS.readName
let name_serialize_encode_chainable = HS.appendName
[@tcinstance]
let nameHasSerialize: TC.hasSerialize name = {
   TC.serialize_chainable   = name_serialize_encode_chainable
 ; TC.deserialize_chainable = name_serialize_decode_chainable
}

let int_serialize_decode_chainable = HS.readInt
let int_serialize_encode_chainable = HS.appendInt
[@tcinstance]
let intHasSerialize: TC.hasSerialize int = {
   TC.serialize_chainable   = int_serialize_encode_chainable
 ; TC.deserialize_chainable = int_serialize_decode_chainable
}

let string_serialize_decode_chainable = HS.readString
let string_serialize_encode_chainable = HS.appendString
[@tcinstance]
let stringHasSerialize: TC.hasSerialize string = {
   TC.serialize_chainable   = string_serialize_encode_chainable
 ; TC.deserialize_chainable = string_serialize_decode_chainable
}

let bool_serialize_decode_chainable = HS.readBool
let bool_serialize_encode_chainable = HS.appendBool
[@tcinstance]
let boolHasSerialize: TC.hasSerialize bool = {
   TC.serialize_chainable   = bool_serialize_encode_chainable
 ; TC.deserialize_chainable = bool_serialize_decode_chainable
}

let list_serialize_decode_chainable = HS.readList
let list_serialize_encode_chainable = HS.appendList
[@tcinstance]
let listHasSerialize a [| TC.hasSerialize a |]: TC.hasSerialize (list a) = {
   TC.serialize_chainable   = list_serialize_encode_chainable TC.serialize_chainable
 ; TC.deserialize_chainable = list_serialize_decode_chainable TC.deserialize_chainable
}

let generateSerialize' = TC.generateSerialize'
let generateSerialize = TC.generateSerialize

%splice[] (generateSerialize (`option))
%splice[] (generateSerialize (`either))
%splice[] (generateSerialize (`tuple2))
%splice[] (generateSerialize (`tuple3))
%splice[] (generateSerialize (`tuple4))
%splice[] (generateSerialize (`tuple5))
%splice[] (generateSerialize (`tuple6))

let nat_serialize_decode_chainable s: (nat * _)
  = match int_serialize_decode_chainable s with
  | i, s -> if i >= 0 then i, s
           else Helpers.mkerror "nat_serialize_decode: got an negative integer"
let nat_serialize_encode_chainable: nat -> _ -> _ = int_serialize_encode_chainable

let hasSerialize = TC.hasSerialize 
let serialize   #a [| hasSerialize a |] v = TC.serialize   #a v
let deserialize #a [| hasSerialize a |] v = TC.deserialize #a v

(**
## Deriving encoders and decoders for a given type

For a type parametrized by `n` arguments, the derived encoder and decoders take `n` sub-encoders/decoders.

*)
val generateEncodeSerialize: (name: fv) -> Tac decls
let generateEncodeSerialize = E.generateEncodeSerialize
val generateDecodeSerialize: (name: fv) -> Tac decls
let generateDecodeSerialize = D.generateDecodeSerialize

