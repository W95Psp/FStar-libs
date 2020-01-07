module Data.JSON

module C = Data.JSON.Parser
module T = Data.JSON.Types
module S = Data.JSON.Stringify

open Data.Serialize

// Data.Serialize constructions looks for terms in the current module 
// so nat or tuple serializer are not found...
// this is both stupid and... a TODO
let nat_serialize_decode_chainable = nat_serialize_decode_chainable
let nat_serialize_encode_chainable = nat_serialize_encode_chainable
%splice[] (generateSerialize (`tuple2))

%splice[] (generateSerialize (`T.decimalNumber))
%splice[] (generateSerialize (`T.jsonValue))

let jsonValue = T.jsonValue
// let parse_not_native = C.parser
// let stringify_not_native = S.stringify

let stringify' (v: serialized) (spaces: string): string =
  S.stringify (deserialize v) spaces

%splice[] (mk_native_version (`stringify') "stringify" [true;false] false)
%splice[] (mk_native_version (`C.parser) "parse" [false] true)

