module Data.Generics

open FStar.Tactics
module L = FStar.List.Tot

open Data.Generics.Helpers
open Data.Generics.Helpers.Serialized
open Data.Generics.Types
open Data.Generics.Rep
 
module E = Data.Generics.Encode
module D = Data.Generics.Decode

let generateEncodeGeneric = E.generateEncodeGeneric
let generateDecodeGeneric = D.generateDecodeGeneric
 
