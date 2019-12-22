module Data.Generic

open FStar.Tactics
module L = FStar.List.Tot

open Data.Generic.Helpers
open Data.Generic.Helpers.Serialized
open Data.Generic.Types
open Data.Generic.Rep
 
module E = Data.Generic.Encode
module D = Data.Generic.Decode

let generateEncodeGeneric = E.generateEncodeGeneric
let generateDecodeGeneric = D.generateDecodeGeneric
 
