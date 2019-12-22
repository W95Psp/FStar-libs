(**
This modules provides (1) a generic representation (namely `Data.Generic.Types.serialized`) and (2) meta programs that generates encoders and decoders for any non-mutually recursive datatype.

The generic representation carefully uses only types for which there exists [embbedings](https://github.com/FStarLang/FStar/wiki/Compiling-tactics#embeddings-two-way-communication-between-normalizer-and-native-tactics). This is very useful for writing F* plugins or native tactics that deals with custom datatypes. 

@summary Embeddings-compatible generic representation and (de)serializer
*)
module Data.Generics

open FStar.Tactics

module T = Data.Generics.Types
module E = Data.Generics.Encode
module D = Data.Generics.Decode

let serialized = T.serialized

(**
## Deriving encoders and decoders for a given type

For a type parametrized by `n` arguments, the derived encoder and decoders take `n` sub-encoders/decoders.

*)
[@"HEY"]
val generateEncodeGeneric: (name: fv) -> Tac decls
let generateEncodeGeneric = E.generateEncodeGeneric
let generateDecodeGeneric = D.generateDecodeGeneric

open Data.Generics.Helpers
let atts ()
  : Tac (list term)
  = 
    let Some t = admit (); lookup_typ (top_env ()) (inspect_fv (fvOf (`generateEncodeGeneric)))
    in sigelt_attrs t

let x: list term = _ by (
  let x = atts () in
  exact (quote x)
)


