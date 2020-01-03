module Data.Serialize.Typeclasses

open FStar.Tactics
open FStar.Tactics.Typeclasses

open Data.Serialize.Types
module L = FStar.List.Tot
module E = Data.Serialize.Encode
module D = Data.Serialize.Decode
open Data.Serialize.Helpers
module HS = Data.Serialize.Helpers.Serialized

class hasSerialize a = {
    serialize_chainable: a -> serialized -> serialized
  ; deserialize_chainable: serialized -> (a * serialized)
}

[@tcinstance]
let intHasSerialize: hasSerialize int = {
   serialize_chainable   = HS.appendInt
 ; deserialize_chainable = HS.readInt 
}

[@tcinstance]
let listIntHasSerialize: #(a: Type) -> #(i: hasSerialize a) -> hasSerialize (list a)
  = fun #a (#i: hasSerialize a) -> {
   serialize_chainable   = HS.appendList serialize_chainable 
 ; deserialize_chainable = HS.readList deserialize_chainable 
}

let xx = serialize_chainable [1;3]

let makeHasSerializeInstance
  (s: inductiveSumup)
  (encode decode: fv)
  : Tac decls
  = let name = pack_fv (nameCurMod' s.iName (fun x -> x ^ "_hasSerialize")) in
    let {iVars; iName; iCons} = s in
    let types = map (fun _ -> pack_binder (fresh_bv (`Type)) Q_Explicit) (mkList 0 (s.iVars-1)) in
    let instances
      = map (fun t -> 
            pack_binder
              (fresh_bv 
                (call1 (`hasSerialize)
                  (binder_to_term t)
                )
              )
              (Q_Meta (`tcresolve))
        ) types in
    let se: sigelt_view
      = Sg_Let false name [] (`_)
        (mk_abs (types @ instances) 
          (Helpers.call2 (`MkhasSerialize)
            (mk_e_app (pack (Tv_FVar encode)) (map (fun i -> 
              call1 (`__proj__MkhasSerialize__item__serialize_chainable)   (binder_to_term i)
            ) instances))
            (mk_e_app (pack (Tv_FVar decode)) (map (fun i -> 
              call1 (`__proj__MkhasSerialize__item__deserialize_chainable) (binder_to_term i)
            ) instances))
          )
        )
    in
    let se = pack_sigelt se in
    let se = set_sigelt_attrs [quote tcinstance] se in
    [se]

open Data.Serialize.Rep
let generateSerialize' tfv
  = let rep = makeGenericRep tfv in
      (D.generateDecodeSerialize tfv)
    @ (E.generateEncodeSerialize tfv)
    @ (makeHasSerializeInstance
        rep 
        (pack_fv (E.transform_name_encode' (inspect_fv tfv)))
        (pack_fv (D.transform_name_decode' (inspect_fv tfv)))
      )
let generateSerialize (t: term)
  = generateSerialize' (fvOf t)

let serialize #a [| hasSerialize a |] (v: a): serialized = serialize_chainable v ([], ([], ([], [])))
let deserialize #a [| hasSerialize a |] (v: serialized): a = fst (deserialize_chainable v)

// type myTest a = | A : a -> myTest a
//                 | B : int -> myTest a
//                 | C : list a -> myTest a
//                 | D : a -> a -> myTest a

// %splice[myTest_hasSerialize] (generateSerialize (`myTest))
