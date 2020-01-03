module Data.Serialize.Encode

open FStar.Tactics
module L = FStar.List.Tot
module LP = FStar.List.Pure

open Data.Serialize.Helpers
open Data.Serialize.Helpers.Serialized
open Data.Serialize.Types
open Data.Serialize.Rep


let transform_name_encode' (n: name): Tac name
  = nameCurMod' n (fun x -> x ^ "_serialize_encode_chainable")
let transform_name_encode (n: name): Tac name
  = nameCurMod' n (fun x -> x ^ "_serialize_encode")


let rec generateDecodeSerialize_term_for_argSumup
  (args_fun: list binder)
  (arg: argSumup (L.length args_fun))
  : Tac term // value -> serialized -> serialized
  = match arg with
  | AS_Int -> (`appendInt) 
  | AS_String -> (`appendString)
  | AS_Bool -> (`appendBool)
  | AS_List typ -> call1 (`appendList) (generateDecodeSerialize_term_for_argSumup args_fun typ)
  | AS_TVar i -> (binder_to_term (L.index args_fun i))
  | AS_Inductive tname args ->
    let f = name_to_term (transform_name_encode' tname) in
    mk_e_app f (map (generateDecodeSerialize_term_for_argSumup args_fun) args)

let generateEncodeSerialize_term_for_inductiveSumup
    (s: inductiveSumup)
  : Tac term
  = let {iVars; iName; iCons} = s in
    let encoders: (x: list binder {L.length x == s.iVars})
      = admit (); map (fun _ -> pack_binder (fresh_bv (`_)) Q_Explicit) (mkList 0 (s.iVars-1)) in
    let inp = fresh_binder (`_) in
    let initialSerialized = fresh_binder (`serialized) in
    mk_abs (encoders @ [inp; initialSerialized]) (
      mkMatchInductive s (binder_to_term inp) 
      ( map 
        (fun (i, (cName, cArgs))
         -> let f (bvs: list bv): Tac term
             = call2 (`appendInt) (pack (Tv_Const (C_Int i))) (
                   fold_left
                   ( fun s (j, arg) 
                     -> call2 
                       (generateDecodeSerialize_term_for_argSumup encoders arg)
                       (bv_to_term (L.index bvs j))
                       s
                   )
                   (binder_to_term initialSerialized)
                   (withIndex cArgs)
               )
           in f
        )
        (withIndex iCons)
      )
    )

let generateEncodeSerialize_for_inductiveSumup
    (s: inductiveSumup)
  : Tac decls
  = let {iVars; iName; iCons} = s in
    let encoders
      = map (fun _ -> pack_binder (fresh_bv (`_)) Q_Explicit) (mkList 0 (s.iVars-1)) in
    let inp = fresh_binder (`_) in
    let initialSerialized = fresh_binder (`serialized) in
    
    let body = generateEncodeSerialize_term_for_inductiveSumup s in
    let mk (fName: _ -> Tac _) = Sg_Let true (pack_fv (fName s.iName)) [] (`_) in
    let sg
    = mk transform_name_encode (
         mk_abs (encoders @ [inp])
         (mk_e_app body (map binder_to_term encoders @ [binder_to_term inp; `emptySerialized]))
      ) in
    let sg' = mk transform_name_encode' body
    in [pack_sigelt sg'; pack_sigelt sg]

let generateEncodeSerialize
    (name: fv)
  : Tac decls
  = let s = makeGenericRep name in
    generateEncodeSerialize_for_inductiveSumup s



