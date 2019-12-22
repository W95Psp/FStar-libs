module Data.Generic.Decode

open FStar.Tactics
module L = FStar.List.Tot

open Data.Generic.Helpers
open Data.Generic.Helpers.Serialized
open Data.Generic.Types
open Data.Generic.Rep

let transform_name_decode' (n: name): Tac name
  = nameCurMod' n (fun x -> x ^ "_generic_decode_chainable")
let transform_name_decode (n: name): Tac name
  = nameCurMod' n (fun x -> x ^ "_generic_decode")

let rec generateDecodeGeneric_term_for_argSumup
  (args_fun: list binder)
  (arg: argSumup (L.length args_fun))
  : Tac term // serialized -> (X * serialized)
  = match arg with
  | AS_Int -> `readInt
  | AS_String -> `readString
  | AS_Bool -> `readBool
  | AS_List typ -> call1 (`readList)
                (generateDecodeGeneric_term_for_argSumup args_fun typ)
  | AS_TVar i -> binder_to_term (L.index args_fun i)
  | AS_Inductive tname args ->
    let f = name_to_term (transform_name_decode' tname) in
    mk_e_app f (map (generateDecodeGeneric_term_for_argSumup args_fun) args)

let id_tac_term (t: term): Tac term = t

let generateDecodeGeneric_term_for_consSumup #n (encoders: list _ {L.length encoders = n}) (cons: consSumup n) (serialized_inp: bv)
  : Tac (constructor_serialized: term)
  = let consName, args = cons in
    let bvs_args, bv_serialized, mkTerm
      = Tactics.fold_left 
          (fun ( bvs_args, bv_serialized, mkTerm) arg -> 
            let result = call1 
              (generateDecodeGeneric_term_for_argSumup encoders arg)
              (bv_to_term bv_serialized)
            in
            let mkTerm', (bv_arg, bv_serialized) = mkLet_tup' result in
            let mkTerm: term -> Tac term = mkTerm in
            bv_arg::bvs_args, bv_serialized, 
            ( let mkTerm'' (inner: term): Tac term = 
                  mkTerm' (mkTerm inner)
              in mkTerm'')
          ) ([], serialized_inp, id_tac_term) args
    in
    mkTerm (
      let branch = mk_e_app (name_to_term consName) (map bv_to_term bvs_args) in
      call2 (`Mktuple2) branch (bv_to_term bv_serialized)
    )

let generateDecodeGeneric_term_for_inductiveSumup
    (s: inductiveSumup)
  : Tac term
  = let {iVars; iName; iCons} = s in
    let decoders: (x: list binder {L.length x == s.iVars})
      = admit ();
        map (fun _ -> pack_binder (fresh_bv (`_)) Q_Explicit
        ) (mkList 0 (s.iVars-1))
    in
    let inp = fresh_binder (`serialized) in
    mk_abs (decoders @ [inp]) (
      mkLet_tup
        (call1 (`readInt) inp)
        (fun consIndex inp -> 
          let branches
            = map (fun cons ->
                generateDecodeGeneric_term_for_consSumup #s.iVars decoders cons inp
              ) iCons
          in 
          mkMatchInt (bv_to_term consIndex) branches
        )
    )

let generateDecodeGeneric_for_inductiveSumup
    (s: inductiveSumup)
  : Tac decls
  = let decoders: (x: list binder {L.length x == s.iVars})
      = admit ();
        map (fun _ -> pack_binder (fresh_bv (`_)) Q_Explicit
        ) (mkList 0 (s.iVars-1))
    in
    let inp = fresh_binder (`serialized) in
    // decoders @ [inp]
    let body = generateDecodeGeneric_term_for_inductiveSumup s in
    let sg'
    = Sg_Let false (pack_fv (transform_name_decode' s.iName)) [] (`_)
             body
    in let sg
    = Sg_Let false (pack_fv (transform_name_decode s.iName)) [] (`_)
             (mk_abs (decoders @ [inp]) (
               call1 (`fst) (mk_e_app body (map binder_to_term (decoders @ [inp])))
             ))
    in [pack_sigelt sg; pack_sigelt sg']

let generateDecodeGeneric
    (name: fv)
  : Tac decls
  = let s = makeGenericRep name in
    generateDecodeGeneric_for_inductiveSumup s



