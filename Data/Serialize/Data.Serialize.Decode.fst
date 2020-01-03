module Data.Serialize.Decode

open FStar.Tactics
module L = FStar.List.Tot

open Data.Serialize.Helpers
open Data.Serialize.Helpers.Serialized
open Data.Serialize.Types
open Data.Serialize.Rep

let transform_name_decode' (n: name): Tac name
  = nameCurMod' n (fun x -> x ^ "_serialize_decode_chainable")
let transform_name_decode (n: name): Tac name
  = nameCurMod' n (fun x -> x ^ "_serialize_decode")

let rec change_last (f: 'a -> 'a) (l: list 'a)
  : list 'a
  = match l with
  | [] -> []
  | hd::[] -> [f hd]
  | hd::tl -> hd::change_last f tl

let rec generateDecodeSerialize_term_for_argSumup
  (args_fun: list binder)
  (arg: argSumup (L.length args_fun))
  : Tac term // serialized -> (X * serialized)
  = match arg with
  | AS_Int -> `readInt
  | AS_String -> `readString
  | AS_Bool -> `readBool
  | AS_List typ -> call1 (`readList)
                (generateDecodeSerialize_term_for_argSumup args_fun typ)
  | AS_TVar i -> binder_to_term (L.index args_fun i)
  | AS_Inductive tname args ->
    let f = name_to_term (transform_name_decode' tname) in
    mk_e_app f (map (generateDecodeSerialize_term_for_argSumup args_fun) args)

let id_tac_term (t: term): Tac term = t

let x = %[123] << %[234]

let xx: term = _ by (
  let t: term = `(%[(123, 42)] << %[234]) in
  exact (quote t)
)


let generateDecodeSerialize_term_for_consSumup #n (encoders: list _ {L.length encoders = n}) (cons: consSumup n) (serialized_inp: bv)
  : Tac (constructor_serialized: term)
  = let consName, args = cons in
    let bvs_args, bv_serialized, mkTerm
      = Tactics.fold_right
          (fun arg ( bvs_args, bv_serialized, mkTerm) -> 
            let result = call1 
              (generateDecodeSerialize_term_for_argSumup encoders arg)
              // (bv_to_term bv_serialized)
              (add_admit_decr_lex (bv_to_term bv_serialized) (bv_to_term bv_serialized))
            in
            let mkTerm', (bv_arg, bv_serialized) = mkLet_tup' result in
            let mkTerm: term -> Tac term = mkTerm in
            bv_arg::bvs_args, bv_serialized, 
            ( let mkTerm'' (inner: term): Tac term = 
                  mkTerm (mkTerm' inner)
              in mkTerm'')
          ) args ([], serialized_inp, id_tac_term)
    in
    mkTerm (
      let branch = mk_e_app (name_to_term consName) (map bv_to_term bvs_args) in
      call2 (`Mktuple2) branch (bv_to_term bv_serialized)
    )

let generateDecodeSerialize_term_for_inductiveSumup
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
                generateDecodeSerialize_term_for_consSumup #s.iVars decoders cons inp
              ) iCons
          in 
          mkMatchInt (bv_to_term consIndex) branches
        )
    )

let generateDecodeSerialize_for_inductiveSumup
    (s: inductiveSumup)
  : Tac decls
  = let decoders: (x: list binder {L.length x == s.iVars})
      = admit ();
        map (fun _ -> pack_binder (fresh_bv (`_)) Q_Explicit
        ) (mkList 0 (s.iVars-1))
    in
    let inp = fresh_binder (`serialized) in
    // decoders @ [inp]
    let body = generateDecodeSerialize_term_for_inductiveSumup s in
    let sg'
    = Sg_Let true (pack_fv (transform_name_decode' s.iName)) [] (`_)
             body
    in let sg
    = Sg_Let true (pack_fv (transform_name_decode s.iName)) [] (`_)
             (mk_abs (decoders @ [inp]) (
               call1 (`fst) (mk_e_app body (map binder_to_term (decoders @ [inp])))
             ))
    in [pack_sigelt sg'; pack_sigelt sg]

let generateDecodeSerialize
    (name: fv)
  : Tac decls
  = let s = makeGenericRep name in
    generateDecodeSerialize_for_inductiveSumup s

