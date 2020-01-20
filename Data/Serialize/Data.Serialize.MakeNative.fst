module Data.Serialize.MakeNative

open FStar.Tactics
module L = FStar.List.Tot

open Data.Serialize.Helpers
open Data.Serialize.Helpers.Serialized
open Data.Serialize.Types
open Data.Serialize.Typeclasses
open Data.Serialize.Rep

let _UNSAFE_flag_is_compiled_rewriter (): Lemma (false == true) = admit ()
let _UNSAFE_flag_is_compiled_rewriter_term = `_UNSAFE_flag_is_compiled_rewriter
// [@(postprocess_for_extraction_with (fun _ -> apply_lemma (`_UNSAFE_flag_is_compiled_rewriter)))]
// let flag_is_in_compiled_context = false

// [@plugin]
// let is_this_module_compiled () = flag_is_in_compiled_context

let plugin_term = `plugin

let mk_native_version (f: term) (nativeName: string) (args: list bool) (return: bool) =
  let fT = f in
  let f: fv = fvOf f in
  let flag: name = nameCurMod' (inspect_fv f) (fun _ -> "flag_is_in_compiled_context") in
  let flagT:term = pack (Tv_FVar (pack_fv flag)) in
  let is_this_module_compiled: name = nameCurMod' (inspect_fv f) (fun _ -> "is_this_module_compiled") in
  let is_this_module_compiledT:term = pack (Tv_FVar (pack_fv is_this_module_compiled)) in
  let flag_generate = match lookup_typ (top_env ()) flag with
    | Some _ -> []
    | None   -> (
      [ set_sigelt_attrs [`(postprocess_for_extraction_with
                         (fun _ -> apply_lemma _UNSAFE_flag_is_compiled_rewriter_term))]
        (pack_sigelt (
          Sg_Let false (pack_fv flag) [] (`bool) (`false)
        ))
      ; set_sigelt_attrs [`plugin]
        (pack_sigelt (
          Sg_Let false (pack_fv is_this_module_compiled) [] (`(unit -> bool))
            (mk_abs [fresh_binder (`unit)] flagT)
        ))]
    )
  in
  let _  = lookup_typ (top_env ()) flag in
  let all = map (fun t -> fresh_binder (`_), t) args in
  let binders = L.map fst all in
  let body = mk_e_app fT (
    map (fun (b, isS) -> 
      if isS
      then call1 (`deserialize) (binder_to_term b)
      else binder_to_term b
      ) all
  ) in
  let body = if return then call1 (`serialize) body else body in
  let f': name = nameCurMod' (inspect_fv f) (fun s -> s ^ "_" ^ nativeName ^ "_native_helper") in
  let body'_if_fstar: term = mk_e_app (pack (Tv_FVar (pack_fv f'))) (
    map (fun (b, isS) -> 
      if isS
      then call1 (`serialize) (binder_to_term b)
      else binder_to_term b
      ) all
  ) in
  let body'_if_fstar: term = if return then call1 (`deserialize) body'_if_fstar else body'_if_fstar in
  let body' = 
    // (pack (mkIfThenElse (call2 (`(fun (x y: bool) -> x || (~y))) flagT is_this_module_compiledT )
    //     ( mk_e_app fT
    //       (map binder_to_term binders)
    //     )
        body'_if_fstar
    //   )
    // )
  in
  let f'': name = nameCurMod' (inspect_fv f) (fun s -> nativeName) in
  let se = 
    Sg_Let false (pack_fv f') []
           (`_) (mk_abs binders body)
  in let se' = 
    Sg_Let false (pack_fv f'') []
           (tc (top_env ()) fT)
           (mk_abs binders body')
  in let ll = flag_generate @ [
    set_sigelt_attrs [plugin_term] (pack_sigelt se)
  ; pack_sigelt se'
 ] in
 // dump (term_to_string (quote ll));
 ll


