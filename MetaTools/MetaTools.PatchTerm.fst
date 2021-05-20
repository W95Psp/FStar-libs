/// This module allow one to patch every subterms of a definition and all its dependencies
/// with a given function.
module MetaTools.PatchTerm

open MetaTools.BrowseTerm
open MetaTools.NamesOfTerm
module U = MetaTools.Util
module L = FStar.List.Tot
module Typeclasses = FStar.Tactics.Typeclasses
open FStar.Tactics
open Control.Monoid

let typechecks (t: term): Tac bool
  = Some? (trytac (fun _ -> tc (top_env ()) t))

/// `patch_term_helper` decides whether a term should be patched
/// basically, a term `t` should be patched if `patch t` typechecks
let patch_term_helper
  (patchFunction: term)
  (patchVarParamBinder: binder)
  (rebuildToplevel: term -> Tac term)
  (t: term)
  : Tac (term * (term -> Tac term))
  = let do_patch t: Tac term = mk_e_app patchFunction
      [ `_; t; binder_to_term patchVarParamBinder] in
    let toplevel_patched = 
      Typeclasses.mk_abs [patchVarParamBinder] (rebuildToplevel (do_patch t))
    in
    t, ( if typechecks toplevel_patched
         then (dump ("DID PATCHED :" ^ term_to_string t ^ "\n\n\n" ^ term_to_string toplevel_patched); do_patch) else id_tac    )

let patch_term (patchFunction: term) (patchVarParamBinder: binder) (t: term): Tac term = 
  fst
  (browse_term #unit #unitIsMonoid
    (fun beforeTransform rebuildToplevel boundedVariables parents typeLevel currentTerm -> 
      ( if (beforeTransform = false) || typeLevel
        then currentTerm, id_tac
        else
          patch_term_helper patchFunction patchVarParamBinder rebuildToplevel currentTerm
      ), ()
    ) false t)

/// This function replace given free variables by given terms in a term 
let replace_fv (t: term) (r: list (name * term))
  : Tac term
  = fst (browse_term #unit #unitIsMonoid begin
    fun beforeTransform _ _ _ _ currentTerm
    -> ((if beforeTransform
        then begin
             match inspect currentTerm with
             | Tv_FVar fv ->
               let n = inspect_fv fv in
               ( match L.find (fun (old, _) -> old = n) r with
               | Some (_, replaceTerm) -> replaceTerm
               | _ -> currentTerm
               )
             | _ -> currentTerm
             end
        else currentTerm ), id_tac), ()
  end false t)

let make_patched_name_tot (cur_module_name: name) (n: name {Cons? n}): name =
  cur_module_name @ [U.last n ^ "_patched"]
  
let make_patched_name (n: name): Tac name =
  guard (Cons? n);
  make_patched_name_tot (cur_module ()) n

let make_patched_replacements (t_lvar: term) (n: list name): Tac (list (name * term))
  = map (fun n ->
    n, (
      let t = pack (Tv_FVar (pack_fv (make_patched_name n))) in
      mk_app t [t_lvar, Q_Explicit]
    )
  ) n

// Todo, move somewhere else
let rec zip #a (l: list a) #b (r: list b)
  : list (a * b)
  = match l, r with
  | hl::tl, hr::tr -> (hl, hr)::zip tl tr
  | _ -> []
// Todo, move somewhere else
let isUppercase = function
  |'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'
  |'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'W'|'X'|'Y'
  |'Z' -> true
  | _  -> false
// Todo, move somewhere else
let startsUppercase s = match String.list_of_string s with
  | hd::_ -> isUppercase hd
  | _ -> false
// Todo, move somewhere else
let rec concatOptions (l: list (option 'a)): list 'a
  = match l with
  | [] -> []
  | Some hd::tl -> hd::concatOptions tl
  | _ ::tl -> concatOptions tl
// Todo, move somewhere else
let put_option_ext_tup (tup: 'a * option 'b): option ('a * 'b)
  = match tup with
  | (a, Some b) -> Some (a, b)
  | _ -> None


let pred_blacklist (blacklist: list (bool * name)) (n: name)
  = Some? 
    (L.find (fun (exactMatch, pat) -> 
            if exactMatch
            then pat = n
            else U.prefixOf pat n 
     ) blacklist)

[@plugin]
let patch_term_and_defs (blacklist: list (bool * name)) (globalArgType: term) (patchFunction: term) (def: term)
 : Tac (list ((n: name {Cons? n}) * option (term * term)))
  =
    // generate unique number for the global arg for patch
    let uid = string_of_int (fresh ()) in
    let label_var: bv = pack_bv ({
        bv_ppname = "global_arg_patch_fun_"^uid;
        bv_sort = globalArgType;
        bv_index = 0;
    }) in
    // compute depenencies of `n`
    let g: graph = fv_dependencies_of
    (             blacklist_ulib
      `join_pred` blacklist_uu
      `join_pred` (pred_blacklist blacklist)
      `join_pred` (fun (n: name) -> Cons? n 
                      && startsUppercase (U.last n)
                      )
      ) def
    in
    // h rewrites Sg_Lets (toplevel fun def) 
    let h arg: Tac (option (term * term)) =
      let toplevel_name, dependencies = arg in
      let dependencies = toplevel_name::dependencies in
      let b_lvar: binder = pack_binder label_var Q_Explicit in
      let t_lvar: term = binder_to_term b_lvar in
      let replacements = make_patched_replacements t_lvar dependencies in
      let patch (t: term): Tac term = replace_fv t replacements in
      match U.sglet_of_name toplevel_name with
      | Some (typ, def) ->
        (* BEGIN FORCE TYPE *)
        let inner_bv = fresh_bv typ in
        let def = pack (Tv_Let false [] inner_bv def (bv_to_term inner_bv)) in
        // END FORCE TYPE *)
        Some (
          mk_tot_arr [b_lvar] (patch ( patch_term patchFunction b_lvar typ ))
        , pack (Tv_Abs b_lvar (patch ( patch_term patchFunction b_lvar def )))
        )
      | None -> None
    in
    // TODO, remove admit here (guard or something)
    let names: list (n: name {Cons? n}) = admit (); L.map fst g in
    zip names (map h g)

let make_sglets (defs: (list ((n: name {Cons? n}) * option (term * term))))
  : Tac (list sigelt)
  = let defs = concatOptions (L.map put_option_ext_tup defs) in
    let env = top_env () in
    let cur_module_name = cur_module () in
    let r = L.map (
      fun (name, (typ, def)) ->
        match lookup_typ env name with
        | Some f -> begin
          match inspect_sigelt f with
          | Sg_Let r fv us _ _ ->
            let name = make_patched_name_tot cur_module_name name in
            let fv = pack_fv name in
            Some (pack_sigelt (Sg_Let r fv us typ def))
          | _ -> None
          end
        | None -> None
    ) defs
    in concatOptions r

/// `generate_patched_defs` will rewrite `entrypoint` and every single functions depending on it
/// such that `patchFunction arg` is added everywhere it is possible in terms trees
/// A toplevel function `f: x -> y` will be rewritted to a function `f_erased: globalArgType -> x -> y` 
let generate_patched_defs (blacklist: list (bool * name)) (patchFunction globalArgType: term) (entrypoint: term) 
  : Tac (list sigelt)
  = make_sglets (patch_term_and_defs blacklist globalArgType patchFunction entrypoint) 

