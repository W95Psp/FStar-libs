module MetaTools.Erase

open MetaTools.BrowseTerm
open MetaTools.NamesOfTerm

module U = MetaTools.Util
module L = FStar.List.Tot

open FStar.Tactics
open Control.Monoid
module Typeclasses = FStar.Tactics.Typeclasses

[@plugin]
let typechecks (t: term): Tac bool
  = Some? (trytac (fun _ -> tc (top_env ()) t))

// type label = int
// type labeled (a: Type) = label * a

// assume val placeholder: (#a: Type) -> a

// let erase a (v: labeled a) (at: label): labeled a
//   = if at > fst v
//     then placeholder
//     else v

let erase_term_helper
  (eraseFunction: term)
  (eraseVarParamBinder: binder)
  (rebuildToplevel: term -> Tac term)
  (t: term)
  : Tac (term * (term -> Tac term))
  = let do_erase t: Tac term = mk_app eraseFunction
      [ pack Tv_Unknown, Q_Explicit
      ; t, Q_Explicit
      ; binder_to_term eraseVarParamBinder, Q_Explicit
      ] in
    let toplevel_erased = 
      Typeclasses.mk_abs [eraseVarParamBinder] (rebuildToplevel (do_erase t))
    in
    let log m = ()
      // dump (term_to_string t ^ "\n" ^ term_to_string toplevel_erased ^ "\nRESPONSE:"^m)
    in
    if typechecks toplevel_erased
    then (log "YES"; t, do_erase)
    else (log "NOP"; t, id_tac)

let erase_term (eraseFunction: term) (eraseVarParamBinder: binder) (t: term): Tac term = 
  fst
  (browse_term #unit #unitIsMonoid
    (fun beforeTransform rebuildToplevel boundedVariables parents typeLevel currentTerm -> 
      ( if (beforeTransform = false) || typeLevel
        then currentTerm, id_tac
        else (
          erase_term_helper eraseFunction eraseVarParamBinder rebuildToplevel currentTerm
        )
      ), ()
    )
    false t)

// let blacklist_lioCore (n: name) =
//   match n with
//   | ["MetaTools"; "Erase"; "label"]
//   | ["MetaTools"; "Erase"; "labeled"]
//   -> true
//   | _ -> false

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

let make_erased_name_tot (cur_module_name: name) (n: name {Cons? n}): name =
  cur_module_name @ [U.last n ^ "_erased"]
  
let make_erased_name (n: name): Tac name =
  guard (Cons? n);
  make_erased_name_tot (cur_module ()) n

let make_erased_replacements (t_lvar: term) (n: list name): Tac (list (name * term))
  = map (fun n ->
    n, (
      let t = pack (Tv_FVar (pack_fv (make_erased_name n))) in
      mk_app t [t_lvar, Q_Explicit]
    )
  ) n

let rec zip #a (l: list a) #b (r: list b)
  : list (a * b)
  = match l, r with
  | hl::tl, hr::tr -> (hl, hr)::zip tl tr
  | _ -> []

let bv_erase = "cur_label_name_GLOBAL"

let isUppercase = function
  |'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'
  |'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'W'|'X'|'Y'
  |'Z' -> true
  | _  -> false

let startsUppercase s = match String.list_of_string s with
  | hd::_ -> isUppercase hd
  | _ -> false

let pred_blacklist (blacklist: list (bool * name)) (n: name)
  = Some? 
    (L.find (fun (exactMatch, pat) -> 
            if exactMatch
            then pat = n
            else U.prefixOf pat n 
     ) blacklist)

[@plugin]
let erase_term_and_defs (blacklist: list (bool * name)) (labelType: term) (eraseFunction: term) (def: term)
 : Tac (list ((n: name {Cons? n}) * option (term * term)))
  =
    let label_var: bv = pack_bv ({
        bv_ppname = bv_erase;
        bv_sort = labelType;
        bv_index = 0;
    }) in
    
    // let label_var_term = pack (Tv_Var label_var) in 
    let g: graph = fv_dependencies_of
    (             blacklist_ulib
      `join_pred` blacklist_uu
      // `join_pred` blacklist_lioCore
      `join_pred` (pred_blacklist blacklist)
      `join_pred` (fun (n: name) -> Cons? n 
                      && startsUppercase (U.last n)
                      )
      ) def
    in
    let h arg: Tac (option (term * term)) =
      let toplevel_name, dependencies = arg in
      let dependencies = toplevel_name::dependencies in
      let b_lvar: binder = pack_binder label_var Q_Explicit in
      let t_lvar: term = binder_to_term b_lvar in
      let replacements = make_erased_replacements t_lvar dependencies in
      let patch (t: term): Tac term = replace_fv t replacements in
      match U.sglet_of_name toplevel_name with
      | Some (typ, def) ->
         Some (
          mk_tot_arr [b_lvar] (patch ( erase_term eraseFunction b_lvar typ ))
        , pack (Tv_Abs b_lvar (patch ( erase_term eraseFunction b_lvar def )))
        )
      | None -> None
    in
    let names: list (n: name {Cons? n}) = admit (); L.map fst g in
    zip names (map h g)

let rec concatOptions (l: list (option 'a)): list 'a
  = match l with
  | [] -> []
  | Some hd::tl -> hd::concatOptions tl
  | _ ::tl -> concatOptions tl

let put_option_ext_tup (tup: 'a * option 'b): option ('a * 'b)
  = match tup with
  | (a, Some b) -> Some (a, b)
  | _ -> None

let make_sglets (defs: (list ((n: name {Cons? n}) * option (term * term))))
  : Tac (list sigelt)
  = let defs = concatOptions (L.map put_option_ext_tup defs) in
    // let err () = fail "make_sglets: lookup or inspect failed, impossible" in
    let env = top_env () in
    let cur_module_name = cur_module () in
    // let defs = map (fun (name, def) -> (name, , def)) 
    let r = L.map (
      fun (name, (typ, def)) ->
        match lookup_typ env name with
        | Some f -> begin
          match inspect_sigelt f with
          | Sg_Let r fv us _ _ ->
            let name = make_erased_name_tot cur_module_name name in
            let fv = pack_fv name in
            Some (pack_sigelt (Sg_Let r fv us typ def))
          | _ -> None
          end
        | None -> None
    ) defs
    in concatOptions r

let generate_erased_defs (blacklist: list (bool * name)) (eraseFunction labelType: term) (entrypoint: term) 
  : Tac (list sigelt)
  = make_sglets (erase_term_and_defs blacklist labelType eraseFunction entrypoint) 


