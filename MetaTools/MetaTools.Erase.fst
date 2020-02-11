module MetaTools.Erase

open MetaTools.BrowseTerm
open MetaTools.NamesOfTerm

module U = MetaTools.Util
module L = FStar.List.Tot

open FStar.Tactics

let typechecks (t: term): Tac bool
  = Some? (trytac (fun _ -> tc (top_env ()) t))


type label =
type labeled (a: Type) =

assume val placeholder: (#a: Type) -> a

let erase #a (v: labeled a) (at: label): labeled a
  = placeholder

let erase_term_helper
  (eraseVarParam: term)
  (rebuildToplevel: term -> Tac term)
  (t: term)
  = let erased_t = pack (Tv_App (`erase) (t, Q_Explicit)) in
    if typechecks erased_t
    then erased_t
    else t

let erase_term (eraseVarParam t: term): Tac term = 
  fst
  (browse_term #unit
    (fun beforeTransform rebuildToplevel boundedVariables parents currentTerm -> 
      ( if beforeTransform
        then erase_term_helper eraseVarParam rebuildToplevel currentTerm
        else currentTerm
      ), ()
    )
    t)


let blacklist_lioCore (n: name) =
  match n with
  | "FStar"::_
  | "List"::_
  | "LowStar"::_
  | "Prims"::_ -> true
  | _ -> false

let bv_erase = "cur_label_name_GLOBAL"

open FStar.Tactics.Typeclasses


let replace_fv (t: term) (r: list (name * name))
  : Tac term
  = fst (browse_term #unit begin
    fun beforeTransform _ _ _ currentTerm
    -> ( if beforeTransform
        then begin
             match inspect currentTerm with
             | Tv_FVar fv ->
               let n = inspect_fv fv in
               ( match L.find (fun (old, _) -> old = n) r with
               | Some (_, newName) -> pack (Tv_FVar (pack_fv newName))
               | _ -> currentTerm
               )
             | _ -> currentTerm
             end
        else currentTerm ), ()
  end t)

let make_erased_name (n: name): Tac name =
  cur_module () @ [last n ^ "_erased"]

let make_erased_names (n: list name): Tac (list (_ * _))
  = map (fun n -> n, make_erased_name n) n

let erase_term_and_defs (def: term)
  =
    let label_var: bv = pack_bv ({
        bv_ppname = bv_erase;
        bv_sort = `label;
        bv_index = 0;
    }) in
    let label_var_term = pack (Tv_Var label_var) in 
    let g: graph = fv_dependencies_of
    (             blacklist_ulib
      `join_pred` blacklist_uu
      `join_pred` blacklist_lioCore) def
    in
    let h arg: Tac (option (term * term)) =
      let toplevel_name, dependencies = arg in
      let replacements = make_erased_names dependencies in
      let patch t: Tac term = replace_fv t replacements in
      U.optmap (fun (typ, def)
        -> 
          let b_lvar = fresh_binder (`label) in
          mk_tot_arr [b_lvar] (patch (erase_term (binder_to_term b_lvar) typ))
        , mk_abs [pack_binder label_var Q_Explicit] (patch (erase_term label_var_term def))
      ) (U.sglet_of_name toplevel_name)
    in
    map h g
    
