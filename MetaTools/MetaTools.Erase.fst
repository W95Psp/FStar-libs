module MetaTools.Erase

open MetaTools.BrowseTerm
open MetaTools.NamesOfTerm

module U = MetaTools.Util
module L = FStar.List.Tot

open FStar.Tactics
open Control.Monoid

[@plugin]
let typechecks (t: term): Tac bool
  = Some? (trytac (fun _ -> tc t))

type label = int
type labeled (a: Type) = label * a

assume val placeholder: (#a: Type) -> a

let erase a (v: labeled a) (at: label): labeled a
  = if at > fst v
    then placeholder
    else v

let erase_term_helper
  (eraseVarParam: term)
  (rebuildToplevel: term -> Tac term)
  (t: term)
  : Tac (term * (term -> Tac term))
  = let do_erase t: Tac term = mk_app (`erase)
      [ pack Tv_Unknown, Q_Explicit
      ; t, Q_Explicit
      ; eraseVarParam, Q_Explicit
      ] in
    let toplevel_erased = rebuildToplevel (do_erase t) in
    let log m =
      dump (term_to_string t ^ "\n" ^ term_to_string toplevel_erased ^ "\nRESPONSE:"^m)
    in
    if typechecks (rebuildToplevel t)//toplevel_erased
    then (log "YES"; t, do_erase)
    else (log "NOP"; t, id_tac)

let erase_term (eraseVarParam t: term): Tac term = 
  fst
  (browse_term #unit #unitIsMonoid
    (fun beforeTransform rebuildToplevel boundedVariables parents currentTerm -> 
      ( if (beforeTransform = false)
        then currentTerm, id_tac
        else (
          erase_term_helper eraseVarParam rebuildToplevel currentTerm
        )
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

let replace_fv (t: term) (r: list (name * term))
  : Tac term
  = fst (browse_term #unit #unitIsMonoid begin
    fun beforeTransform _ _ _ currentTerm
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
  end t)

let make_erased_name (n: name): Tac name =
  guard (Cons? n);
  cur_module () @ [U.last n ^ "_erased"]

let make_erased_replacements (lvar: bv) (n: list name): Tac (list (name * term))
  = map (fun n ->
    n, (
      let t = pack (Tv_FVar (pack_fv (make_erased_name n))) in
      mk_app t [pack (Tv_Var lvar), Q_Explicit]
    )
  ) n

module Typeclasses = FStar.Tactics.Typeclasses

let rec zip #a (l: list a) #b (r: list b)
  : list (a * b)
  = match l, r with
  | hl::tl, hr::tr -> (hl, hr)::zip tl tr
  | _ -> []

let bv_erase = "cur_label_name_GLOBAL"
let erase_term_and_defs (def: term)
 : Tac (list (name * option (term * term)))
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
      let replacements = make_erased_replacements label_var dependencies in
      let patch (t: term): Tac term = replace_fv t replacements in
      match U.sglet_of_name toplevel_name with
      | Some (typ, def) ->
         Some (
          let b_lvar: binder = fresh_binder (`label) in
          mk_tot_arr [b_lvar] (patch (erase_term (binder_to_term b_lvar) typ))
        , (
          Typeclasses.mk_abs
            [pack_binder label_var Q_Explicit]
            (patch (erase_term label_var_term def))
          )
        )
      | None -> None
      // U.optmap (fun (typ, def)
      //   -> 
      //     let b_lvar = fresh_binder (`label) in
      //     mk_tot_arr [b_lvar] (patch (erase_term (binder_to_term b_lvar) typ))
      //   , Typeclasses.mk_abs [pack_binder label_var Q_Explicit] (patch (erase_term label_var_term def))
      // ) (U.sglet_of_name toplevel_name)
    in
    zip (L.map fst g) (map h g)



let valOf (x: labeled int): int = snd x 

let add (x y: labeled int) = 
  valOf x + valOf y

let hey: (list (name * option (term * term)))
  = _ by (
    let x = erase_term_and_defs (`add) in
    // let _ = typechecks (`(snd (1, 2))) in
    exact (quote x)
  )


// let f n: normalize_term (if n then (int -> int) else (int -> int -> int))
// = let c1: int -> int = magic ()
  


let _ = fun cur_label_name_GLOBAL x y ->
  Prims.op_Addition (valOf (erase _ x cur_label_name_GLOBAL))
    (valOf y)
    

let _ = fun x y glb ->
  Prims.op_Addition (valOf x)
    (valOf (erase _ y glb))

let xx x (y: labeled int)
  = erase _ (FStar.Pervasives.Native.snd y) x
