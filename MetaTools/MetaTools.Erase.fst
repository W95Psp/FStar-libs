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
  (eraseVarParamBinder: binder)
  (rebuildToplevel: term -> Tac term)
  (t: term)
  : Tac (term * (term -> Tac term))
  = let do_erase t: Tac term = mk_app (`erase)
      [ pack Tv_Unknown, Q_Explicit
      ; t, Q_Explicit
      ; eraseVarParam, Q_Explicit
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

let erase_term (eraseVarParam: term) (eraseVarParamBinder: binder) (t: term): Tac term = 
  fst
  (browse_term #unit #unitIsMonoid
    (fun beforeTransform rebuildToplevel boundedVariables parents typeLevel currentTerm -> 
      ( if (beforeTransform = false) || typeLevel
        then currentTerm, id_tac
        else (
          erase_term_helper eraseVarParam eraseVarParamBinder rebuildToplevel currentTerm
        )
      ), ()
    )
    false t)

let blacklist_lioCore (n: name) =
  match n with
  | ["MetaTools"; "Erase"; "label"]
  | ["MetaTools"; "Erase"; "labeled"]
  -> true
  | _ -> false

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

[@plugin]
let erase_term_and_defs (def: term)
 : Tac (list ((n: name {Cons? n}) * option (term * term)))
  =
    let label_var: bv = pack_bv ({
        bv_ppname = bv_erase;
        bv_sort = (`label);
        bv_index = 0;
    }) in
    // let label_var_term = pack (Tv_Var label_var) in 
    let g: graph = fv_dependencies_of
    (             blacklist_ulib
      `join_pred` blacklist_uu
      `join_pred` blacklist_lioCore) def
    in
    let h arg: Tac (option (term * term)) =
      let toplevel_name, dependencies = arg in
      let dependencies = toplevel_name::dependencies in
      let b_lvar: binder = pack_binder label_var Q_Explicit in
      let t_lvar: term = binder_to_term b_lvar in
      let replacements = make_erased_replacements t_lvar dependencies in
      // dump ("For top_level " ^ term_to_string (quote toplevel_name)
      //     ^ term_to_string (quote replacements));
      let patch (t: term): Tac term = replace_fv t replacements in
      match U.sglet_of_name toplevel_name with
      | Some (typ, def) ->
         Some (
          mk_tot_arr [b_lvar] (patch (erase_term (binder_to_term b_lvar) b_lvar typ))
          // typ//patch (erase_term t_lvar (mk_tot_arr [b_lvar] typ))
        , //patch ( erase_term t_lvar (pack (Tv_Abs b_lvar def)) )
          pack (Tv_Abs b_lvar (patch ( erase_term t_lvar b_lvar def )))
        )
      | None -> None
      // U.optmap (fun (typ, def)
      //   -> 
      //     let b_lvar = fresh_binder (`label) in
      //     mk_tot_arr [b_lvar] (patch (erase_term (binder_to_term b_lvar) typ))
      //   , Typeclasses.mk_abs [pack_binder label_var Q_Explicit] (patch (erase_term label_var_term def))
      // ) (U.sglet_of_name toplevel_name)
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

let generate_erased_defs (entrypoint: term)
  : Tac (list sigelt)
  = make_sglets (erase_term_and_defs entrypoint) 












(*

// let _ = fun x0 -> erase _ (a: Type -> Prims.Tot Type) x0

let valOf (x: labeled int): int = snd x 

let add (x y: labeled int) = 
  valOf x + valOf y

// let hey: (list (name * option (term * term)))
//   = _ by (
//     let x = erase_term_and_defs (`add) in
//     // let _ = typechecks (`(snd (1, 2))) in
//     exact (quote x)
//   )
let hey2: list sigelt = _ by (
    let t = generate_erased_defs (`add) in
    exact (quote t)
  )

let label_erased _ = label
let labeled_erased _ t = labeled t



%splice[] (
  // let Some t = admit (); L.nth hey2 2 in
  // let t = [t] in
  exact (quote hey2)
)

// let valOf_erased (x: labeled int): int = snd x 
%splice[] (
  let Some t1 = admit (); L.nth hey2 1 in
  let Some t2 = admit (); L.nth hey2 2 in
  let t = [t1;t2] in
  exact (quote t)
)


// let valOf_erased
//   : cur_label_name_GLOBAL: label -> Prims.Tot (x: labeled int -> Prims.Tot int)
//   = fun cur_label_name_GLOBAL x -> snd (erase _ x cur_label_name_GLOBAL)

// let _ = fun cur_label_name_GLOBAL x y ->
//             op_Addition (valOf_erased cur_label_name_GLOBAL (erase _ x cur_label_name_GLOBAL))
//               (valOf_erased cur_label_name_GLOBAL (erase _ y cur_label_name_GLOBAL))

// let valOf_erased (x: labeled int): int = snd x 



let xx: term = _ by (
  let Some t = admit (); L.nth hey2 1 in
  let Sg_Let _ _ _ typ def = admit (); inspect_sigelt t in
  let [_;x], def = collect_abs def in
  // let t = [t] in
  exact (quote (type_of_binder x))
)


%splice[valOf_erased; add_erased] (
  // let Some t0 = admit (); L.nth hey2 1 in
  let Some t1 = admit (); L.nth hey2 2 in
  let xx = [t1] in 
  exact (quote xx)
)
// %splice[] (
//   let Some t = admit (); L.nth hey2 2 in
//   let t = [t] in
//   exact (quote t)
// )

%splice[valOf_erased;add_erased] (
  let t = generate_erased_defs (`add) in
  exact (quote t)
)

let hey: (list (name * option (term * term)))
  = _ by (
    let x = erase_term_and_defs (`add) in
    // let _ = typechecks (`(snd (1, 2))) in
    exact (quote x)
  )

let hey: unit
  = _ by (
    let label_var: bv = pack_bv ({
        bv_ppname = bv_erase;
        bv_sort = (`label);
        bv_index = 0;
    }) in
    let b_lvar: binder = pack_binder label_var Q_Explicit in
    let t_lvar: term = binder_to_term b_lvar in
    let x = pack (Tv_Abs b_lvar (`(fun x y -> x + y))) in
    let x = pack (Typeclasses.mk_abs [b_lvar; fresh_binder_named "hey" (`int)] (t_lvar)) in
    // let x = pack (Typeclasses.mk_abs [fresh_binder_named "hey" (`int); b_lvar; fresh_binder_named "heasdsady" (`int);] (mk_app (`(+)) [
    //   (`3), Q_Explicit;
    //   t_lvar, Q_Explicit;
    // ])) in
    fail (term_to_string x);
    // let _ = typechecks (`(snd (1, 2))) in
    exact (`())
  )





// #set-options "--print_full_names --print_implicits"


let _ = fun x cur_label_name_GLOBAL y ->
  Prims.op_Addition (valOf x)
    (valOf (erase _ y cur_label_name_GLOBAL))

  


let _ = fun cur_label_name_GLOBAL x y ->
  Prims.op_Addition (valOf (erase _ x cur_label_name_GLOBAL))
    (valOf y)
    

let _ = fun x y glb ->
  Prims.op_Addition (valOf x)
    (valOf (erase _ y glb))

let xx x (y: labeled int)
  = erase _ (FStar.Pervasives.Native.snd y) x
*)
