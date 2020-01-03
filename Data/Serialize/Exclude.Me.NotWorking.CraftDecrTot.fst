module Exclude.Me.NotWorking.CraftDecrTot
open FStar.Tactics

// a few helpers
let fvOf (t: term) = match inspect t with
  | Tv_FVar fv -> fv | _ -> fail "not a fv"
unfold let call1 (f arg: term): Tac term = pack (Tv_App f (arg, Q_Explicit))
unfold let call2 (f arg1 arg2: term): Tac term = call1 (call1 f arg1) arg2
let rec mk_abs (bs : list binder) (body : term) : Tac term (decreases bs) =
    match bs with
    | [] -> body | b::bs -> pack (Tv_Abs b (mk_abs bs body))

// let's try to craft the following function
// that does not typecheck without the `decrases` clause
let rec f m (n: nat): Tot int (decreases n) =
  if n = 0 then 0
  else f (m + 1) (n - 1)

// we extract f's type
let typOfF (): Tac typ =
  let Some fdef = admit (); lookup_typ (top_env ()) (inspect_fv (fvOf (`f (*`*)))) in
  let Sg_Let _ _ _ typ _ = admit (); inspect_sigelt fdef in
  typ
  
// Note that the type of f is actually 
// not containing multiple decreases clauses
// as in https://github.com/FStarLang/FStar/issues/1901
// (since the following typechecks)
let _ =
  let t: option term
    = _ by (
      let Tv_Arrow _ comp = admit (); inspect (typOfF ()) in
      let C_Total _ typ = inspect_comp comp in
      exact (quote typ)
    )
  in assert (None? t)

// modifier version of mk_tot_arr
// so that one can specify a decreasing term
let decr_at_every_level = false
let rec mk_tot_arr_decr (bs: list binder) (cod : term) decr : Tac term =
    match bs with
    | [] -> cod
    | (b::bs) -> pack (Tv_Arrow b (pack_comp (C_Total (mk_tot_arr_decr bs cod decr) (
      if decr_at_every_level || FStar.List.Tot.length bs = 0
      then Some decr
      else None
    ))))

let craft_f' use_f_type: Tac decls =
  let name = pack_fv (cur_module () @ ["f'" ^ (
    if use_f_type then "_using_f_type" else ""
    )]) in
  // binders for `m` then `n`, just as `f` does
  let m = fresh_binder_named "m" (`(*`*)int) in
  let n = fresh_binder_named "n" (`(*`*)nat) in
  // make term versions of these binders
  let m',n'= binder_to_term m, binder_to_term n in
  // either use f original type or craft it
  let typ= if use_f_type
           then typOfF ()
           else mk_tot_arr_decr [m;n] (`(*`*)int) n'
  in  
  let se = Sg_Let true name [] typ (
    mk_abs [m;n] (
      pack (
        Tv_Match n'
        [ (Pat_Constant   (C_Int 0), (`(*`*)0))
        ; (Pat_Wild (fresh_bv (`(*`*)int)), 
                    call2 (pack (Tv_FVar name))
                      (call2 (`(*`*)(+)) m' (`(*`*)  1 ))
                      (call2 (`(*`*)(+)) n' (`(*`*)(-1)))
          )
        ]
      )
    ) 
  ) in
  [pack_sigelt se]

// crafting f' using f type works
%splice[] (craft_f' true)

// however, crafting f' with a crafted type
// result in F* failing because of termination issues 
%splice[] (craft_f' false)











// let decreasingList last_b: Tac typ = call1 (`List.length) (binder_to_term last_b)
// let decreasingSerialize last_b: Tac typ
//   = call1 (`serialize_size) (binder_to_term last_b)

// let decreasingFunctionLastArg n (decr: binder -> Tac typ): Tac typ =
//   let u = `int in
//   let binders = map (fun _ -> fresh_binder u) (mkList 1 n) in
//   let last_b  = fresh_binder u in
//   let pre = mk_tot_arr binders
//     (pack (Tv_Arrow last_b
//            (pack_comp (C_Total u (
//              (Some (decr last_b))
//            )))
//           )) in
//   pre

// let xxx: typ = _ by (let t = decreasingFunctionLastArg 0 decreasingSerialize in exact (quote t))
// let x_str = term_to_string xxx

// let typOfF (): Tac typ =
//   let Some fdef = admit (); lookup_typ (top_env ()) (inspect_fv (fvOf (`f))) in
//   let Sg_Let r fv us typ def = admit (); inspect_sigelt fdef in
//   // let Tv_Arrow _ comp = admit (); inspect typ in
//   // let C_Total typ _ = inspect_comp comp in 
//   // let Tv_Arrow _ comp = admit (); inspect typ in
//   // let C_Total _ (Some typ) = inspect_comp comp in 
//   typ
// let rec f: type_f = fun _ -> 23





// let _ = assert (true) by (
//   let Some fdef = admit (); lookup_typ (top_env ()) (inspect_fv (fvOf (`f))) in
//   let Sg_Let r fv us typ def = admit (); inspect_sigelt fdef in
//   let Tv_Arrow _ comp = admit (); inspect typ in
//   let C_Total typ _ = inspect_comp comp in 
//   // let Tv_Arrow _ comp = admit (); inspect typ in
//   // let C_Total _ typ = inspect_comp comp in
//   match typ with
//   | None -> dump "None"
//   | Some typ -> dump (term_to_string (typ))
// )
