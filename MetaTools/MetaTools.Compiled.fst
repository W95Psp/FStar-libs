module MetaTools.Compiled

open FStar.Tactics.Typeclasses
open FStar.Tactics
open MetaTools.Util
open MetaTools.BrowseTerm
open MetaTools.Erase


module L = FStar.List.Tot

// open Data.Serialize
// copy paste from pervasives
// noeq type fake_norm_step =
//   | Simpl | Weak | HNF | Primops | Delta
//   | Zeta | Iota | NBE | Reify
//   | UnfoldOnly  : list string -> fake_norm_step
//   | UnfoldFully : list string -> fake_norm_step
//   | UnfoldAttr  : list string -> fake_norm_step

// let unfake_norm_step: fake_norm_step -> norm_step
//   = function
//   | Simpl -> simplify | Primops -> primops
//   | Weak -> weak      | HNF -> hnf
//   | Delta -> delta    | Zeta -> zeta
//   | Iota -> iota      | NBE -> nbe
//   | Reify -> reify_
//   | UnfoldOnly  l -> delta_only l
//   | UnfoldFully l -> delta_fully l
//   | UnfoldAttr  l -> delta_attr l

// %splice[] (generateSerialize (`fake_norm_step))

// let normalize_inside_matches' (steps: list fake_norm_step) (t: term): Tac term
//   = let steps = L.map unfake_norm_step steps in
//     let norm = norm_term steps in
//     let f before getTree vars parents t
//         : Tac _
//         = match before, parents with
//          | false, Th_Match::_ -> 
//             let abs = norm (mk_abs (L.map (fun bv -> pack_binder bv Q_Explicit) vars) t) in
//             mk_e_app abs (map bv_to_term vars), ()
//          | _ -> t, ()
//     in fst (browse_term #unit f t)

// %splice[normalize_inside_matches] (mk_native_version (`normalize_inside_matches') "normalize_inside_matches" [true] false)


[@plugin]
let erase_term_and_defs (def: term)
 : Tac (list (name * option (term * term)))
 = erase_term_and_defs def



