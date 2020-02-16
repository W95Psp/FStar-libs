module MetaTools.Env

open FStar.Tactics
open Control.Monoid
module L = FStar.List.Tot
module U = MetaTools.Util

let applyMagic (f t: term) q: Tac term
  = pack (Tv_App f (U.mkMagic t, q))


let is_EffectArr typ =
  let r = L.rev (String.list_of_string (term_to_string typ)) in
  match (fun x -> x) r with
  | 't'::'c'::'e'::'f'::'f'::'E'::' '::_ -> true
  | _ -> false
  

let is_type_lemmaComp (t: typ)
  = let c: comp = snd (collect_arr t) in
    C_Lemma? (inspect_comp c) 

[@plugin]
let is_name_not_inspectable (n: name)
  : Tac bool
  = match trytac begin fun () -> 
      match lookup_typ (top_env ()) n with
      | Some se ->
        let quals = sigelt_quals se in
        (match trytac (fun _ -> tc (top_env ()) (pack (Tv_FVar (pack_fv n)))) with
        | Some typ ->
          if is_EffectArr typ
          then false
          else
          let isLemma = is_type_lemmaComp typ in
          (false = isLemma) && 
            (L.existsb (fun q -> 
              match q with
              | Assumption | Irreducible | Action _ | HasMaskedEffect
              | Discriminator _ | RecordConstructor _ -> true
              | _ -> false
            ) quals)
        | _ -> false)
      | None   -> false
    end with
    | Some v -> v
    | None -> false

type placeholderMaker =
    (placeholderTerm: term)
  * (isArgumentExplicit: bool)

let mk_app_bds t bds =
  mk_app t (map (fun b -> binder_to_term b, snd (inspect_binder b)) bds)

[@plugin]
let make_absorbant_axioms
  (n: name)
  (mkPlaceholder: placeholderMaker)
  : Tac (list ((fv * term) * term))
  =
  let fv = pack_fv n in
  let t = pack (Tv_FVar fv) in
  match trytac (fun _ -> tc (cur_env ()) t) with
  | None -> []
  | Some typ ->
    let bds = fst (collect_arr_ln_bs typ) in
    if L.length bds = 0
    then []
    else
    begin
      let phs = U.mapFilterTac (fun (i, binder) -> 
        let mkPlaceholder t = 
          let ph, e = mkPlaceholder in
          mk_app ph [t, (if e then Q_Explicit else Q_Implicit)]
        in
        let ph = mkPlaceholder (type_of_binder binder) in
        let name = (inspect_bv (fst (inspect_binder binder))).bv_ppname in
        let lemmaBody = FStar.Tactics.Typeclasses.mk_abs bds (`(admit ())) in
        let getNthBinder n = (let Some x = admit (); L.nth bds n in x) in  
        let b_i = getNthBinder i in
        let leftHand = mk_app_bds t bds in
        let post = mk_e_app (`(==)) [leftHand;mkPlaceholder (`_)] in
        let lemmaTyp
          = U.mk_lemma_arr
bds
              (mk_e_app (`(==)) [binder_to_term b_i; mkPlaceholder (type_of_binder b_i)])
              (FStar.Tactics.Typeclasses.mk_abs [fresh_binder (`_)] post)
              (mk_e_app (`Cons) [mk_e_app (`smt_pat) [leftHand]; `Nil])
        in
        let lemmaName = cur_module () @ ["lemma_placeholder_" ^ U.last (admit (); n) ^ "_" ^ string_of_int i] in
        let lemmaFv = pack_fv lemmaName in
        if None? (trytac (fun () -> tc (top_env ()) lemmaTyp))
        then None
        else Some ((lemmaFv, lemmaTyp), lemmaBody)
      ) (U.withIndexes bds) in
      phs
    end


[@plugin]
let make_all_absorbant_axioms
  (mkPlaceholder: placeholderMaker)
  : Tac (list ((fv * term) * term))
  = let lst = L.map inspect_fv (all_defs_in_env (top_env ())) in
    let pred = fun x -> is_name_not_inspectable x, x in
    let lst = L.map snd (L.filter fst (map pred lst)) in
    dump "Okay, process now";
    mconcat (map (fun n -> 
      if fresh () % 20 = 0
      then
      // dump ("Try to make " ^ String.concat "." n);
      make_absorbant_axioms n mkPlaceholder
      else []
    ) lst)

let turn_triples_into_sigelts
  = L.map
    (fun ((fv, typ), body) ->
      pack_sigelt (Sg_Let false fv [] typ body)
    )

let slice_absorbant_axioms
  (n: name)
  (mkPlaceholder: placeholderMaker)
  : Tac (list sigelt)
  = turn_triples_into_sigelts (make_absorbant_axioms n mkPlaceholder)

let slice_absorbant_axioms_s
  (n: string)
  (mkPlaceholder: placeholderMaker)
  : Tac (list sigelt)
  = slice_absorbant_axioms (String.split ['.'] n) mkPlaceholder
  
let slice_all_absorbant_axioms
  (mkPlaceholder: placeholderMaker)
  : Tac (list sigelt)
  = turn_triples_into_sigelts (make_all_absorbant_axioms mkPlaceholder)

// assume val placeholder: (#a: Type) -> a

// let xx: decls
//   = _ by (
//   let t = slice_all_absorbant_axioms
//       ((`placeholder), false)
//   in 
//   exact (quote t)
// )
