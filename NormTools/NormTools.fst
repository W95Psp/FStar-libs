module NormTools

open FStar.Tactics
module L = FStar.List.Tot
module P = FStar.Pervasives

let rec rmLast l = match l with
  | [] -> []
  | hd::[_] -> [hd]
  | hd::tl -> hd::rmLast tl
let rec last (l: _ {L.length l > 0}) = match l with
  | [hd] -> hd
  | hd::tl -> last tl

[@plugin]
let concatLists #a (l: list (list a)): list a = L.fold_left (@) [] l

[@plugin]
let projectorsOf (ind: fv): Tac (list name) =
  let Some f = admit (); lookup_typ (top_env ()) (inspect_fv ind) in
  match inspect_sigelt f with
  | Sg_Inductive _ univs params typ' (constructors)
       -> concatLists
         (map (fun constructor ->
           let Some f = admit (); lookup_typ (top_env ()) constructor in
           match inspect_sigelt f with
           | Sg_Constructor name typ ->
             let p_base = rmLast name in
             let p_name = last name in
             let p_name c = "__proj__" ^ p_name ^ "__item__" ^ c in
             L.map (
               fun b -> p_base @ [p_name (name_of_binder b)]
             ) (fst (collect_arr_bs typ))
           | _ -> fail "except Sg_Constructor"
         ) constructors)
  | _ -> fail (fv_to_string ind ^ " is not an inductive")

[@plugin]
let safeProjectorOf (ind: fv): Tac (list name) = 
  match trytac (fun _ -> projectorsOf ind) with
  | Some x -> x
  | None   -> []

[@plugin]
let resolve_projNames_of_attr' (delta_attr: list term)
  : Tac (delta_only: list string)
  = let env = top_env () in
    let concerned: list fv = concatLists (L.map (fun a -> lookup_attr a env) delta_attr) in
    let projectors: list name = concatLists (map safeProjectorOf concerned) in
    L.map (String.concat ".") projectors

unfold let string_to_name = String.split ['.']
unfold let string_to_fv x = pack_fv (string_to_name x)

[@plugin]
let resolve_projNames_of_attr (delta_attr: list string)
  = resolve_projNames_of_attr' (map (fun x -> pack (Tv_FVar (string_to_fv x))) delta_attr)

let norm_attrs (norm_steps: list norm_step) (delta_attr: list string) (delta_only: list string)
  : Tac ((#t: Type) -> t -> t)
  = normalize_term (
    let projectors = resolve_projNames_of_attr delta_attr in
    fun #t (v: t) -> P.norm
      ( norm_steps @
      [ P.delta_attr delta_attr
      ; P.delta_only ( delta_only @ projectors)
      ] ) #t v
    )

let norm_term_attrs (norm_steps: list norm_step) (delta_attr: list string) (delta_only: list string)
  : Tac (term -> Tac term)
  = normalize_term (
    let projectors = resolve_projNames_of_attr delta_attr in
    fun t -> norm_term
      ( norm_steps @
      [ P.delta_attr delta_attr
      ; P.delta_only ( delta_only @ projectors)
      ] ) t
    )

