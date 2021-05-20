module MetaTools.Util

open FStar.Tactics
module L = FStar.List.Tot



let rec replaceAt_helper #a (pos: nat) (v: a) (i: nat) (l: list a):
  Tot
  (list a)
  (decreases l)
  = match l with
    | [] -> []
    | hd::tl -> (if i = pos then v else hd)::replaceAt_helper pos v (i+1) tl
let replaceAt #a (l: list a) (pos: nat) (v: a): list a
  = replaceAt_helper pos v 0 l

let rec withIndexes_helper (l: list 'a) (i: nat): list (nat * 'a)
  = match l with
    | [] -> []
    | hd::tl -> (i, hd)::withIndexes_helper tl (i+1)
let withIndexes (l: list 'a): list (nat * 'a)
  = withIndexes_helper l 0

let rec bvs_of_pattern (p: pattern): list bv =
  match p with
  | Pat_Constant _ -> []
  | Pat_Cons   _ l ->
    let f (v: pattern * bool) = bvs_of_pattern (admit (); fst v) in
    let l = L.map f l in
    L.fold_left (@) [] l
  | Pat_Var     bv 
  | Pat_Wild    bv 
  | Pat_Dot_Term bv _ -> [bv]   


type term_view_head = 
  | Th_Var  | Th_BVar | Th_FVar | Th_App
  | Th_Abs  | Th_Arrow| Th_Type | Th_Refine
  | Th_Const| Th_Uvar | Th_Let  | Th_Match
  | Th_AscribedT | Th_AscribedC | Th_Unknown

let term_head t : Tac term_view_head =
  match inspect t with
  | Tv_Var   _ -> Th_Var
  | Tv_BVar  _ -> Th_BVar
  | Tv_FVar  _ -> Th_FVar
  | Tv_App _ _ -> Th_App
  | Tv_Abs _ _ -> Th_Abs
  | Tv_Const _ -> Th_Const
  | Tv_Type  _ -> Th_Type
  | Tv_Unknown -> Th_Unknown
  | Tv_Arrow  _ _ -> Th_Arrow
  | Tv_Refine _ _ -> Th_Refine
  | Tv_Uvar   _ _ -> Th_Uvar
  | Tv_Match  _ _ -> Th_Match
  | Tv_Let   _ _ _ _ _ -> Th_Let
  | Tv_AscribedT _ _ _ -> Th_AscribedT
  | Tv_AscribedC _ _ _ -> Th_AscribedC

let term_head_to_string x = 
  match x with
  | Th_Var       -> "Var"
  | Th_BVar      -> "BVar"
  | Th_FVar      -> "FVar"
  | Th_App       -> "App"
  | Th_Abs       -> "Abs"
  | Th_Arrow     -> "Arrow"
  | Th_Type      -> "Type"
  | Th_Refine    -> "Refine"
  | Th_Const     -> "Const"
  | Th_Uvar      -> "Uvar"
  | Th_Let       -> "Let"
  | Th_Match     -> "Match"
  | Th_AscribedT -> "AscribedT"
  | Th_AscribedC -> "AscribedC"
  | Th_Unknown   -> "Unknown"

let term_heads_to_string l
  = String.concat " " (L.map term_head_to_string l)

let sglet_of_name (name: name): Tac (option (typ * term)) 
  = match lookup_typ (top_env ()) name with
    | Some f -> begin
      match inspect_sigelt f with
      | Sg_Let r fv _ typ def -> Some (typ, def)
      | _ -> None
      end
    | None -> None

let rec exists_in_list  (eq: 'a -> 'a -> bool) (v: 'a) (l: list 'a) =
  match l with
  | [] -> false
  | hd::tl -> 
    if eq hd v then true else exists_in_list eq v tl

let rec rm_dup_in_list_helper #a (eq: a -> a -> bool) (l: list a) (seen: list a)
    : list a
    = match l with
    | [] -> []
    | hd::tl -> if exists_in_list eq hd seen
              then rm_dup_in_list_helper eq tl seen
              else hd::rm_dup_in_list_helper eq tl (hd::seen) 

let rm_dup_in_list (eq: 'a -> 'a -> bool) (l: list 'a) =
  rm_dup_in_list_helper eq l []

let rec last (l: list 'a {Cons? l}): 'a = 
  match l with
  | [hd] -> hd
  | hd::tl -> last tl


let rec names_of_pattern (p: pattern): list name =
  match p with
  | Pat_Cons   x l ->
    let f (v: pattern * bool) = names_of_pattern (admit (); fst v) in
    let l = L.map f l in
    L.fold_left (@) [inspect_fv x] l
  | _ -> []


open Control.Monoid
open FStar.Tactics.Typeclasses

unfold let bindM (#s: Type0) [| monoid s |] #a #b (x: a * s) (f: a -> Tac (b * s)): Tac (b * s) =
    let result, s0 = x in
    let result, s1 = f result in
    result, s0 <+> s1


// let f (): Tac (int * list int) =
//   let bind = bindM #(list int) in
//   x <-- "hey", [2];
//   x <-- "hey", [6];
//   x <-- (term_to_string (quote "hey")), [1];
//   x <-- "hey", [8];
//   9, []

// let xxx: int * (list int) = _ by (
//   let x = f () in
//   exact (quote x)
// )

unfold let optmap #a #b (f: a -> Tac b) (x: option a): Tac (option b)
  = match x with
  | Some v -> Some (f v)
  | None   -> None

unfold let optmapS (#s: Type0) [| monoid s |] #a #b (f: a -> Tac (b * s)) (x: option a): Tac (option b * s)
  = match optmap f x with
  | Some (v,s0) -> Some v, s0 
  | None        -> None  , mempty
  
unfold let focusFst ((a,b),c) = a,c


// type sg_Let' =
//        (r:bool)
//   * (  (fv:fv)
//   *  ( (us:list string)
//   *   ((typ:typ)
//   *    (def:term)
//     )))

// let dummy_range = range_of "DUMMY_RANGE"

// let sg_let'_to_Sg_LetMN
//   (sglet: sg_Let')
//   = let r, (fv, (us, (typ, def))) = sglet in
//     Sg_Let r fv (L.map (fun x -> dummy_range, x) us) typ def



let rec prefixOf (#a:eqtype) (prefix l: list a) =
  match prefix, l with
  | hdP::tlP, hdL::tlL -> if hdP = hdL then prefixOf tlP tlL else false
  | [], _ -> true
  | _ -> false
let prefixOfStr prefix str = prefixOf (String.list_of_string prefix) (String.list_of_string str)


let mkMagic typ: Tac term
  = mk_app (`magic) [typ, Q_Implicit; `(), Q_Explicit]  

let rec mk_lemma_arr (bs: list binder {Cons? bs}) (pre post pats: term) : Tot term (decreases bs) =
    match bs with
    | (b::bs) -> pack_ln (Tv_Arrow b (pack_comp (
      match bs with
      | []  -> C_Lemma pre post pats
      | _ -> C_Total (mk_lemma_arr bs pre post pats) None
    )))

let rec mapFilterTac (f: 'a -> Tac (option 'b)) (l: list 'a): Tac (list 'b)
  = match l with
    | [] -> []
    | hd::tl ->
      begin
        match f hd with
        | Some v -> v::mapFilterTac f tl
        |      _ ->   mapFilterTac f tl
      end

let rec mapFilter (f: 'a -> option 'b) (l: list 'a): list 'b
  = match l with
    | [] -> []
    | hd::tl ->
      begin
        match f hd with
        | Some v -> v::mapFilter f tl
        |      _ ->   mapFilter f tl
      end        
