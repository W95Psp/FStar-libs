module MetaTools.Util

open FStar.Tactics
module L = FStar.List.Tot

let replaceAt (l: list 'a) (pos: nat) (v: 'a): list 'a
  = let rec h l (i: nat) = match l with
    | [] -> []
    | hd::tl -> (if i = pos then v else hd)::h tl (i+1)
    in h l 0

let withIndexes (l: list 'a): list (nat * 'a)
  = let rec h l (i: nat) = match l with
    | [] -> []
    | hd::tl -> (i, hd)::h tl (i+1)
    in h l 0

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

