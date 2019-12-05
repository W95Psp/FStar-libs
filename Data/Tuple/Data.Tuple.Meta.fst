module Data.Tuple.Meta
open FStar.Tactics

let mkToplevel (name:string) (v: term) : Tac sigelt_view = let freeVariableName = pack_fv (cur_module () @ [name]) in
  Sg_Let false freeVariableName [] (`_) v

let rec mk_abs (bs : list binder) (body : term) : Tac term (decreases bs) = match bs with
  | [] -> body
  | b::bs -> pack (Tv_Abs b (mk_abs bs body))

unfold let tupleConsFV n = 
  pack_fv ["FStar";"Pervasives";"Native";"Mktuple" ^ string_of_int n]

let rec castList #a #p1 (#p2: _ {forall x. p1 x ==> p2 x}) (x: list (i: a {p1 i})): (x: list (i: a {p2 i}))
  = match x with
  | [] -> []
  | hd::tl -> hd::castList tl

let rec mkList (min: int) (max: int)
  : Tot (list (x: int {x >= min && x <= max}))
        (decreases (if min > max then 0 else 1 + max - min))
  = if min > max
    then [] else min::castList (mkList (min+1) max)

unfold let mkBV'     t name
  = pack_bv ({ bv_ppname = name; bv_sort = t; bv_index = 0})
unfold let mkBinder' t name
  = pack_binder (mkBV' t name) Q_Explicit
  
unfold let mkBV     name
  = pack_bv ({ bv_ppname = name; bv_sort = (`_); bv_index = 0})
unfold let mkBinder name
  = pack_binder (mkBV name) Q_Explicit
unfold let v tag n
  = mkBV (tag ^ string_of_int n)
unfold let h x el n
  = List.Tot.map (fun i -> Pat_Wild (v n i), true) el

let makeSelector (n: nat) (nth: nat {nth <= n})
  : Tac sigelt_view
  = let b = mkBinder "vTup" in
    let el = mkList 1 n in
    mkToplevel
      ("tup"^string_of_int n^"proj"^string_of_int nth)
      (pack (Tv_Abs b
        (pack (Tv_Match (binder_to_term b) [
                ( Pat_Cons (tupleConsFV n) (h false (castList el) "t" @ h true (castList el) "x")
                , pack (Tv_BVar (v "x" nth))
                )
              ])
        )
      ))

let makeUncurry (n: nat): Tac sigelt_view
  = let v tag n = mkBV (tag ^ string_of_int n) in
    let f, b = mkBinder "vFun", mkBinder "vTup" in
    let el = mkList 1 n in
    mkToplevel
      ("uncurry"^string_of_int n)
      (pack (Tv_Abs f (pack (Tv_Abs b (pack (
        Tv_Match (binder_to_term b) [
          ( Pat_Cons (tupleConsFV n) (h false (castList el) "t" @ h true (castList el) "x")
          , mk_e_app (binder_to_term f) (map (fun i -> pack (Tv_BVar (v "x" i))) (castList el))
          )]))))))

let makeCurry (n: nat): Tac sigelt_view
  = let v tag n = mkBV (tag ^ string_of_int n) in
    let f, b = mkBinder "vFun", mkBinder "vTup" in
    let el = mkList 1 n in
    mkToplevel
      ("curry"^string_of_int n)
      (mk_abs
        ( [f] @ List.Tot.map (fun i -> pack_binder (v "x" i) Q_Explicit) (castList el))
        (mk_e_app (binder_to_term f) (map (fun i -> pack (Tv_BVar (v "x" i))) (castList el)))
      )

%splice[] (
  let max = 14 in
  let l = 
  List.Tot.fold_left (@) [] (map (fun i -> 
      [
        makeUncurry i
      ; makeCurry   i
      ] @
      map (fun j -> makeSelector i j) (castList (mkList 1 i)) 
  ) (castList (mkList 2 max)))
  in let l = List.Tot.map (fun sev -> set_sigelt_quals [Unfold_for_unification_and_vcgen] (pack_sigelt sev)) l in
  l
)


