module Generic

open FStar.Tactics
module L = FStar.List.Tot
module L = FStar.List.Pure

open Data.Serialize.Helpers
open Data.Serialize.Types
open Data.Serialize.Rep

open FStar.Tactics.Typeclasses

// noeq
// type genericRepr (t: Type0): Type u#1 = 
//   | RSum  : string -> a:Type0 -> b:Type0 -> either (unit -> genericRepr a)   (unit -> genericRepr b) -> genericRepr t
//   | RProd : string -> a:Type0 -> b:Type0 ->        (unit -> genericRepr a) -> (unit -> genericRepr b) -> genericRepr t
//   | REmptySum : string -> genericRepr t
//   | REmptyProd : string -> genericRepr t
//   | RPrimitive : (p: Type0) -> p -> genericRepr t

noeq
type genericType (t: Type0): Type u#1 =
  | GTInductive : list (list Type0) -> genericType t
  // | GTFunction  : (arg: Type0) -> (ret: Type0) -> genericType t
  | GTPrimitive : (p: Type0) -> genericType t

noeq
type genericValue (t: Type0): Type u#1 =
  | GVInductive : nth: nat -> list (t: Type0 & gT: genericType t & (f: (genericValue t))) -> genericValue t
  // | GVFunction  : (arg: Type0) -> (ret: Type0)
  //               -> genericType arg
  //               -> (genericValue arg -> genericValue arg)
  //               -> genericValue t
  | GVPrimitive : (p: Type0) -> v: p -> genericValue t

let rec zip (l1: list 'a) (l2: list 'b {L.length l1 = L.length l2}): (r: list ('a * 'b) {L.length r = L.length l1}) = match l1, l2 with
  | h1::t1, h2::t2 -> (h1,h2)::zip t1 t2
  | _ -> []

let andL l = L.fold_right (fun x y -> x /\ y) l True

let for_all (f: 'a -> Type0) (l: list 'a) = andL (L.map f l)

let rec cast_list_lex ref (l: list 'a {l << ref})
  : (r: list (x:'a {x << ref}) {L.length l == L.length r})
  = match l with
  | hd::tl -> hd::cast_list_lex ref tl
  | [] -> []

let rec rel t (gT: genericType t) (gV: genericValue t)
  : Tot Type0 (decreases gV)
  = match gT, gV with
  | GTInductive l, GVInductive nth l' ->
    assert (l << gT);
    nth < L.length l /\
    ( let typs = L.index (cast_list_lex gT l) nth in
      let typs = cast_list_lex gT typs in
      let l' = cast_list_lex gV l' in
      L.length typs = L.length l' /\
      for_all (fun (inp: (x:_{x<<gT}) * (x:_{x<<gV})) ->
          let (t, (|t', gT', gV'|)) = inp in
          t == t' /\
          ( let gV': genericValue t' = gV' in
            rel t' gT' gV'
          )
      ) (zip typs l')
    )
  | GTPrimitive t, GVPrimitive t' _ -> t == t'
  | _ -> False

let genericValueD t gT = gV: genericValue t {rel t gT gV}

class generic (a: Type0): Type u#1 = {
  typ: genericType a;
  to: a -> genericValueD a typ;
  from: genericValueD a typ -> a;
}

let is_contaminant (f: 'a -> 'b -> 'c) = 
  forall prop. ~(
    forall x x' y y'.
      prop (f x  y)
    /\ prop (f x' y)
    /\ ~(prop (f x y')) 
  )

let is_contaminant' (f: 'a -> 'b -> 'c) = 
  ( forall p x v.
    p (f x v) ==> 
      (
          (exists x'. ~(p(f x' v)))
        \/ (forall r . p r)
      )
  )

// let _ = assert (is_contaminant (+))
let _ = assert (~(is_contaminant' (fun (a: int) (b: int) -> a)))

let transform_name_erase' (n: name): Tac name
  = nameCurMod' n (fun x -> x ^ "_eraser_chainable")
let transform_name_erase (n: name): Tac name
  = nameCurMod' n (fun x -> x ^ "_eraser")

let makeHasEraserInstance
  (s: inductiveSumup)
  (erase: fv)
  : Tac decls
  = let name = pack_fv (nameCurMod' s.iName (fun x -> x ^ "_hasErased")) in
    let {iVars; iName; iCons} = s in
    let types = map (fun _ -> pack_binder (fresh_bv (`Type)) Q_Explicit) (mkList 0 (s.iVars-1)) in
    let instances
      = map (fun t -> 
            pack_binder
              (fresh_bv 
                (call1 (`hasEraser)
                  (binder_to_term t)
                )
              )
              (Q_Meta (`tcresolve))
        ) types in
    let se: sigelt_view
      = Sg_Let false name [] (`_)
        (mk_abs (types @ instances) 
          (call1 (`MkhasEraser)
            (mk_e_app (pack (Tv_FVar erase)) (map (fun i -> 
              call1 (`__proj__MkhasEraser__item__erase_chainable)   (binder_to_term i)
            ) instances))
          )
        )
    in
    let se = pack_sigelt se in
    let se = set_sigelt_attrs [quote tcinstance] se in
    [se]


let rec generateDecodeEraser_term_for_argSumup
  (args_fun: list binder)
  (arg: argSumup (L.length args_fun))
  : Tac term // value -> value
  = match arg with
  | AS_Int | AS_String | AS_Bool -> (`id)
  | AS_List typ -> call1 (`L.map) (generateDecodeEraser_term_for_argSumup args_fun typ)
  | AS_TVar i -> binder_to_term (L.index args_fun i)
  | AS_Inductive tname args ->
    let f = name_to_term (transform_name_erase' tname) in
    let f = add_admit f in // TODO: this is a very dirty hack
    mk_e_app f (map (generateDecodeEraser_term_for_argSumup args_fun) args)

let generateEncodeEraser_term_for_inductiveSumup
    (s: inductiveSumup)
  : Tac term
  = let {iVars; iName; iCons} = s in
    let erasers: (x: list binder {L.length x == s.iVars})
      = admit (); map (fun _ -> pack_binder (fresh_bv (`_)) Q_Explicit) (mkList 0 (s.iVars-1)) in
    let inp = fresh_binder_named "inp'" (`_) in
    mk_abs (erasers @ [inp]) (
      mkMatchInductive s (binder_to_term inp) 
      ( map 
        (fun (i, (cName, cArgs))
         -> let f (bvs: list bv): Tac term
             = (
                 let fn = pack (Tv_FVar (pack_fv cName)) in
                 let cT = tc (top_env ()) fn in
                 let cArgs = zip (L.map (fun b -> snd (inspect_binder b)) (fst (collect_arr_ln_bs cT))) (withIndex cArgs) in
                 mk_app
                   fn
                   begin
                   map
                   ( fun (q, (j, arg))
                     -> let b = L.index bvs j in
                       call1
                       (generateDecodeEraser_term_for_argSumup erasers arg)
                       (bv_to_term (L.index bvs j))
                       , Q_Explicit
                   )
                   cArgs
                   // (withIndex cArgs)
                   end
               )
           in f
        )
        (withIndex iCons)
      )
    )


let generateEncodeEraser_for_inductiveSumup
    (s: inductiveSumup)
  : Tac decls
  = let {iVars; iName; iCons} = s in
    let erasers
      = map (fun _ -> pack_binder (fresh_bv (`_)) Q_Explicit) (mkList 0 (s.iVars-1)) in
    // fail (string_of_int (L.length erasers));
    let inp = fresh_binder_named "inp" (`_) in
    let body = generateEncodeEraser_term_for_inductiveSumup s in
    let mk (fName: _ -> Tac _) = Sg_Let true (pack_fv (fName s.iName)) [] (`_) in
    let sg
    = mk transform_name_erase (
         mk_abs (erasers @ [inp])
         (mk_e_app body (map binder_to_term erasers @ [binder_to_term inp]))
      ) in
    let sg' = mk transform_name_erase' body
    in [pack_sigelt sg'; pack_sigelt sg]

let generateEncodeEraser
    (name: fv)
  : Tac decls
  = let s = makeGenericRep name in
    generateEncodeEraser_for_inductiveSumup s


let generateEraser' tfv
  = let rep = makeGenericRep tfv in
      (generateEncodeEraser tfv)
    @ (makeHasEraserInstance
        rep 
        (pack_fv (transform_name_erase' (inspect_fv tfv)))
      )
let generateEraser (t: term)
  = generateEraser' (fvOf t)

let erase_value #a [| hasEraser a |] (v: a): a = erase_chainable v

instance int_hasErased: hasEraser int = {
  erase_chainable = id
}

type test (a b: Type) =
  | B : a -> b -> test a b
  | A : (int * int) -> (test a b * labeled a) -> test a b

%splice[tuple2_hasErased] (generateEraser (`tuple2))
%splice[test_hasErased] (generateEraser (`test))

let x = erase_value (A (3, 4) (B 32 2, LBL 3))
let _ = assert (x == A (3, 4) (B 32 2, magic ()))

