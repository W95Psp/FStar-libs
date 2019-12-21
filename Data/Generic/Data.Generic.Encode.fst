module Data.Generic.Encode

open FStar.Tactics
module L = FStar.List.Tot
module LP = FStar.List.Pure

open Data.Generic.Helpers
open Data.Generic.Helpers.Serialized
open Data.Generic.Types
open Data.Generic.Rep


let transform_name_encode (n: name): Tac name
  = nameCurMod' n (fun x -> x ^ "_generic_encode")

let mkMatchInductive (s: inductiveSumup) (head: term) (bodies: list (list ((argSumup s.iVars) * bv) -> Tac term))
  : Tac term
  = pack
    ( Tv_Match head
      ( let bodies' = withIndex (zip s.iCons bodies) in
        map #(_ * (_ * (_ -> Tac term)))
        (fun (i, ((name, args), (body: _ -> Tac term))) ->
          let vars = map (fun x -> x, fresh_bv (`_)) args in
          let pat = Pat_Cons (pack_fv name)
            (
                // map (fun _ -> Pat_Var (fresh_bv (`_)), false) (mkList 1 s.iVars)  @
              L.map (fun x -> Pat_Wild (snd x), false) vars
            )
          , body vars
          in dump (term_to_string (quote pat)); pat
        )
        bodies'
      )
    )

type mkMatchInductive_test_typ =
  | MkMatchInductive_testA : int      -> mkMatchInductive_test_typ
  | MkMatchInductive_testB : string -> mkMatchInductive_test_typ

let mkMatchInductive_test: int = _ by (
  let s = makeGenericRep (fvOf (`mkMatchInductive_test_typ)) in
  let t = mkMatchInductive s (`(MkMatchInductive_testA 4)) [
    (fun l -> let [_, a] = admit (); l in 
      bv_to_term a
    );
    (fun _ -> `66)
  ] in
  exact (t)
)



let rec generateDecodeGeneric_term_for_argSumup
  (args_fun: list binder)
  (arg: argSumup (L.length args_fun))
  : Tac term // value -> serialized -> serialized
  = match arg with
  | AS_Int -> (`appendInt) 
  | AS_String -> (`appendString)
  | AS_Bool -> (`appendBool)
  | AS_List typ -> call1 (`appendList) (generateDecodeGeneric_term_for_argSumup args_fun typ)
  | AS_TVar i -> (binder_to_term (L.index args_fun i))
  | AS_Inductive tname args ->
    let f = name_to_term (transform_name_encode tname) in
    mk_e_app f (map (generateDecodeGeneric_term_for_argSumup args_fun) args)

let generateEncodeGeneric_term_for_inductiveSumup
    (s: inductiveSumup)
  : Tac term
  = let {iVars; iName; iCons} = s in
    let encoders: (x: list binder {L.length x == s.iVars})
      = admit (); map (fun _ -> pack_binder (fresh_bv (`_)) Q_Explicit) (mkList 0 (s.iVars-1)) in
    let inp = fresh_binder (`_) in
    mk_abs (encoders @ [inp]) (
      mkMatchInductive s (binder_to_term inp) 
      ( map 
        (fun (i, (cName, cArgs))
         -> let f (bvs: list ((argSumup s.iVars) * bv)): Tac term
             = call2 (`appendInt) (pack (Tv_Const (C_Int i))) (
                   fold_left
                   ( fun s (j, arg) 
                     -> call2 
                       (generateDecodeGeneric_term_for_argSumup encoders arg)
                       (bv_to_term (snd (L.index bvs j)))
                       s
                   )
                   (`emptySerialized)
                   (withIndex cArgs)
               )
           in f
        )
        (withIndex iCons)
      )
    )

let generateEncodeGeneric_for_inductiveSumup
    (s: inductiveSumup)
  : Tac decls
  = let sg
    = Sg_Let false (pack_fv (transform_name_encode s.iName)) [] (`_)
             (generateEncodeGeneric_term_for_inductiveSumup s)
    in dump (term_to_string (quote sg)); [pack_sigelt sg]

let generateEncodeGeneric
    (name: fv)
  : Tac decls
  = let s = makeGenericRep name in
    dump (term_to_string (quote s));
    generateEncodeGeneric_for_inductiveSumup s



type myTest a = | A : a -> myTest a
                | B : int -> myTest a

let _ = B #int 3

%splice[myTest_generic_encode] (generateEncodeGeneric (fvOf (`myTest)))

let test: serialized = ( 
   []
, ([0;0]
, ([]
,  [true]
)))

let x: serialized = myTest_generic_encode #int appendInt (B 45)

let _ = assert (fst x == magic ()) by (
  compute ();
  fail ""
)




