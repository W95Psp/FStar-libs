module Data.XX

open FStar.Tactics
module L = FStar.List.Tot

open Data.Serialize.Helpers
open Data.Serialize.Types
open Data.Serialize.Rep

open FStar.Tactics.Typeclasses

type sum a b =
  | SumLeft : a -> sum a b
  | SumRight : b -> sum a b

type product a b =
  | Product : a -> b -> product a b

type constructor (name: string) a =
  | Constructor : a -> constructor name a

type argument (name: string) a =
  | Argument : a -> argument name a

type noConstructors = | NoConstructors
type noArguments = | NoArguments

class generic a = {
  rep: Type;
  to: a -> rep;
  from: rep -> a;
}




let transform_name_erase' (n: name): Tac name
  = nameCurMod' n (fun x -> x ^ "_eraser_chainable")
let transform_name_erase (n: name): Tac name
  = nameCurMod' n (fun x -> x ^ "_eraser")

type labeled (x: Type) = | LBL : x -> labeled x

class hasEraser a = {
    erase_chainable: a -> a
}

let labeled_eraser_chainable _ _ = magic () 

[@tcinstance]
let labeledHasEraser (x: Type): hasEraser (labeled x) = {
   erase_chainable = (fun _ -> magic ()) 
}

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

