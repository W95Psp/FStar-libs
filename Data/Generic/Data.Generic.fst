module Data.Generic

open FStar.Tactics
module L = FStar.List.Tot

open Data.Generic.Helpers
open Data.Generic.Helpers.Serialized
open Data.Generic.Types
open Data.Generic.Rep
 
module D = Data.Generic.Decode

let generateDecodeGeneric = D.generateDecodeGeneric
 
// let writeValueAs (s: inductiveSumup): Tac term
//   = let {iVars; iName; iCons} = s in
//     let binders: list binder = L.map binderNth (mkList 0 (iVars-1)) in
//     let inp = binderName "input" in
//     let mk' t t' = pack (Tv_App t (t', Q_Explicit)) in
//     let mk t b = pack (Tv_App t (pack (Tv_Var b), Q_Explicit)) in
//     let rec h (a: argSumup iVars) = (match a with
//       | AS_String -> 
//         let b = fresh_bv (`string) in
//         Pat_Var b, [mk (`appendString) b]
//       | AS_Int -> 
//         let b = fresh_bv (`int) in
//         Pat_Var b, [mk (`appendInt) b]
//       | AS_Bool -> 
//         let b = fresh_bv (`bool) in
//         Pat_Var b, [mk (`appendBool) b]
//       | AS_List arg -> 
//         let b = fresh_bv (`list _) in
//         let len = mk (`FStar.List.length) b in
//         let inner = h arg in
//         Pat_Var b, [ mk' (`appendInt) len
//                    ; mk  (`appendBool) b]
//     ) in
//     let f (c: consSumup iVars) = Pat_Cons (pack_fv (fst c)) (map h (snd c)) in
//     Tv_Match inp 
//       (map f iCons)















// let rec mkArg n (a: argSumup n): Tac typ = 
//   match a with
//   | AS_Int -> (`int) | AS_String -> (`string) | AS_Bool -> (`bool)
//   | AS_List i -> pack (Tv_App (`list) (mkArg n i, Q_Explicit))
//   | AS_TVar n -> pack (Tv_Var (bvNth n)) 
//   | AS_Inductive f args -> mk_e_app (pack (Tv_FVar (mkName'S f))) (map (mkArg n) args)

// let makeGenericRepType (s: inductiveSumup): Tac decls =
//   let {iVars; iName; iCons} = s in
//   let binders: list binder = L.map binderNth (mkList 0 (iVars-1)) in
//   let mkCons (c: consSumup iVars) = makeTupleType (map (mkArg iVars) (snd c)) in
//   let inner = mk_abs binders (makeEitherType (map mkCons iCons)) in 
//   let sv = Sg_Let false (mkName'S iName) [] (`_) inner in
//   dump (term_to_string (quote sv));
//   [pack_sigelt sv]

// let mkMatchEither (n: nat) (l: list term) otherwise: term
//   = match n with
//   | 0 -> `()
//   | 1 -> `(if x > 0 then 1 else 2)
  
// let makeGenericRepEncoder (s: inductiveSumup): Tac decls =
//   let {iVars; iName; iCons} = s in
//   let encoders: list binder = L.map binderNth (mkList 0 (iVars-1)) in
//   let mkCons (c: consSumup iVars) = makeTupleType (map (mkArg iVars) (snd c)) in
//   let inner = mk_abs encoders () in 
//   let sv = Sg_Let false (mkName'S iName) [] (`_) inner in
//   dump (term_to_string (quote sv));
//   [pack_sigelt sv]
  
// %splice[myTest'S] (
//   let rep = makeGenericRep (fvOf (`myTest)) in
//   makeGenericRepType rep
//   )

// let xx: myTest'S int = (None, None)

[@plugin]
let f (x:term): (x: int {x == 35})
  = admit ()
  
// [@plugin]
// let f (): Tac int
//   = let x = A 3 in
//     match x with 

[@plugin]
let any_tac (#a: Type) (l: list a): Tac (list a) = l

