(**
This modules furnishes a simple representation for reflected inductives in F*.
I use this mainly to divide the complexity of Data.Generics.(En|De)code.

@summary Simple representation for reflected inductives
*)
module Data.Serialize.Rep

open Data.Serialize.Helpers
open Data.Serialize.Helpers.Serialized
open Data.Serialize.Types

open FStar.Tactics
module L = FStar.List.Tot

let rec makeGenericRep'Cons'Arg (iVars: nat) (bvs: list bv) t: Tac (argSumup iVars) =
  let f, args = collect_app t in
  let h = makeGenericRep'Cons'Arg iVars bvs in
  match f with
  | Tv_FVar x -> 
    (match inspect_fv x, args with
    | ["Prims";"list"], [arg, _] -> AS_List (h arg)
    | ["Prims";"int"], _ -> AS_Int
    | ["Prims";"bool"], _ -> AS_Bool
    | ["Prims";"string"], _ -> AS_String
    | n, _ -> AS_Inductive n (map (fun (x, _) -> h x) args)
    )
  | Tv_Refine bv ref -> makeGenericRep'Cons'Arg iVars bvs ((inspect_bv bv).bv_sort)
  | Tv_Var x | Tv_BVar x -> AS_TVar (if iVars = 0 then fail "found Tv_BVar while no type variable are bound" else (findIndex x bvs % iVars))
  | tv -> fail ("DUNNO HOW TO ACT WITH " ^ term_to_string f ^ ", that is " ^ term_to_string (quote tv))
  
let makeGenericRep'Cons (iVars: nat) (*params: list argv*) (name: name): Tac (consSumup iVars) =
  let f = lookup_typ' (top_env ()) name in
  match inspect_sigelt f with
  | Sg_Constructor _ typ
      -> let args, comp' = collect_arr_bs typ in
        guard (C_Total? (inspect_comp comp'));
        let tvars, args = L.splitAt iVars args in
        let tvars_bvs = L.map bv_of_binder tvars in
        // let _ = map type_of_binder  in
        name, map (fun x -> makeGenericRep'Cons'Arg iVars tvars_bvs (type_of_binder x)) args
  | _ -> fail (String.concat "." name ^ " is not a constructor")

let makeGenericRep (name: fv): Tac inductiveSumup =
  let f = lookup_typ' (top_env ()) (inspect_fv name) in
  match inspect_sigelt f with
  | Sg_Inductive name univs params typ' constructors
       -> let iVars = L.length params in
         let iCons = map (makeGenericRep'Cons iVars) constructors in
         {iCons = iCons; iName = name; iVars = iVars}
  | _ -> fail (fv_to_string name ^ " is not an inductive")

