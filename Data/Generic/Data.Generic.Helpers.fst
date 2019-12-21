module Data.Generic.Helpers

open FStar.Tactics
module L = FStar.List.Tot

let mkTupleType a b = a * b

unfold let mkTupleTypeTac (a b: typ): Tac typ
  = pack (Tv_App (pack (Tv_App (`mkTupleType) (a, Q_Explicit))) (b, Q_Explicit))

let makeOptionType t: Tac typ
  = pack (Tv_App (`option) (t, Q_Explicit))

let rec makeTupleType (types: list typ): Tac typ
  = match types with
  | [] -> `unit
  | [t] -> t
  | t::tl -> mkTupleTypeTac t (makeTupleType tl) 

let makeEitherType (types: list typ): Tac typ
  = makeTupleType (map makeOptionType types)

let lookup_typ' env (name: name): Tac sigelt =
  match lookup_typ env name with
  | Some x -> x
  | None   -> fail ("No \""^String.concat "." name^"\" found in the given enviroment")

let rec mk_abs (bs : list binder) (body : term) : Tac term (decreases bs) =
    match bs with
    | [] -> body
    | b::bs -> pack (Tv_Abs b (mk_abs bs body))

let rec last (l : list 'a) : Tac 'a =
  match l with
  | [] -> fail "last: empty list"
  | [x] -> x
  | _::xs -> last xs

let argvToBinder (x: argv) =
  let v, q = x in
  pack_binder
  (match v with
  | Tv_BVar b -> b
  | Tv_Var  b -> b
  | x         -> fail ("argvToBinder \"" ^ (term_to_string (quote x)) ^ "\": not a BV")
  ) q
let binderToArgv b =
  let bv, q = inspect_binder b in
  bv_to_term bv, q

let norm_term' t
  = //dump ("XXXXX -> " ^ term_to_string t);
    norm_term [zeta] t

let fvOf (t: term) = match inspect t with
  | Tv_FVar fv -> fv
  | _ -> fail "not a fv"

let nameCurMod' (n: name) (f: string -> string) =
  cur_module () @ [f (last n)]

let findIndex (x: bv) l: Tac nat =
  let rec h (n: nat) l: Tac nat
    = match l with
    | [] -> fail "findIndex failed"
    | hd::tl -> if compare_bv x hd = FStar.Order.Eq 
              then n
              else h (n+1) tl
  in h 0 l

let name_to_term (n: name): Tac term
  = pack (Tv_FVar (pack_fv n))
  
let bvName n = pack_bv ({bv_ppname = "v" ^ n; bv_index = 0; bv_sort = (`(_))})
let binderName n = pack_binder (bvName n) Q_Explicit
let bvNth n = pack_bv ({bv_ppname = "typvar" ^ string_of_int n; bv_index = 0; bv_sort = (`(_))})
let binderNth n = pack_binder (bvNth n) Q_Explicit


let rec mkList (min: int) (max: int)
  : Tot (list int)
        (decreases (if min > max then 0 else 1 + max - min))
  = if min > max
    then [] else min::(mkList (min+1) max)


unfold let call1 (f arg: term): Tac term
  = pack (Tv_App f (arg, Q_Explicit))
unfold let call2 (f arg1 arg2: term): Tac term
  = call1 (call1 f arg1) arg2
unfold let call3 (f arg1 arg2 arg3: term): Tac term
  = call1 (call2 f arg1 arg2) arg3

let mkLet_tup' (def: term)
  : Tac ((term -> Tac term) * (bv * bv))
  = let tmp = fresh_bv (`(_ * _)) in
    let fstbv = fresh_bv (`_) in
    let sndbv = fresh_bv (`_) in
    (fun (body: term) ->
      pack
      (Tv_Let false []
       tmp
       def
       ( pack ( Tv_Let false []
         fstbv
         (call1 (`fst) (bv_to_term tmp))
         (pack ( Tv_Let false []
           sndbv
           (call1 (`snd) (bv_to_term tmp))
           body
         ))
       ))
      )
    ), (fstbv, sndbv)


let mkLet_tup (def: term) (body: bv -> bv -> Tac term)
  : Tac term
  = let f, (a, b) = mkLet_tup' def in
    f (body a b)


unfold let admitMe n =
  admit (); n

unfold let admitTerm t = call1 (`admitMe) t


let rec withIndex_helper (l: list 'a) (n: int): (r: list (int * 'a)) =
    match l with
    | [] -> []
    | hd::tl -> (n, hd)::( withIndex_helper tl (n+1))

let withIndex (l: list 'a): (r: list (int * 'a))
  =  withIndex_helper l 0
    
let mkMatchInt (n: term) (bodies: list term)
  : Tac term
  = pack
    ( Tv_Match n 
      ( L.map
        (fun (i, body) -> Pat_Constant (C_Int i), body)
        (withIndex bodies)
        @ [Pat_Wild (fresh_bv (`int)), (`magic ())]
      )
    )
