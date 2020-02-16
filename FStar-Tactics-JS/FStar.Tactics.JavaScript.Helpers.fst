module FStar.Tactics.JavaScript.Helpers

open FStar.Tactics

module S = FStar.String
module L = FStar.List.Tot

module JSON = Data.JSON

let rec last (l: list 'a {Cons? l}): 'a = 
  match l with
  | [hd] -> hd
  | hd::tl -> last tl

let rec is_prefix (#a: eqtype) (prefix l: list a)
  = match prefix, l with
  | hdP::tlP, hdL::tlL -> hdP = hdL && is_prefix tlP tlL
  | [], _ -> true
  | _, [] -> false

let rec drop (l: list 'a) (n: nat): list 'a
  = match n, l with
  | 0, _ -> l
  | _, hd::tl -> drop tl (n-1)
  | _, [] -> []

let rec list_replace (#a: eqtype) (l: list a) (sl: (l: list a {L.length l > 0})) (sl': list a)
  : list a
  = if is_prefix l sl
    then drop l (L.length sl)
    else ( match l with
         | [] -> []
         | hd::tl -> hd::(list_replace tl sl sl')
         )

let str_replace s (ss: _ {String.length ss > 0}) ss': string
  = let s  = S.list_of_string s   in
    let ss = S.list_of_string ss  in
    let ss'= S.list_of_string ss' in
    S.string_of_list (list_replace s ss ss')

let list_replace_item (#a: eqtype) (l: list a) i i'
  = L.map (fun x -> if x = i then i' else x) l

let replaceChar (str: string) (ch ch': _): string =
  S.string_of_list (list_replace_item (S.list_of_string str) ch ch')

let makeJsName s =
  let s = str_replace s "_" "__" in // so that a odd number of _ is impossible unless `s` matches one of the following rules 
  let s = str_replace s "this" "this_" in
  let s = str_replace s "'" "_" in
  // let s = replaceChar s '\'' '_' in
  s


let isUppercase s = String.uppercase s = s
let isCharUppercase c = isUppercase (S.string_of_list [c])
let isFirstCharUppercase s =
  match String.list_of_string s with
  | [] -> false
  | c::_ -> isCharUppercase c

let isNameConstructor n = match n with
    | [] -> false
    | _  -> isFirstCharUppercase (last n)

let printConst = function
    | C_Unit      -> "UnitType"
    | C_Int     n -> string_of_int n   
    | C_True      -> "true"
    | C_False     -> "false"
    | C_String  s -> JSON.stringify_string s //"\"" ^ s ^ "\""
    | _   -> "'unknown Tv_Const!'"

let bv_to_js (bv: bv): Tac string =
  let {bv_ppname} = inspect_bv bv in
  bv_ppname

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


let rec names_of_pattern (p: pattern): list name =
  match p with
  | Pat_Cons   x l ->
    let f (v: pattern * bool) = names_of_pattern (admit (); fst v) in
    let l = L.map f l in
    L.fold_left (@) [inspect_fv x] l
  | _ -> []


let rec mkList (n: nat): list int 
  = if n = 0
    then []
    else n::mkList (n-1)

let getConstructorBinderNames n: Tac (option (list string))
  = match lookup_typ (top_env ()) n with
    | Some cons -> 
      begin
        match inspect_sigelt cons with
        | Sg_Constructor _ typ -> 
          let arrs, _ = collect_arr_bs typ in
          let arrs = L.filter (fun x -> Q_Explicit? (snd (inspect_binder x))) arrs in
          Some (L.map name_of_binder arrs)
        | _ -> None
      end
    | None -> None

let getArityOfConstructor name: Tac (option int)
  = match lookup_typ (top_env ()) name with
    | Some cons -> 
      begin
        match inspect_sigelt cons with
        | Sg_Constructor _ typ -> 
          let arrs, _ = collect_arr_bs typ in
          let arrs = L.filter (fun x -> Q_Explicit? (snd (inspect_binder x))) arrs in
          Some (L.length arrs)
        | _ -> None
      end
    | None -> None


let mapJoin' s (f: 'a -> string) l: _ = S.concat s (L.map f l)
let mapJoin s (f: 'a -> Tac string) l: Tac _ = S.concat s (map f l)

let dotTo_ (s: string): string =
  S.concat "_" (S.split ['.'] s)



let rec (@!) (#a: eqtype) (l1 l2: list a)
  : Tot (list a) (decreases l2) = 
  match l2 with
  | [] -> l1
  | hd::tl -> if L.mem hd l1
            then l1 @! tl
            else (hd::l1) @! tl

let rec names_of_term (t: term) : Tac (list name) =
  match inspect t with
  | Tv_FVar f -> [inspect_fv f]
  | Tv_Match expr branches ->
      L.fold_left (@!) (names_of_term expr) (
        map
          (fun (pat, body) -> names_of_pattern pat @! names_of_term body)
          branches
        )
  | Tv_App a (b, _)
  | Tv_Let   _ _ _ a b -> 
      names_of_term a @! names_of_term b
  | Tv_Abs _ x
  | Tv_AscribedT x _ _
  | Tv_AscribedC x _ _ -> names_of_term x
  | _ -> []

let sglet_of_name (name: name): Tac (list _) 
  = 
  let Some f = admit (); lookup_typ (top_env ()) name in
  match inspect_sigelt f with
  | Sg_Let r fv _ _ def -> [def]
  | _ -> []
  
let names_of_name (n: name): Tac (list name) 
  = 
  match lookup_typ (top_env ()) n with
  | Some f -> 
      begin 
        match inspect_sigelt f with
        | Sg_Let r fv _ _ def -> 
                 // let dd = quote def in
                 // fail (term_to_string dd);
                 names_of_term def
        | _ -> []
      end
  | None -> []

let rec name_closure (names_todo: list name) (exclude: list name)
  : Tac (list name)
  = 
  let excluded someName exclude: bool = L.mem someName exclude in 
  match names_todo with
  | [] -> []
  | n::todo -> 
      if excluded n exclude
      then name_closure todo exclude
      else begin
        let newTodo = names_of_name n in
        n :: name_closure (todo @! newTodo) (n::exclude)
      end

// hand a name `n`, that contains `l`
// let rec names_of_name_star' (n: name) (seen: list name)
//   : Tac (list name)
//   = let names = n::(names_of_name n) in
//     let names = L.filter (fun nn -> L.mem nn seen = false) names in
//     fold_left ()
    



let rec rename_on_dup' (#a:eqtype) (rename: a -> a) (l seen: list a): list a =
  match l with
  | [] -> []
  | hd::tl -> if L.mem hd seen
            then rename_on_dup' rename (admit(); (rename hd)::tl) seen
            else hd::(rename_on_dup' rename tl (hd::seen))
  
let rename_on_dup (#a:eqtype) (rename: a -> a) (l: list a) = rename_on_dup' rename l []  

let rec init (l: list 'a {Cons? l}): list 'a =
  match l with
  | hd::[] -> [hd]
  | hd::tl -> hd::init tl

let rename_on_dup_names (l: list name) = rename_on_dup (fun name -> match name with
    | [] -> []
    | _ -> init name @ [last name ^ "_"]
    ) l

let rename_on_dup_names_str (l: list string) = L.map (S.concat ".") (rename_on_dup_names (L.map (S.split ['.']) l))

let rec remove_dup' (#a:eqtype) (l seen: list a): list a =
  match l with
  | [] -> []
  | hd::tl -> if L.mem hd seen
            then remove_dup' tl seen
            else hd::(remove_dup' tl (hd::seen))
  
let remove_dup (#a:eqtype) (l: list a) = remove_dup' l []  

//     if L.mem n seen
//     then []
//     else
//     begin
//       dump ("names_of_name_star' n=" ^ String.concat "." n ^ " seen=" ^ mapJoin' ";" (String.concat ".") seen);
//       let seen = n::seen in
//       let names = n::(names_of_name n) in
//       let names = L.filter (fun nn -> L.mem nn seen = false) names in
//       let seen: list name = seen @! names in 
//       let l = map (fun n -> names_of_name_star' n seen) names in
//       L.fold_left (@!) names l 
//     end
// let names_of_name_star (n: name): Tac (list _) 
//   = names_of_name_star' n []
