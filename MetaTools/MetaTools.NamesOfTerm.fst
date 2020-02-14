module MetaTools.NamesOfTerm

open FStar.Tactics
open Control.Monoid

open FStar.Tactics.Typeclasses
open MetaTools.Util
open MetaTools.BrowseTerm

module L = FStar.List.Tot


// browseFun s = bool -> (term -> Tac term) -> list bv -> list term_view_head -> term -> Tac (term * s)

let rec names_of_pattern (p: pattern): list name =
  match p with
  | Pat_Cons   x l ->
    let f (v: pattern * bool) = names_of_pattern (admit (); fst v) in
    let l = L.map f l in
    L.fold_left (@) [inspect_fv x] l
  | _ -> []

let rec (@!) (#a: eqtype) (l1 l2: list a)
  : Tot (list a) (decreases l2) = 
  match l2 with
  | [] -> l1
  | hd::tl -> if L.mem hd l1
            then l1 @! tl
            else (hd::l1) @! tl

let fvsOfTerm_helper (t: term_view)
  : Tac (list name)
  = match t with
  | Tv_FVar  fv-> [inspect_fv fv]
  | Tv_Match _ branches ->
      L.fold_left (@!) [] (
        L.map (fun (pat, _) -> names_of_pattern pat) branches
      )
  | _ -> []

open Control.Semigroup

let lsetSemiGroup (#a: eqtype) : semigroup (list a)
  = mkSemigroup _ (@!)

let lsetIsMonoid (#a: eqtype) : monoid (list a) = 
  mkMonoid _ #lsetSemiGroup []

let fvsOfTerm (t: term)
  : Tac (list name)
  = snd (browse_term #_ #lsetIsMonoid begin
    fun beforeTransform _ _ _ _ currentTerm
    -> (currentTerm, id_tac),
      ( if beforeTransform
        then fvsOfTerm_helper currentTerm
        else [] )
  end false t)

unfold let graph = list (name * list name) 

let list_to_string (l: list string)
  = "[" ^ String.concat "; " l ^ "]"
let name_to_string' (n: name)
  = list_to_string (L.map (fun s -> "\""^s^"\"") n)
let g_toString' (g: graph): string =
  list_to_string begin
  L.map (fun (n, l) ->
      name_to_string' n ^
      list_to_string (L.map name_to_string' l)
  ) g end

let g_get (g: graph) k = L.find (fun (n, _) -> n = k) g
let g_mem (g: graph) k = Some? (g_get g k)
let g_rm (g: graph) k: graph =
  L.filter (fun (n, _) -> false = (n = k)) g
let g_set (g: graph) k v = (k, v)::g_rm g k
let g_values (g: graph): list name =
  mconcat #_ #lsetIsMonoid (L.map snd g)

unfold let negPred f x = false = f x 

let name_to_string = String.concat "."

let g_toString (g: graph): string =
  String.concat "\n\n" begin
  L.map (fun (n, l) ->
      "# " ^ name_to_string n
    ^ "\n"
    ^ String.concat "\n" (L.map (fun nn -> " - " ^ name_to_string nn) l)
  ) g end

let g_get_unexplored_names (g: graph): list name
  = let vals = g_values g in
    L.filter (negPred (g_mem g)) vals

let isGraphComplete (g: graph): bool
  = Nil? (g_get_unexplored_names g)

let g_resolve_once (filterNames: name -> bool) (g: graph): Tac (graph * done: bool)
  = let l = g_get_unexplored_names g in
    fold_left #graph begin
      fun g n -> 
        let deps = match sglet_of_name n with
        | Some (typ, def) -> 
          fvsOfTerm typ @! fvsOfTerm def
        | None            -> [] in
        g_set g n (L.filter (negPred filterNames) deps)        
    end g l, Nil? l

let rec g_resolve (filterNames: name -> bool) (g: graph): Tac graph
  = let g, continue = g_resolve_once filterNames g in
    if continue
    then g_resolve filterNames g
    else g

let g_sort_helper (a b: name * list name)
  = let (a, a_deps), (b, b_deps) = a, b in
    if L.mem a b_deps
    then -1
    else begin
      if L.mem b a_deps
      then 1
      else 0
    end
let g_sort g = L.sortWith g_sort_helper g

let g_parse (g: string): graph
   = let h c = String.split [c] in 
    L.map (fun s -> match L.map (h '.') (h ' ' s) with
    | hd::tl -> hd, tl
    | _ -> ["???"], []
    ) (L.filter (fun x -> (x = "") = false) (h '\n' g))

// let e = g_parse "
// X.d X.e
// X.b X.c X.e
// X.c X.d X.e
// X.e
// X.a X.b X.c
// "

// let gg = g_sort e


let rootName = ["{root}"]

let fv_dependencies_of (filterNames: name -> bool) (t: term)
  : Tac graph
  = let deps = L.filter (negPred filterNames) (fvsOfTerm t) in 
    let g = g_set [] rootName deps in
    // something is wrong with g_resolve, I need to call it twice...
    let g = g_resolve filterNames g in
    let g = g_sort (g_resolve filterNames g) in
    g

[@plugin]
let fv_dependencies_of' (blacklist: list name) (t: term): Tac graph
  = fv_dependencies_of (fun n -> L.mem n blacklist) t

let join_pred (p: 'a -> bool) (q: 'a -> bool) (v: 'a): bool
  = p v || q v

let blacklist_uu (n: name) =
  match n with
  | [] -> false
  | _ ->  prefixOfStr "uu_" (last n)
 
let blacklist_ulib (n: name) =
  match n with
  | "FStar"::_
  | "List"::_
  | "LowStar"::_
  | "Prims"::_ -> true
  | _ -> false

