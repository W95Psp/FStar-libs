/// This module collects free variable names contained in a term 
module MetaTools.NamesOfTerm

open FStar.Tactics
open Control.Monoid
open FStar.Tactics.Typeclasses
open MetaTools.Util
open MetaTools.BrowseTerm
module L = FStar.List.Tot

/// A pattern contains `Pat_Cons` expressions
/// they contains constructor names
/// `names_of_pattern` recursively go through these patterns and collect names
let rec names_of_pattern (p: pattern): list name =
  match p with
  | Pat_Cons   x l ->
    let f (v: pattern * bool) = names_of_pattern (admit (); fst v) in
    let l = L.map f l in
    L.fold_left (@) [inspect_fv x] l
  | _ -> []

/// `(@!)` is a list concatener that avoids duplicates
let rec (@!) (#a: eqtype) (l1 l2: list a)
  : Tot (list a) (decreases l2) = 
  match l2 with
  | [] -> l1
  | hd::tl -> if L.mem hd l1
            then l1 @! tl
            else (hd::l1) @! tl

/// `fvsOfTerm_helper` collects immediate names of a term (no recursion here)
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

/// Defines alternatives semigroup and monoid for list, relying on `(@!)` 
let lsetSemiGroup (#a: eqtype) : semigroup (list a)
  = mkSemigroup _ (@!)
let lsetIsMonoid (#a: eqtype) : monoid (list a) = 
  mkMonoid _ #lsetSemiGroup []

/// `fvsOfTerm` collects names of a term
let fvsOfTerm (t: term)
  : Tac (list name)
  = snd (browse_term #_ #lsetIsMonoid begin
    fun beforeTransform _ _ _ _ currentTerm
    -> (currentTerm, id_tac),
      ( if beforeTransform
        then fvsOfTerm_helper currentTerm
        else [] )
  end false t)

/// `fvsOfTerm (`(f 1 + 4))` will basically yeiled `[f, (+)]`
/// However, one might want to get the full dependency graph of all subterms of a term
/// This is what is done here
///
/// `graph` maps term names to lists of dependencies names  
unfold let graph = list (name * list name) 
let g_get (g: graph) k = L.find (fun (n, _) -> n = k) g
let g_mem (g: graph) k = Some? (g_get g k)
let g_rm (g: graph) k: graph =
  L.filter (fun (n, _) -> false = (n = k)) g
let g_set (g: graph) k v = (k, v)::g_rm g k
let g_values (g: graph): list name =
  mconcat #_ #lsetIsMonoid (L.map snd g)

unfold let negPred f x = false = f x 

/// `g_get_unexplored_names` collect the names that have not been looked up yey
let g_get_unexplored_names (g: graph): list name
  = let vals = g_values g in
    L.filter (negPred (g_mem g)) vals

/// `isGraphComplete` holds when every term dependencies of `g` have been looked up
let isGraphComplete (g: graph): bool
  = Nil? (g_get_unexplored_names g)

/// `g_resolve_once` looks up the unexplored names of `g` and update `g`
let g_resolve_once (filterNames: name -> bool) (g: graph): Tac (graph * continue: bool)
  = let l = g_get_unexplored_names g in
    fold_left #graph begin
      fun g n -> 
        let deps = match sglet_of_name n with
        | Some (typ, def) -> 
          fvsOfTerm typ @! fvsOfTerm def
        | None            -> [] in
        g_set g n (L.filter (negPred filterNames) deps)        
    end g l, Cons? l

/// `g_resolve` is `g_resolve_once`'s fixpoint
let rec g_resolve (filterNames: name -> bool) (g: graph): Tac graph
  = let g, continue = g_resolve_once filterNames g in
    if continue
    then g_resolve filterNames g
    else g

/// `g_sort_helper` compares dependencies
let g_sort_helper (a b: name * list name)
  = let (a, a_deps), (b, b_deps) = a, b in
    if L.mem a b_deps
    then -1
    else begin
      if L.mem b a_deps
      then 1
      else 0
    end
/// `g_sort` makes `g` such that if `T1` depends on `T2`, `T2` appears first in `g` 
let g_sort g = L.sortWith g_sort_helper g

/// `rootName` is a dummy name for the anonymous root term we analyse
let rootName = ["{root}"]

/// Given a fitlering function, `fv_dependencies_of` computes the dependency graph of a term
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







/// Rest of this file are parser and printers for `graph`s
let g_parse (g: string): graph
   = let h c = String.split [c] in 
    L.map (fun s -> match L.map (h '.') (h ' ' s) with
    | hd::tl -> hd, tl
    | _ -> ["???"], []
    ) (L.filter (fun x -> (x = "") = false) (h '\n' g))
    
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
let name_to_string = String.concat "."
let g_toString (g: graph): string =
  String.concat "\n\n" begin
  L.map (fun (n, l) ->
      "# " ^ name_to_string n
    ^ "\n"
    ^ String.concat "\n" (L.map (fun nn -> " - " ^ name_to_string nn) l)
  ) g end
