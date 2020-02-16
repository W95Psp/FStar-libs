module MetaTools.Compiled

open FStar.Tactics.Typeclasses
open FStar.Tactics
open MetaTools.Util
open MetaTools.BrowseTerm
open MetaTools.PatchTerm
open MetaTools.Env
module L = FStar.List.Tot

[@plugin]
let patch_term_and_defs (blacklist: list (bool * name)) (globalArgType: term) (patchFunction: term) (def: term)
 : Tac (list ((n: name {Cons? n}) * option (term * term)))
  = patch_term_and_defs blacklist globalArgType patchFunction def

[@plugin]
let make_absorbant_axioms
  (n: name)
  mkPlaceholder
  : Tac (list ((fv * term) * term))
  = make_absorbant_axioms n mkPlaceholder

[@plugin]
let is_name_not_inspectable (n: name): Tac bool
  = is_name_not_inspectable n

[@plugin]
let make_all_absorbant_axioms
  mkPlaceholder
  : Tac (list ((fv * term) * term))
  = make_all_absorbant_axioms mkPlaceholder

[@plugin]
let selectOnOf (n: nat {n <> 0}) (l: list 'a): list 'a
  = mapFilter #(nat*_) (fun (i, v) -> 
      if i % n = 0
      then Some v
      else None
    ) (withIndexes l)
