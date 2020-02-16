module Test

// open MetaTools.BrowseTerm
// open MetaTools.NamesOfTerm
open MetaTools.PatchTerm
open MetaTools.Env
open MetaTools.Compiled
module L = FStar.List.Tot
module U = MetaTools.Util
open FStar.Tactics

let strings_to_names = L.map (String.split ['.'])

let label = int
let labeled t = label * t

let plus x y = x + y

assume val placeholder: (#a: Type) -> a

irreducible let f (x: int) = x

let xx: list sigelt = _ by (
  let x = slice_absorbant_axioms_s
    (`%(f))
    ((`placeholder), false)
  in exact (quote x)
)

%splice[lemma_placeholder_f_0] (
  xx
)

let _ = assert (f placeholder == placeholder)

let axiom_phAbsorbPlusL n: Lemma (placeholder + n = placeholder)
  [SMTPat (placeholder + n)] = admit ()
let axiom_phAbsorbPlusR n: Lemma (n + placeholder = placeholder)
  [SMTPat (n + placeholder)] = admit ()
let axiom_phAbsorbMinus (): Lemma (- placeholder = placeholder) = admit ()
let lemma_phAbsorbMinusL n: Lemma (n - placeholder = placeholder) [SMTPat (n - placeholder)] = 
  axiom_phAbsorbMinus (); axiom_phAbsorbPlusR n
let lemma_phAbsorbMinusR n: Lemma (placeholder - n = placeholder) [SMTPat (placeholder - n)] =  axiom_phAbsorbPlusL n
let axiom_phAbsorbLeL v: Lemma (placeholder < v = placeholder)
  [SMTPat (placeholder < v)] = admit ()
let axiom_phAbsorbLeR v: Lemma (v < placeholder = placeholder)
  [SMTPat (v < placeholder)] = admit ()
  
let erase a (v: labeled a) (at: label): labeled a
  = if at > fst v
    then placeholder
    else v

let valOf (x: labeled int): int = snd x 

let add (x y: labeled int) = 
  valOf x + valOf y

let add2 (x y: labeled int) = 
  add x y + valOf x + valOf y

let strings_to_pnames
  : list (bool * string) -> list (bool * name) 
  =
  L.map (fun (exactMatch, x) -> exactMatch, String.split ['.'] x)

%splice[add2_patched] (
    let t = generate_patched_defs
      (strings_to_pnames [
        true, `%label;
        true, `%labeled;
      ])
      (`erase)
      (`label)
      (`add2)
    in
    t
)

let x = 3, 100
let y = 4, 120

let h = add2_patched 0 x y

let _ = assert (
  h == (add2 x y)
)
