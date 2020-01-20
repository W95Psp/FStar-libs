module Control.Semigroup

open FStar.Tactics.Typeclasses

class semigroup a = {
  sappend: a -> a -> a;
}

let mkSemigroup (a: Type) (f: a -> a -> a) = {sappend = f} 

let nonEmptyList t = l: list t {FStar.List.Tot.length l <> 0}

let sconcat #t [| semigroup t |] ((hd::tl): nonEmptyList t)
  = List.Tot.fold_left #t sappend hd tl

let rec stimes #t [| semigroup t |] (n: nat {n >= 1}) (item: t):
  Tot t (decreases n)
  = if n = 1
    then item
    else 
      let item: t = item `sappend` item in
      stimes #t (n - 1) item

instance listSemigroup #a : semigroup (list a)
  = mkSemigroup _ (@)
  
instance unitSemigroup : semigroup unit
  = mkSemigroup _ (fun _ _ -> ())

let intSemigroup_max : semigroup int
  = mkSemigroup _ (fun a b -> if a > b then a else b)
  
let intSemigroup_min : semigroup int
  = mkSemigroup _ (fun a b -> if a > b then b else a)

let intSemigroup_sum : semigroup int
  = mkSemigroup _ (+)
  
let intSemigroup_product : semigroup int
  = mkSemigroup _ FStar.Mul.op_Star

instance optionSemigroup #a [| semigroup a |] : semigroup (option a) 
  = mkSemigroup _ (fun x y -> match x, y with
                  | None, None -> None
                  | x, None | None, x -> x
                  | Some x, Some y -> Some (x `sappend` y)
                  )

let allSemigroup : semigroup bool
 = mkSemigroup _ (&&)

let anySemigroup : semigroup bool
 = mkSemigroup _ op_BarBar

