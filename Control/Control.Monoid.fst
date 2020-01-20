module Control.Monoid

open Control.Semigroup
open FStar.Tactics.Typeclasses

class monoid a = {
  mempty: a;
  semigroup_of_monoid: semigroup a;
}

let mkMonoid a [| semigroup a |] (mempty: a): monoid a =
  { mempty = mempty
  ; semigroup_of_monoid = solve
  }

let mappend #a [| monoid a |]: a -> a -> a
  = semigroup_of_monoid.sappend

let (<+>) #t [| monoid t |] = mappend #t
let mconcat #t [| monoid t |] = List.Tot.fold_left #t mappend mempty
let mtimes #t [| monoid t |] (n: nat) (item: t)
  : t
  = if n = 0
    then mempty
    else stimes #_ #semigroup_of_monoid n item

instance listIsMonoid #a : monoid (list a) = 
  mkMonoid _ []
instance unitIsMonoid : monoid unit = 
  mkMonoid _ ()

instance optionIsMonoid #a [| semigroup a |] : monoid (option a) 
  = mkMonoid _ #optionSemigroup None
