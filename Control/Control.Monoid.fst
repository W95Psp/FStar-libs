module Control.Monoid

open Control.Semigroup
open FStar.Tactics.Typeclasses

class monoid a = {
  mempty: a;
  semigroup_of_monoid: semigroup a;
}

unfold let mkMonoid a [| semigroup a |] (mempty: a): monoid a =
  { mempty = mempty
  ; semigroup_of_monoid = solve
  }

unfold let mappend #a [| monoid a |]: a -> a -> a
  = semigroup_of_monoid.sappend

unfold let (<+>) #t [| monoid t |] = mappend #t
unfold let mconcat #t [| monoid t |] = List.Tot.fold_left #t mappend mempty
unfold let mtimes #t [| monoid t |] (n: nat) (item: t)
  : t
  = if n = 0
    then mempty
    else stimes #_ #semigroup_of_monoid n item

unfold instance listIsMonoid #a : monoid (list a) = 
  mkMonoid _ #listSemigroup []
unfold instance unitIsMonoid : monoid unit = 
  mkMonoid _ #unitSemigroup ()

unfold instance optionIsMonoid #a [| semigroup a |] : monoid (option a) 
  = mkMonoid _ #optionSemigroup None
