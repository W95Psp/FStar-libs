module Control.Applicative

open Control.Functor
open FStar.Tactics.Typeclasses

class applyTC f = {
  apply: (#a: Type) -> (#b: Type) -> f (a -> b) -> f a -> f b;
  applyTC_has_functor: functor f;
}

let (<*>) #f [| functor: applyTC f |] = functor.apply

let lift2 #f #a #b #c [| applyTC: applyTC f |] (g: a -> b -> c) (u: f a) (v: f b)
  : f c
  = let (<$>) = applyTC.applyTC_has_functor.fmap in
    let (<*>) = applyTC.apply in
    g <$> u <*> v

class applicative f = {
  applicative_has_applyTC: applyTC f;
  pure: (#a: Type) -> a -> f a
}

