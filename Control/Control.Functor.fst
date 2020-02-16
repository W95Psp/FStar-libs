module Control.Functor

open Data.Function
open FStar.Tactics.Typeclasses
module L = FStar.List.Tot

class functor (f: Type -> Type) = {
  fmap: (#a: Type) -> (#b: Type) -> (a -> b) -> f a -> f b;
}

unfold let mkFunctor (f: Type -> Type) fmap: functor f =
  { fmap = fmap
  }


unfold let (<$>) (#f: Type -> Type) [| functor: functor f |]
  = functor.fmap

unfold let (<$) (#f: Type -> Type) [| functor: functor f |] = functor.fmap @@ const 

unfold instance listFunctor = mkFunctor _ L.map
unfold instance optionFunctor = mkFunctor option (fun #a #b f o -> match o with
                                     | Some x -> Some (f x)
                                     | _ -> None
                                     )

unfold instance eitherFunctor #a
  = mkFunctor (either a)
    (fun #a #b f o -> match o with
    | Inr v -> Inr (f v)
    | Inl v -> Inl v
    )

