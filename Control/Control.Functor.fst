module Control.Functor

open Data.Function
open FStar.Tactics.Typeclasses
module L = FStar.List.Tot

class functor (f: Type -> Type) = {
  fmap: (#a: Type) -> (#b: Type) -> (a -> b) -> f a -> f b;
}

let mkFunctor (f: Type -> Type) fmap: functor f =
  { fmap = fmap
  }


let (<$>) (#f: Type -> Type) [| functor: functor f |]
  = functor.fmap

let (<$) (#f: Type -> Type) [| functor: functor f |] = functor.fmap @@ const 

instance listFunctor = mkFunctor _ L.map
instance optionFunctor = mkFunctor option (fun #a #b f o -> match o with
                                     | Some x -> Some (f x)
                                     | _ -> None
                                     )

instance eitherFunctor #a
  = mkFunctor (either a)
    (fun #a #b f o -> match o with
    | Inr v -> Inr (f v)
    | Inl v -> Inl v
    )

