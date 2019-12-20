module Data.Generic.Helpers.Test

open Data.Generic.Helpers

open FStar.Tactics

let call1_test: int =
  _ by (exact (call1 (`(fun x -> x + 12)) (`30)))
let _ = assert (call1_test = 42)

let call2_test: int =
  _ by (exact (call2 (`(+)) (`30) (`12)))
let _ = assert (call2_test = 42)

let mkLet_tup'_test: int = 
  _ by (
    let mk, (a, b) = mkLet_tup' (`(12, 42)) in
    let t = mk (
      call2 (`(+)) (bv_to_term a) (bv_to_term b)
    ) in
    exact t
  )
let _ = assert (mkLet_tup'_test = 54)



let mkMatchInt_test: int =
  _ by (
    let term = (
      mkMatchInt (`2)
      [`111;`222;`333;`444;`555]
    ) in exact (term)
  )

let mkMatchInt_test2: int -> int =
  _ by (
    let n = fresh_binder (`int) in
    let term = mk_abs [n] (
      mkMatchInt n
      [`111;`222;`333;`444;`555]
    ) in exact (term)
  )



