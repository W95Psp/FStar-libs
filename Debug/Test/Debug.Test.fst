module Debug.Test

open Debug.Trace

let rec f (i: nat) =
  if i = 0 then 0 else 2 + f (i - 1)

let x y = trace ("AAAAxxx" ^ string_of_int (f y)) "ho"

let norm_me = x 3

let _ = assert_norm (norm_me == "ho")

