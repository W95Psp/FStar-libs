module Data.Tuple

include Data.Tuple.Meta

let uncurry = uncurry2
let curry = curry2

let swap (x, y) = (y, x)

unfold let first f (a, b) = (f a, b) 
unfold let second f (a, b) = (a, f b) 

unfold let ( *** ) f g (a, b) = (f a, g b)
unfold let dup v = (v, v) 
unfold let both f (a, b) = (f a, f b) 

