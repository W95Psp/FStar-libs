module Data.Generic.Test

open Data.Generic
open Data.Generic.Helpers.Serialized
open Data.Generic.Helpers
open Data.Generic.Types

open FStar.Tactics
module L = FStar.List.Tot

type myTest a = | A : a -> myTest a
                | B : int -> myTest a
                | C : list a -> myTest a
%splice[myTest_generic_decode] (generateDecodeGeneric (fvOf (`myTest)))
%splice[myTest_generic_encode; myTest_generic_encode_chainable] (generateEncodeGeneric (fvOf (`myTest)))

type myTest' a = | A' : a -> myTest' a
                 | B' : int -> myTest' a
                 | C' : list a -> myTest' a
%splice[myTest'_generic_decode] (generateDecodeGeneric (fvOf (`myTest')))
%splice[myTest'_generic_encode; myTest'_generic_encode_chainable] (generateEncodeGeneric (fvOf (`myTest')))

let test: serialized = ( 
   []
, ([0;0]
, ([]
,  [true]
)))

let x: myTest (myTest bool)
  = myTest_generic_decode (myTest_generic_decode_chainable readBool) test
let y: serialized
  = myTest_generic_encode #int appendInt (C [1;3;5;21;1])


let y': myTest int = myTest_generic_decode readInt y

let encodeDecode #t #p
  t_generic_encode t_generic_decode
  (read: serialized -> p * serialized)
  (append: p -> serialized -> serialized)
  (native0: t p)
  : t p
  = let ser: serialized  = t_generic_encode append native0 emptySerialized in
    let native1: t p * _ = t_generic_decode read ser in
    fst native1

let isIdentity fn value = value == fn value

let _ = assert (
  normalize_term (
    L.fold_left (fun x y -> x /\ y) True
      (L.map (isIdentity (encodeDecode myTest_generic_encode_chainable myTest_generic_decode_chainable readInt appendInt))
        [ A 12; A 45
        ; B 3
        ; C []; C [1]; C [423;532]; C [12;123;6;3]]
      )
  )
)

let _ = assert (
  normalize_term (
    L.fold_left (fun x y -> x /\ y) True
      (L.map (isIdentity (encodeDecode myTest'_generic_encode_chainable myTest'_generic_decode_chainable readInt appendInt))
        [ A' 12; A' 45
        ; B' 3
        ; C' []; C' [1]; C' [423;532]; C' [12;123;6;3]]
      )
  )
)

let _ = assert (
  normalize_term (
    L.fold_left (fun x y -> x /\ y) True
      (L.map (isIdentity (
        encodeDecode myTest'_generic_encode_chainable myTest'_generic_decode_chainable
             (myTest_generic_decode_chainable readInt)
             (myTest_generic_encode_chainable appendInt)
        ))
        [ A' (A 3); C' [A 2] ]
      )
  )
)




