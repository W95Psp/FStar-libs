module Data.Serialize.Test

open Data.Serialize
open Data.Serialize.Helpers.Serialized
open Data.Serialize.Helpers
open Data.Serialize.Types

open FStar.Tactics
module L = FStar.List.Tot

type myTest a = | A : a -> myTest a
                | B : int -> myTest a
                | C : list a -> myTest a
                | D : a -> a -> myTest a

%splice[myTest_serialize_decode; myTest_serialize_decode_chainable]
  (generateDecodeSerialize (fvOf (`myTest)))
%splice[myTest_serialize_encode; myTest_serialize_encode_chainable]
  (generateEncodeSerialize (fvOf (`myTest)))

type myTest' a = | A' : a -> myTest' a
                 | B' : int -> myTest' a
                 | C' : list a -> myTest' a
                 | D' : myTest a -> myTest' a
                 | E' : myTest' a -> int -> string -> myTest' a



let decls_: decls = 
  _ by (
    let d = generateDecodeSerialize (fvOf (`myTest')) in
    exact (quote d)
  )

// let _ = assert (decls_ == magic ()) by (unfold_def (`decls_); fail "x")


%splice[myTest'_serialize_decode; myTest'_serialize_decode_chainable]
  (generateDecodeSerialize (fvOf (`myTest')))
%splice[myTest'_serialize_encode; myTest'_serialize_encode_chainable]
  (generateEncodeSerialize (fvOf (`myTest')))

let test: serialized = ( 
   []
, ([0;0]
, ([]
,  [true]
)))

let x: myTest (myTest bool)
  = myTest_serialize_decode (myTest_serialize_decode_chainable readBool) test
let y: serialized
  = myTest_serialize_encode #int appendInt (C [1;3;5;21;1])


let y': myTest int = myTest_serialize_decode readInt y

let encodeDecode #t #p
  t_serialize_encode t_serialize_decode
  (read: serialized -> p * serialized)
  (append: p -> serialized -> serialized)
  (native0: t p)
  : t p
  = let ser: serialized  = t_serialize_encode append native0 emptySerialized in
    let native1: t p * _ = t_serialize_decode read ser in
    fst native1

let isIdentity fn value = value == fn value

let _ = assert (
  normalize_term (
    L.fold_left (fun x y -> x /\ y) True
      (L.map (isIdentity (encodeDecode myTest_serialize_encode_chainable myTest_serialize_decode_chainable readInt appendInt))
        [ A 12; A 45
        ; B 3
        ; C []; C [1]; C [423;532]; C [12;123;6;3]
        ; D 2 3
        ]
      )
  )
)
// | E' : myTest' a -> int -> string -> myTest' a

let _ = assert (
  normalize_term (
    L.fold_left (fun x y -> x /\ y) True
      (L.map (isIdentity (encodeDecode myTest'_serialize_encode_chainable myTest'_serialize_decode_chainable readInt appendInt))
        [ A' 12; A' 45
        ; B' 3
        ; C' []; C' [1]; C' [423;532]; C' [12;123;6;3]
        ; D' (A 12)
        ; E' (A' 13) 4 "HEY"
        ; E' (E' (E' (D' (A 23)) 6 "X") 5 "B") 3 "A"
        ]
      )
  )
)

let _ = assert (
  normalize_term (
    L.fold_left (fun x y -> x /\ y) True
      (L.map (isIdentity (
        encodeDecode myTest'_serialize_encode_chainable myTest'_serialize_decode_chainable
             (myTest_serialize_decode_chainable readInt)
             (myTest_serialize_encode_chainable appendInt)
        ))
        [ A' (A 3); C' [A 2] ]
      )
  )
)




