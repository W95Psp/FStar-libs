module Data.Generic.Test

open Data.Generic
open Data.Generic.Helpers.Serialized
open Data.Generic.Helpers
open Data.Generic.Types

open FStar.Tactics

type myTest a = | A : a -> myTest a
                | B : int -> myTest a

%splice[myTest_generic_decode] (generateDecodeGeneric (fvOf (`myTest)))

let test: serialized = ( 
   []
, ([0;0]
, ([]
,  [true]
)))

let x: myTest (myTest bool) * _ = myTest_generic_decode (myTest_generic_decode readBool) test

let _ = assert (fst x == magic ()) by (
  compute ();
  fail ""
)

