module Data.JSON.Test

open Data.JSON

let r = parse "
 [
   \"str\",
   null,
   2.3234234e20,
   {
     \"a\": 123,
     \"b\": 456,
     \"c\": [789, {\"a\": 3}, null]
   }
 ]
"

open FStar.Tactics
let writeToFile file content
  = let _ = launch_process "sh" ["-c"; "cat - > " ^ file] content in
    ()

let rr = match r with
  | Inl r -> stringify r "    "
  | Inr e -> e


let _ = assert (true) by (
  writeToFile "/tmp/result.json" rr
)


