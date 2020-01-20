module Example

open FStar.Tactics
open FStar.Tactics.JavaScript

type tree x =
  | Leaf: (leaf: x) -> tree x
  | Node: (nodes: list (tree x)) -> tree x

let rec fib (n: nat) =
  if n > 2
  then fib (n - 1) + fib (n - 2)  
  else 1

let rec fibs (n: nat)
  = if n = 0
    then []
    else fib n::fibs (n-1)

let rec funny x (n: nat)
  : Tot (tree int) (decreases n)
  =
  if n = 0
  then (Leaf (fib x))
  else (Node [funny (x + 1) (n-1);funny (x + 2) (n-1);funny (x + 3) (n-1)])

let console_log x = 
  let f = currifyMethod (obj_of_jsVal (jsGlobalThis () @. "console")) "log" 1 in
  f x

let writeToFile file content
  = let _ = launch_process "sh" ["-c"; "cat - > " ^ file] content in
    ()

let _ = assert (true) by (
  let ex = (`(
    console_log (json_stringify (funny 0 5));
    console_log (match (funny 0 5) with
    | Node l -> l
    | Leaf _ -> []);
    let wholaObject = emptyObject () in
    jsObjectSet wholaObject "x" "a";
    let n = (fun hey -> 
      let s = fromJsObject (jsObjectGet wholaObject "x") in
      // jsObjectSet this "test" ("helloooo:"^hey);
      jsObjectSet wholaObject "x" (s ^ "'");
      console_log ("helloooo:"^hey^s)
    ) in
    n "a"; n "b"; n "c" 
  )) in
  
  let c = defaultConfig in
  let c = collectJSOf ex [] c in
  // let c = jsDefOfStr (`%fib) c in
  // let c = jsDefOfStr (`%fibs) c in
  writeToFile "test.js" (print c ex)
)
