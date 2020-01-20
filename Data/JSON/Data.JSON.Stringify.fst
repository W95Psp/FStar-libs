module Data.JSON.Stringify

open Data.JSON.Types

open FStar.String
open FStar.List.Tot

let joinstr: list string -> string = fold_left (^) ""

let escapeString (s: string): string =
  let s = FStar.String.split ['\n'] s in
  let s = FStar.String.concat "\\n" s in
  fold_left (fun l s -> l ^ s) ""
  (map (fun c -> 
    match c with
    | '\"' -> "\\\""
    | '\\' -> "\\\\"
    | c -> string_of_list [c]
  ) (list_of_string s))

let rec printDecimalNumber_h (digits: list char) n exp
  = match digits with
    | [] -> (if exp = 0 then "" else "e" ^ string_of_int exp)
    | hd::tl -> (if n = 0 then "." else "") ^ string_of_list [hd] ^ printDecimalNumber_h tl (n - 1) exp

let printDecimalNumber (DecimalNumber digits cp exp)
  = let l = list_of_string (string_of_int digits) in
    printDecimalNumber_h
      l
      (length l - cp)
      exp

// open FStar.String

let rec repeat (n: nat) s: string =
  if n = 0 then "" else s ^ (repeat (n - 1) s)

let stringify_string s = "\"" ^ escapeString s ^ "\""

let rec stringify_helper jump tab (n: nat) value: Tot _ (decreases %[value]) =
  let h jump n (v: jsonValue) = stringify_helper jump tab n (admitP (v << value); v) in
  let s = repeat n tab in 
  (if jump then "\n" ^ s else "") ^ 
  (match value with
  | JsonObject o -> 
    "{"
    ^ (match o with
       | [] -> ""
       | _ -> concat "," (map (
               fun (key, v) -> h true (n+1) (JsonString key)
                    ^ ": " ^ h false (n+1) v
             ) o) ^ "\n" ^ s
      ) ^ "}"
  | JsonArray l ->
    "[" ^ (
      match l with
      | [] -> ""
      | _  -> concat "," (map (h true (n + 1)) l) ^ "\n" ^ s
    ) ^ "]"
  | JsonString s -> stringify_string s
  | JsonNumber d -> printDecimalNumber d
  | JsonNull -> "null"
  | JsonBool b -> string_of_bool b
  )

let stringify (value: jsonValue) (spaces: string): string =
  stringify_helper false spaces 0 value

