module MkDoc

open FStar.Tactics
module T = FStar.Tactics
open FStar.List.Tot
open FStar.String
open Data.JSON
open Data.JSON.Types

type doc =
  | Doc : string -> list (string * string) -> doc

irreducible let show_def_in_doc = ()

let compute_term = norm_term [primops; iota; delta; zeta]

let string_of_term t = match inspect t with
    | Tv_Const (C_String x) -> x
    | _                     -> "DOC:ERROR, not a string literal"

let x = Cons

let term_eq x y = compare_term x y = FStar.Order.Eq


let rec liststring_of_term (t: term): Tac (list (string * string))
  = 
  let explicitOnly = FStar.List.Tot.filter (fun (_, q) -> Q_Explicit? q) in 
  match collect_app t with
  | w, args -> 
    if   w `term_eq` (`Cons)
    then (
      match explicitOnly args with
      | [hd,_;tl,_] -> 
        ( match explicitOnly (snd (collect_app hd)) with
        | [k,_;v,_] -> (string_of_term k, string_of_term v)::liststring_of_term tl
        | args -> ["doc:liststring_of_term:error2", term_to_string hd ^ "//" ^ concat "|" (map (fun (x, _) -> term_to_string x) args)]
        )
      | _   -> ["doc:liststring_of_term:error1", concat "|" (map (fun (x, _) -> term_to_string x) args)]
    )
    else []
    
[@plugin]
let getTypes_asJSON (matchingAttr: option term): Tac jsonValue
  = let names 
        = match matchingAttr with
        | Some m -> lookup_attr m (top_env ())
        | None   -> all_defs_in_env (top_env ())
    in let names = map inspect_fv names in
    JsonObject (T.map (
      fun name -> 
      ( concat "." name
      , ( match trytac (fun _ -> tc (top_env ()) (pack (Tv_FVar (pack_fv name)))) with
          | Some typ -> 
            let attrs: list (either (string * list (string * string)) unit) = match lookup_typ (top_env ()) name with
              | Some sigelt ->
                T.filter_map
                  (fun t ->
                    match collect_app t with
                    | (t, [desc,_;args,_]) -> 
                      if compare_term t (`Doc) = FStar.Order.Eq
                      then (
                        Some (Inl (string_of_term (compute_term desc), liststring_of_term (compute_term args)))
                      ) else None
                    | (t, []) ->
                      if compare_term t (`show_def_in_doc) = FStar.Order.Eq
                      then Some (Inr ()) else None
                    | _ -> None
                  ) (sigelt_attrs sigelt)
              | None -> []
            in
            let doc = find (Inl?) attrs in
            let show_def = find (Inr?) attrs in
            JsonObject [
                 "type", JsonString (term_to_string typ);
                 "doc", ( match doc with
                        | Some (Inl (desc, args)) -> 
                            JsonObject [ "args", JsonArray (map (fun (l,r) -> JsonArray [JsonString l;JsonString r]) args)
                                       ; "desc", JsonString desc]
                        | None        -> JsonNull);
                 "def", ( match show_def with
                        | Some _ -> JsonString (term_to_string (
                          let n = String.concat "." name in
                          norm_term [delta_fully [n]] (pack (Tv_FVar (pack_fv name)))
                        ))
                        | None   -> JsonNull)
            ]
          | None   ->  JsonNull
        )
      )
    ) names)

[@plugin]
let export_types_to_file (matchingAttr: option term) filepath: Tac unit
  =  let o = getTypes_asJSON matchingAttr
  in let json = stringify o "" 
  in let _ = launch_process "sh" ["-c"; "cat - > " ^ filepath] json
  in ()

