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


let mk (k: string) (args: list (string * jsonValue))
  = JsonObject (("kind", JsonString k)::args)

let vconst_to_json (cst: vconst): Tac jsonValue
  = match cst with
  | C_Unit      -> mk "unit" []
  | C_Int     i -> mk "int" ["value", JsonNumber (DecimalNumber i 0 0)]
  | C_True      -> mk "bool" ["value", JsonBool true]
  | C_False     -> mk "bool" ["value", JsonBool false]
  | C_String  s -> mk "string" ["value", JsonString s]
  | C_Range   r -> mk "range" ["value", JsonString "not supported"]
  | C_Reify     -> mk "reify" []
  | C_Reflect n -> mk "reflect" ["name", JsonArray (map JsonString n)]

let fv_to_json fv = JsonArray (map JsonString (inspect_fv fv))

let maybe_to_json #a (f: a -> Tac jsonValue) (v: option a): Tac jsonValue
 = match v with
 | Some v -> f v
 | None   -> JsonNull

let rec term_to_json' (deep: nat) (t: term): Tac jsonValue
  = if deep > 6 then mk "error" ["detail", JsonString "too deep"]
    else
  ( match inspect t with
  | Tv_Var bv     -> bv_to_json deep "var" bv
  | Tv_BVar bv    -> bv_to_json deep "bvar" bv
  | Tv_FVar fv    -> mk "fvar" ["value", fv_to_json fv]
  | Tv_App f x    -> 
    let f, args = collect_app t in
    mk "app" [ "function", term_to_json' (deep + 1) f
             ; "args", JsonArray (
               T.map (
                 fun (arg, qual) -> 
                   JsonObject 
                     [ "qualifier", aqual_to_json deep qual
                     ; "term", term_to_json' (deep + 1) arg
                     ]
               ) args)
             ]
  | Tv_Abs _ _    -> 
    let bds, body = collect_abs t in
    mk "abs" [ "body", term_to_json' (deep + 1) t
             ; "args", JsonArray (
               T.map (
                 fun (arg, qual) -> 
                   JsonObject 
                     [ "qualifier", aqual_to_json deep qual
                     ; "bv", bv_to_json deep "binder" arg 
                     ]
               ) (map inspect_binder bds))
             ]
  | Tv_Arrow _ _  -> 
    let typs, c = collect_arr t in
    mk "arrow" [ "comp", comp_to_json c
               ; "args", JsonArray (T.map (term_to_json' (deep + 1)) typs)
               ]
  | Tv_Type ()    -> mk "Type" []
  | Tv_Refine x t -> mk "refine"
    [ "var", bv_to_json deep "bv" x
    ; "refinement", term_to_json' (deep + 1) t
    ]
  | Tv_Const cst  -> vconst_to_json cst
  | Tv_Uvar i t   -> mk "uvar" ["index", JsonNumber (DecimalNumber i 0 0)]
  | Tv_Let r attrs b t1 t2 -> 
      mk "let" 
      [ "recursive", JsonBool r
      ; "attributes", JsonArray (T.map (term_to_json' (deep + 1)) attrs)
      ; "name", bv_to_json deep "bv" b
      ; "definition", term_to_json' (deep + 1) t1
      ; "type", term_to_json' (deep + 1) t2
      ]
  | Tv_Match t branches    -> 
      mk "match"
      [ "term", term_to_json' (deep + 1) t
      ; "branches", JsonArray (T.map
        (fun (patt, t) -> 
          JsonObject
            [ "pattern", pattern_to_json deep patt
            ; "term", term_to_json' (deep + 1) t
            ]
        )
        branches)
      ]
  | Tv_AscribedT e t tt -> mk "ascribed_term"
                 [ "e", term_to_json' (deep + 1) e
                 ; "t", (term_to_json' (deep + 1) t)
                 ; "tt", maybe_to_json (term_to_json' (deep + 1)) tt]
  | Tv_AscribedC e t tt -> mk "ascribed_comp"
                 [ "e", (term_to_json' (deep + 1)) e
                 ; "t", comp_to_json t
                 ; "tt", maybe_to_json (term_to_json' (deep + 1)) tt]
  | Tv_Unknown -> mk "unknown" []
  )
and bv_to_json deep (n: string) (b: bv): Tac jsonValue =
  let b = inspect_bv b in
  mk n
    ["name", JsonString b.bv_ppname
    ;"type", (term_to_json' (deep + 1)) b.bv_sort]
and aqual_to_json deep (q: aqualv): Tac jsonValue =
  match q with
  | Q_Explicit -> JsonObject ["kind", JsonString "explicit"]
  | Q_Implicit -> JsonObject ["kind", JsonString "implicit"]
  | Q_Meta t   -> JsonObject ["kind", JsonString "meta"; "resolver", term_to_json' (deep + 1) t]
and comp_to_json (c: comp): Tac jsonValue =
  match inspect_comp c with
  | C_Total ret decr -> JsonObject ["kind", JsonString "total"]
  | C_Lemma pre post -> JsonObject ["kind", JsonString "lemma"]
  | C_Unknown   -> JsonObject
    [ "kind", JsonString "unknown"
    ; "term", JsonString (comp_to_string c)]
and pattern_to_json deep (c: pattern): Tac jsonValue =
  match c with
  | Pat_Constant c -> mk "constant" [ "value", vconst_to_json c]
  | Pat_Cons fv l -> 
             mk "constructor"
             [ "constructor", fv_to_json fv
             ; "args", JsonArray (T.map (fun (a, _) -> pattern_to_json deep a) l)] 
  | Pat_Var bv -> mk "var" [ "name", bv_to_json (deep + 1) "bv" bv ]
  | Pat_Wild bv -> mk "wild" [ "name", bv_to_json (deep + 1) "bv" bv ]
  | Pat_Dot_Term bv term -> mk "dot" [ "name", bv_to_json (deep + 1) "bv" bv
                                    ; "term", term_to_json' (deep + 1) term]

let xxx: string =
  _ by (
 let fv = (`(
  forall x. true
   // h: Prims.squash (forall (x: t). pred x) -> x: t -> int//Prims.Tot (Prims.squash (pred x))
 )) in
 let json = term_to_json' 0 fv in
 // let json = trytac (fun () -> term_to_json_native fv) in
 // let json = match json with | Some x -> x | None -> JsonNull in  
 let str  = stringify json "   " in
 exact (quote str)
 )



let rec hasPrefix (#a: eqtype) (l1 l2: list a) =
  match l1, l2 with
  | [], _ -> true
  | (hd1::tl1), (hd2::tl2) -> hd1 = hd2 && hasPrefix tl1 tl2
  | _ -> false


let hasPrefixStr s1 s2 = hasPrefix (list_of_string s1) (list_of_string s2)


let term_to_json (excludeNames: list string) name (t: term): Tac jsonValue
  = let err msg = mk "error" ["detail", JsonString "term_to_json failed, this is probably due to an unknown constant"; "message", JsonString msg] in
    if
      (
        Some? (find (fun bl -> hasPrefixStr bl name) excludeNames)
      )
    then (mk "PrimsEffect" ["name", JsonString name])
    else
    ( try (
          term_to_json' 0 t
          // match trytac (fun () -> term_to_json' 0 t) with
          // | Some x -> x
          // | None   -> err "trytac failed"
      )
      with | TacticFailure s -> err s
           | _ -> err "?"
    )

module SRL = Data.Serialize
let term_to_json_native_adapt (t: term): Tac SRL.serialized
   = SRL.serialize (term_to_json [] "***" t)
[@plugin]
let term_to_json_native (t: term): Tac jsonValue
   = SRL.deserialize (term_to_json_native_adapt t)

[@plugin]
let getTypes_asJSON (excludeNames: list string) (matchingAttr: option term): Tac jsonValue
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
                 "type_detailed", (
                             dump (">>>" ^ (concat "." name));
                             let x = (try (term_to_json excludeNames (concat "." name) typ) with
                              |_ -> JsonNull) in x
                             );
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
let export_types_to_file (excludeNames: list string) (matchingAttr: option term) filepath: Tac unit
  =  let o = getTypes_asJSON excludeNames matchingAttr
  in let json = stringify o "  " 
  in let _ = launch_process "sh" ["-c"; "cat - > " ^ filepath] json
  in ()

