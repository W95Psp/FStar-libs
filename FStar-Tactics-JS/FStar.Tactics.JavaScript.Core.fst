module FStar.Tactics.JavaScript.Core

open FStar.Tactics

module S = FStar.String
module L = FStar.List.Tot

module JSON = Data.JSON
open FStar.Tactics.JavaScript.Helpers

// type nativeConstructor
let ncMatch = fst//: string 
let ncConstruct = snd//: string
let t_nativeConstructors = list (string * (string * string))
noeq type jsConfig = 
  { nativeConstructors: t_nativeConstructors
  ; runtime: t_nativeConstructors -> string
  ; funs: list (string * string)
  }

let fv_to_js (config: jsConfig) (known: list string) (fv: fv): Tac string =
  let name = inspect_fv fv in
  let l = last (admit (); name) in
  let str = S.concat "_" name in
  let str_fstar = S.concat "." name in
  let isConstructor = isFirstCharUppercase l in
  if isConstructor
  then 
    begin
      dump ("config.funs ?'"^str_fstar^"' --> " ^ S.concat "|" (L.map fst config.funs));
      if Some? (L.find (fun (x, _) -> x = str_fstar) config.nativeConstructors)
       || Some? (L.find (fun (x, _) -> x = str_fstar) config.funs)
       || L.mem str_fstar known
      then str
      else
      ( match getArityOfConstructor name with
      | Some n -> 
        "_MkMK_(\"" ^ str ^ "\"," ^ string_of_int n ^ ")"
      | None -> fail "getArityOfConstructor failed"
      )
    end//"(" ^  ^ ")"
  else str

let rec term_to_js (known: list string) (config: jsConfig) t : Tac _ =
  match inspect t with
  | Tv_Var  bv -> bv_to_js bv
  | Tv_BVar bv -> bv_to_js bv
  | Tv_FVar fv -> fv_to_js config known fv
  | Tv_App f (arg, q) -> begin
    match q with
    | Q_Explicit -> 
      let f = term_to_js known config f in
      f ^ "("^term_to_js known config arg^")"
    | _ -> term_to_js known config f
    end
  | Tv_Abs binder body -> begin 
    let bv, aqual = inspect_binder binder in
    match aqual with
    | Q_Explicit -> "((" ^ bv_to_js bv ^ ") => " ^ term_to_js known config body ^ ")"
    | _ -> term_to_js known config body
    end
  | Tv_Const c -> printConst c
  | Tv_Type  _ -> "'GOT Tv_Type'"
  | Tv_Unknown -> "'GOT Tv_Unknown'"
  | Tv_Arrow  _ _ -> "'GOT Tv_Arrow'"
  | Tv_Refine bv _ -> bv_to_js bv
  | Tv_Uvar   _ _ -> "'GOT Tv_UVar'"
  | Tv_Match expr branches ->
       match_to_js known config expr branches
  | Tv_Let   r _ bv def body ->
    let bv = bv_to_js bv in
    let def = term_to_js known config def in
    let body = term_to_js known config body in
    "_Y(" ^ 
    begin match r with
    | true  -> bv 
    | false -> "_"
    end ^ " => " ^ def ^ ", " ^ bv ^ " => " ^ body ^ " )"
  | Tv_AscribedT e _ _ -> term_to_js known config e
  | Tv_AscribedC e _ _ -> term_to_js known config e
// and argv_to_js (arg: argv): Tac string =
//   let arg, _ = arg in
//   term_to_js arg
and pat_to_js (pat: pattern): Tac string
  = match pat with
  | Pat_Wild bv ->       "{bv: \"" ^ bv_to_js bv ^ "\"}"
  | Pat_Var bv ->        "{bv: \"" ^ bv_to_js bv ^ "\"}"
  | Pat_Dot_Term bv _ -> "{bv: \"" ^ bv_to_js bv ^ "\"}"
  | Pat_Cons cons pats -> 
    let pats = L.filter (fun (_, x) -> x = false) pats in
    let cons = "\"" ^ String.concat "_" (inspect_fv cons) ^ "\"" in
    "[" ^ String.concat ", " (cons::map (fun (p, _) -> pat_to_js p) pats) ^ "]"
  | Pat_Constant v -> printConst v
and match_to_js (known: list string) (config: jsConfig) (expr: term) (branches: list branch): Tac string =
  let jsBrs = mapJoin ", " (
      fun (pat, body) ->
        let bvs = map bv_to_js (bvs_of_pattern pat) in
        let bvs = remove_dup bvs in
        let o = "{" ^ String.concat "," bvs ^ "}" in
        "[" ^ pat_to_js pat ^ ", (" ^ o ^ ") => " ^ term_to_js known config body ^ "]"
    ) branches in
  "match([" ^ jsBrs ^ "], " ^ term_to_js known config expr ^ ")"


let jsDefOfName' (name: name) (known: list string) (config: jsConfig): Tac (list (string * string)) =
  let Some f = admit (); lookup_typ (top_env ()) name in
  match inspect_sigelt f with
  | Sg_Let r fv _ _ def ->
    let fv = fv_to_js config known fv in
    let def = term_to_js known config def in
    let make name def = name, def in
    if r
    then [make fv def]
    else 
       let nfv = "NonRecFun_" ^ fv in
         [make  fv nfv;make nfv def] // reverse order
  | Sg_Constructor _ _ ->
    let names = match getConstructorBinderNames name with | Some x -> x | None -> [] in
    let names = rename_on_dup_names_str names in
    let name = S.concat "_" name in
    let call = L.map JSON.stringify_string (name::names) in
    let call = S.concat ", " call in
    [name, "Make_Cons("^call^")"]
  // | Sg_Inductive name univs params typ' constructors ->
  //      -> let iVars = L.length params in
  //        let iCons = map (makeGenericRep'Cons iVars) constructors in
  //        {iCons = iCons; iName = name; iVars = iVars}
  | _ -> fail (String.concat "." name ^ " is not an inductive")
    
let jsDefOfName (name: name) (known: list string) (config: jsConfig): Tac jsConfig
  = { config
      with funs = jsDefOfName' name known config @ config.funs
    }

let jsDefOfStr (name: string) (config: jsConfig): Tac jsConfig
  = jsDefOfName (String.split ['.'] name) [] config


let printJsDef (name, def) =
  "\nlet " ^ dotTo_ name ^ " = " ^ def ^ ";\n"

let print' (config: jsConfig): Tac string
  = let { nativeConstructors; runtime; funs} = config in
      "//PRINT 'RUNTIME'\n\n"
    ^ runtime nativeConstructors
    ^ "\n\n// PRINT nativeConstructors\n\n"
    ^ mapJoin' "" printJsDef (L.map (fun (k, d) -> k, ncConstruct d) nativeConstructors)
    ^ "\n\n// PRINT funs\n\n"
    ^ mapJoin' "" printJsDef (L.rev funs)

let print (config: jsConfig) (t: term): Tac string
  = print' config
  ^ "\n\n//ENTRY POINT\n\n" ^ term_to_js [] config t

