module FStar.Tactics.JavaScript
open FStar.Tactics

open FStar.Tactics.JavaScript.Core
include FStar.Tactics.JavaScript.Natives
open FStar.Tactics.JavaScript.Helpers

module L = FStar.List.Tot

let collectJSOf (t: term) (extra: list name) (c: jsConfig): Tac _ =
  let x = names_of_term t @! extra in
  let already: list string = L.map fst c.funs in
  let already: list string = already @! L.map fst c.nativeConstructors in
  let already: list name = L.map (String.split ['.']) already in
  let names = name_closure x already in
  fold_left (fun c n ->
    let names = L.map (String.concat ".") names in
    match trytac (fun _ -> jsDefOfName n names c) with
    | Some c -> c
    | None   -> c
  ) c names

[@plugin]
let jsCodeOf
  (additional_cons: t_nativeConstructors)
  (additional_funs: list (string * string))
  (extra: list string)
  (t: term)
  : Tac string
  = let c = { defaultConfig
              with
              funs = defaultConfig.funs @ additional_funs
            ; nativeConstructors = defaultConfig.nativeConstructors
                                 @ additional_cons
            } in
    let extra = L.map (String.split ['.']) extra in
    let c = collectJSOf t extra c in
    print c t
  
let print = print
let print' = print'

