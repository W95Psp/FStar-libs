module MkDoc.Test

open FStar.Tactics
open MkDoc
open Data.JSON
open Data.JSON.Types

open Data.Serialize.Helpers

// let forallInstSq () = pack (Tv_FVar (pack_fv ["FStar"; "Tactic"; "Logic"; "__forall_inst_sq"]))

let t = magic ()
let pred = magic ()

open FStar.Tactics.PatternMatching
let a: string = _ by (
  let x = tc (top_env ()) (`(forall x. true)) in
  let x = term_head (`(forall x. true)) in
  exact (quote x)
)


let x: string =
  _ by (
 let fv = (`(
  forall x. true
   // h: Prims.squash (forall (x: t). pred x) -> x: t -> int//Prims.Tot (Prims.squash (pred x))
 )) in
 let json = term_to_json_native fv in
 // let json = trytac (fun () -> term_to_json_native fv) in
 // let json = match json with | Some x -> x | None -> JsonNull in  
 let str  = stringify json "   " in
 exact (quote str)
 )

let exclude = ["Prims.Pure"; "Prims.Admit"; "Prims.Ghost"; "Prims.GHOST"
       ; "Prims.GTot"; "Prims.Lemma"; "Prims.PURE"; "Prims.M"
       ; "Prims.Tot"; "FStar.ST"; "FStar.Tactics.Effect"; "FStar.Classical"
       ; "FStar.All" //; "FStar.Pervasives.Div"; "FStar.Pervasives.ALL_h"
       ; "FStar.Pervasives"]

[@(Doc "s" ["x: adasd", "asd"]) show_def_in_doc]
type monessai =
  | A : int -> monessai

[@(Doc "s" ["x: adasd", "asd"]) show_def_in_doc]
let hey x = x + 1

let _: unit = _ by (
  export_types_to_file exclude None "result.json";
  exact (`())
)



