module MkDoc.Test

open FStar.Tactics
open MkDoc
open Data.JSON
open Data.JSON.Types

open Data.Serialize.Helpers

[@(Doc "s" ["x: adasd", "asd"]) show_def_in_doc]
type monessai =
  | A : int -> monessai

[@(Doc "s" ["x: adasd", "asd"]) show_def_in_doc]
let hey x = x + 1

let _: unit = _ by (
  export_types_to_file None "result.json";
  exact (`())
)

