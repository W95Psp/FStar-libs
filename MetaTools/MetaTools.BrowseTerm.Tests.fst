module MetaTools.BrowseTerm.Tests

open FStar.Tactics
open Control.Monoid

open FStar.Tactics.Typeclasses
open MetaTools.Util
open MetaTools.BrowseTerm

module L = FStar.List.Tot

let rebuiltTopLevel_works t: Tac term = 
  fst
  (browse_term #unit #unitIsMonoid
    (fun beforeTransform rebuildToplevel boundedVariables parents currentTerm -> 
      (( if beforeTransform
        then currentTerm
        else (
           let rebuilt = rebuildToplevel currentTerm in
           let strict = compare_term rebuilt t = FStar.Order.Eq in
           let relaxed = term_to_string rebuilt = term_to_string t in 
           ( if relaxed
             then ()
             else dump (
                      "(Err rebuiltTopLevel_works)\n## Child\n"
                    ^ term_to_string currentTerm
                    ^ "\n\n## Toplevel\n"
                    ^ term_to_string rebuilt
                    ^ "\n\n## Actual toplevel\n"
                    ^ term_to_string t
                    ^ "\n\n## Parents\n"
                    ^ term_heads_to_string parents
                  )
           );
           currentTerm
        )
      ), id_tac), ()
    )
    t)


let _: unit = _ by (
  let _ = rebuiltTopLevel_works (`(fun x y -> x + y + 4)) in
  exact (`())
)

