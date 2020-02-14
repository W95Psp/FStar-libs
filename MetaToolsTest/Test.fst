module Test

open MetaTools.BrowseTerm
open MetaTools.NamesOfTerm
open MetaTools.Erase
open MetaTools.Compiled

module U = MetaTools.Util
module L = FStar.List.Tot
module LP = FStar.List.Pure

let unlabel (x: labeled int): int = snd x

let add (x y: labeled int) = 
  unlabel x + unlabel y

open FStar.Tactics

let hey: (list (name * option (term * term)))
  = _ by (
    let x = erase_term_and_defs (`add) in
    exact (quote x)
  )

