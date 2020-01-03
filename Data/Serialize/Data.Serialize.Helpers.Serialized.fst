module Data.Serialize.Helpers.Serialized

open Data.Serialize.Types
open Data.Serialize.Helpers
module T = FStar.Tactics
module L = FStar.List.Tot

unfold let readName (x: serialized): T.name * serialized
  = let n::tlN,o = admit (); x in
    n, (tlN, o)
unfold let readInt  (x: serialized): int * serialized
  = let ns,(i::tlI, o) = admit (); x in
    i, (ns, (tlI, o))
unfold let readString (x: serialized): string * serialized
  = let ns,(is, (s::tlS, o)) = admit (); x in
    s, (ns, (is, (tlS, o)))
unfold let readBool (x: serialized): bool * serialized
  = let ns,(is, (ss, b::tlB)) = admit (); x in
    b, (ns, (is, (ss, tlB)))

unfold let appendName   (n: T.name) x: serialized = let names,                    ((r)) = x in n::names, r
unfold let appendInt    (i: int)      x: serialized = let names, (ints,              (r)) = x in names, (i::ints, r)
unfold let appendString (s: string) x: serialized = let names, (ints, (strings, bools)) = x in names, (ints, (s::strings, bools))
unfold let appendBool   (b: bool)   x: serialized = let names, (ints, (strings, bools)) = x in names, (ints, (strings, b::bools))


let appendList #t
  (appender: t -> serialized -> serialized)
  (v: list t) (s: serialized)
  : serialized
  = let s
      = L.fold_left
        (fun s v -> appender v s)
        s v
    in appendInt (L.length v) s
     

unfold let compose (f: serialized -> serialized) (g: serialized -> serialized) 
  : serialized -> serialized
  = fun x -> f (g x)

let emptySerialized: serialized = ([], ([], ([], [])))

let readList #t
  (f: serialized -> t * serialized)
  (s: serialized)
  : list t * serialized
  = let len, s = readInt s in
    mkerror ("readList " ^ string_of_int len);
    L.fold_left
      (fun (l, s) _ -> 
        let x, s = f s
        in x::l, s)
      ([], s)
      (mkList 1 len) 



