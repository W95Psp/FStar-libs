module Data.Generics.Types

module T = FStar.Tactics

type argSumup (args: nat) =
  | AS_Int | AS_String | AS_Bool
  | AS_List: argSumup args -> argSumup args
  | AS_Inductive: T.name -> list (argSumup args) -> argSumup args
  | AS_TVar: (argIndex: nat {argIndex < args}) -> argSumup args

type consSumup (iVars: nat) = T.name * list (argSumup iVars)
type inductiveSumup =
  { iName: T.name
  ; iVars: nat
  ; iCons: list (consSumup iVars)
  }

let serialized = (
   list T.name
* (list int
* (list string
*  list bool
)))

