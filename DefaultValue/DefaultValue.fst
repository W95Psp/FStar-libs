module DefaultValue

open FStar.Tactics.Typeclasses

class hasDefaultValue a = {
  def: a
}

instance intHasDefaultValue : hasDefaultValue int = {def = 0}
instance stringHasDefaultValue : hasDefaultValue string = {def = ""}
instance listHasDefaultValue #a : hasDefaultValue (list a) = {def = []}
instance optionHasDefaultValue #a [| hasDefaultValue a |] : hasDefaultValue (option a) = {def = Some def}
instance boolHasDefaultValue : hasDefaultValue bool = {def = false}

