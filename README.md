Here is a centralized repo for some F* "libraries" I use. 

These are packed into sort of packages using https://github.com/W95Psp/fstar-nix-packer

# Data
## Data.Function
 - `Data.Function.const`
```
x: 'a -> _: 'b -> Prims.Tot 'a
```

 - `Data.Function.flip`
```
f: (_: 'a -> _: 'b -> Prims.Tot 'c) -> b: 'b -> a: 'a -> Prims.Tot 'c
```

 - `Data.Function.on`
```
f: (_: 'a -> _: 'a -> Prims.Tot 'b) -> t: (_: 'c -> Prims.Tot 'a) -> v: 'c -> w: 'c -> Prims.Tot 'b
```

 - `Data.Function.op_At_At`
```
g: (_: 'b -> Prims.Tot 'c) -> f: (_: 'a -> Prims.Tot 'b) -> a: 'a -> Prims.Tot 'c
```

 - `Data.Function.op_Bar_Greater`
```
v: 'a -> f: (_: 'a -> Prims.Tot 'b) -> Prims.Tot 'b
```

 - `Data.Function.op_Less_Bar`
```
f: (_: 'a -> Prims.Tot 'b) -> v: 'a -> Prims.Tot 'b
```

## Data.JSON
 - `Data.JSON.Parser.convert`
```
c: Prims.list (n: Prims.nat{n <= 9}) -> Prims.Tot Prims.nat
```

 - `Data.JSON.Parser.convert_digit`
```
c: FStar.Char.char -> Prims.Tot (n: Prims.nat{n <= 9})
```

 - `Data.JSON.Parser.jsonCharParser`
```
StarCombinator.Core.parser FStar.String.char
```

 - `Data.JSON.Parser.jsonStringParser`
```
StarCombinator.Core.parser Data.JSON.Types.jsonValue
```

 - `Data.JSON.Parser.jsonStringParser'`
```
StarCombinator.Core.parser Prims.string
```

 - `Data.JSON.Parser.match_list`
```

    l: FStar.String.char ->
    r: FStar.String.char ->
    s: StarCombinator.Core.parser _ ->
    i: StarCombinator.Core.parser _
  -> Prims.Tot (StarCombinator.Core.parser (Prims.list _))
```

 - `Data.JSON.Parser.parseArray`
```
_: Prims.unit -> Prims.Tot (StarCombinator.Core.parser Data.JSON.Types.jsonValue)
```

 - `Data.JSON.Parser.parseBool`
```
StarCombinator.Core.parser Data.JSON.Types.jsonValue
```

 - `Data.JSON.Parser.parseDecimalNumber`
```
StarCombinator.Core.parser Data.JSON.Types.decimalNumber
```

 - `Data.JSON.Parser.parseNull`
```
StarCombinator.Core.parser Data.JSON.Types.jsonValue
```

 - `Data.JSON.Parser.parseNumber`
```
StarCombinator.Core.parser Data.JSON.Types.jsonValue
```

 - `Data.JSON.Parser.parseObject`
```
_: Prims.unit -> Prims.Tot (StarCombinator.Core.parser Data.JSON.Types.jsonValue)
```

 - `Data.JSON.Parser.parseValue`
```
_: Prims.unit -> Prims.Tot (StarCombinator.Core.parser Data.JSON.Types.jsonValue)
```

 - `Data.JSON.Parser.parser`
```
source: Prims.string -> Prims.Tot (FStar.Pervasives.either Data.JSON.Types.jsonValue Prims.string)
```

 - `Data.JSON.Parser.test`
```
FStar.Pervasives.either Data.JSON.Types.jsonValue Prims.string
```

 - `Data.JSON.Stringify.escapeString`
```
s: Prims.string -> Prims.Tot Prims.string
```

 - `Data.JSON.Stringify.joinstr`
```
_: Prims.list Prims.string -> Prims.Tot Prims.string
```

 - `Data.JSON.Stringify.printDecimalNumber`
```
_: Data.JSON.Types.decimalNumber -> Prims.Tot Prims.string
```

 - `Data.JSON.Stringify.printDecimalNumber_h`
```
digits: Prims.list FStar.String.char -> n: Prims.int -> exp: Prims.int -> Prims.Tot Prims.string
```

 - `Data.JSON.Stringify.repeat`
```
n: Prims.nat -> s: Prims.string -> Prims.Tot Prims.string
```

 - `Data.JSON.Stringify.stringify`
```
value: Data.JSON.Types.jsonValue -> spaces: Prims.string -> Prims.Tot Prims.string
```

 - `Data.JSON.Stringify.stringify_helper`
```
jump: Prims.bool -> tab: Prims.string -> n: Prims.nat -> value: Data.JSON.Types.jsonValue
  -> Prims.Tot Prims.string
```

 - `Data.JSON.Types.decimalNumber`
```
Type0
```

 - `Data.JSON.Types.jsonValue`
```
Type0
```

 - `Data.JSON.decimalNumber_hasSerialize`
```
Data.Serialize.Typeclasses.hasSerialize Data.JSON.Types.decimalNumber
```

 - `Data.JSON.decimalNumber_serialize_decode`
```
x0: Data.Serialize.Types.serialized -> Prims.Tot Data.JSON.Types.decimalNumber
```

 - `Data.JSON.decimalNumber_serialize_decode_chainable`
```
x1: Data.Serialize.Types.serialized
  -> Prims.Tot (Data.JSON.Types.decimalNumber * Data.Serialize.Types.serialized)
```

 - `Data.JSON.decimalNumber_serialize_encode`
```
x19: Data.JSON.Types.decimalNumber -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.JSON.decimalNumber_serialize_encode_chainable`
```
x21: Data.JSON.Types.decimalNumber -> x22: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.JSON.jsonValue`
```
Type0
```

 - `Data.JSON.jsonValue_hasSerialize`
```
Data.Serialize.Typeclasses.hasSerialize Data.JSON.Types.jsonValue
```

 - `Data.JSON.jsonValue_serialize_decode`
```
x0: Data.Serialize.Types.serialized -> Prims.Tot Data.JSON.Types.jsonValue
```

 - `Data.JSON.jsonValue_serialize_decode_chainable`
```
x1: Data.Serialize.Types.serialized
  -> Prims.Tot (Data.JSON.Types.jsonValue * Data.Serialize.Types.serialized)
```

 - `Data.JSON.jsonValue_serialize_encode`
```
x30: Data.JSON.Types.jsonValue -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.JSON.jsonValue_serialize_encode_chainable`
```
x32: Data.JSON.Types.jsonValue -> x33: Data.Serialize.Types.serialized
  -> Prims.Tot
    (Prims.list FStar.Reflection.Types.name *
      (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
```

 - `Data.JSON.nat_serialize_decode_chainable`
```
s: Data.Serialize.Types.serialized -> Prims.Tot (Prims.nat * Data.Serialize.Types.serialized)
```

 - `Data.JSON.nat_serialize_encode_chainable`
```

    _: Prims.nat ->
    _:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.JSON.parse`
```
source: Prims.string -> Prims.Tot (FStar.Pervasives.either Data.JSON.Types.jsonValue Prims.string)
```

 - `Data.JSON.parse'`
```
source: Prims.string -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.JSON.stringify`
```
v: Data.JSON.Types.jsonValue -> Prims.Tot (spaces: Prims.string -> Prims.Tot Prims.string)
```

 - `Data.JSON.stringify'`
```
v: Data.Serialize.serialized -> Prims.Tot (spaces: Prims.string -> Prims.Tot Prims.string)
```

 - `Data.JSON.tuple2_hasSerialize`
```

    x28: Type ->
    x29: Type ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x30: Data.Serialize.Typeclasses.hasSerialize x28) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x31: Data.Serialize.Typeclasses.hasSerialize x29)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize (x28 * x29))
```

 - `Data.JSON.tuple2_serialize_decode`
```

    x0: (_: _ -> Prims.Tot (_ * _)) ->
    x1: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x2: Data.Serialize.Types.serialized
  -> Prims.Tot (_ * _)
```

 - `Data.JSON.tuple2_serialize_decode_chainable`
```

    x3: (_: _ -> Prims.Tot (_ * _)) ->
    x4: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x5: Data.Serialize.Types.serialized
  -> Prims.Tot ((_ * _) * _)
```

 - `Data.JSON.tuple2_serialize_encode`
```

    x18: (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot _) ->
    x19:
      (_: _ -> _: _
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x20: (_ * _)
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.JSON.tuple2_serialize_encode_chainable`
```

    x22: (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot _) ->
    x23:
      (_: _ -> _: _
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x24: (_ * _) ->
    x25: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```

## Data.Map
### Data.Map.Enumerable
#### Data.Map.Enumerable.NonOrdered
 - `Data.Map.Enumerable.NonOrdered.emHasToString`
```
(#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString a)
  -> Prims.Tot (ToString.hasToString (Data.Map.Enumerable.NonOrdered.enumerableMap a))
```

 - `Data.Map.Enumerable.NonOrdered.em_combine`
```

    m1: Data.Map.Enumerable.NonOrdered.enumerableMap 'a ->
    m2: Data.Map.Enumerable.NonOrdered.enumerableMap 'a ->
    f: (_: 'a -> _: 'a -> Prims.Tot 'a)
  -> Prims.Tot (Data.Map.Enumerable.NonOrdered.enumerableMap 'a)
```

 - `Data.Map.Enumerable.NonOrdered.em_equal`
```

    myEq: (_: a -> _: a -> Prims.Tot Prims.bool) ->
    m1: Data.Map.Enumerable.NonOrdered.enumerableMap a ->
    m2: Data.Map.Enumerable.NonOrdered.enumerableMap a
  -> Prims.Tot Prims.bool
```

 - `Data.Map.Enumerable.NonOrdered.em_get`
```
m: Data.Map.Enumerable.NonOrdered.enumerableMap 'a -> k: Prims.string -> Prims.Tot 'a
```

 - `Data.Map.Enumerable.NonOrdered.em_set`
```
m: Data.Map.Enumerable.NonOrdered.enumerableMap 'a -> k: Prims.string -> v: 'a
  -> Prims.Tot (Data.Map.Enumerable.NonOrdered.enumerableMap 'a)
```

 - `Data.Map.Enumerable.NonOrdered.enumerableMap`
```
a: Type -> Prims.Tot Type
```

 - `Data.Map.Enumerable.NonOrdered.enumerableMap'S`
```
a: Type -> Prims.Tot Type
```

 - `Data.Map.Enumerable.NonOrdered.enumerableMap'S'dec`
```

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: DefaultValue.hasDefaultValue a) ->
    m: Data.Map.Enumerable.NonOrdered.enumerableMap'S a
  -> Prims.Tot (Data.Map.Enumerable.NonOrdered.enumerableMap a)
```

 - `Data.Map.Enumerable.NonOrdered.enumerableMap'S'enc`
```
m: Data.Map.Enumerable.NonOrdered.enumerableMap a
  -> Prims.Tot (Data.Map.Enumerable.NonOrdered.enumerableMap'S a)
```

 - `Data.Map.Enumerable.NonOrdered.listToEnumerableSet`
```

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: DefaultValue.hasDefaultValue a) ->
    lst: Prims.list (Prims.string * a)
  -> Prims.Tot (Data.Map.Enumerable.NonOrdered.enumerableMap a)
```

 - `Data.Map.Enumerable.NonOrdered.listToEnumerableSet_resolver`
```

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: DefaultValue.hasDefaultValue a) ->
    l: Prims.list (Prims.string * a) ->
    query: Prims.string
  -> Prims.Tot a
```

 - `Data.Map.Enumerable.NonOrdered.state_to_em`
```
s: (_: Prims.string -> Prims.Tot _) -> Prims.Tot (Data.Map.Enumerable.NonOrdered.enumerableMap _)
```

## Data.Serialize
 - `Data.Serialize.Decode.change_last`
```
f: (_: 'a -> Prims.Tot 'a) -> l: Prims.list 'a -> Prims.Tot (Prims.list 'a)
```

 - `Data.Serialize.Decode.generateDecodeSerialize`
```
name: FStar.Reflection.Types.fv -> FStar.Tactics.Effect.Tac FStar.Reflection.Data.decls
```

 - `Data.Serialize.Decode.generateDecodeSerialize_for_inductiveSumup`
```
s: Data.Serialize.Types.inductiveSumup -> FStar.Tactics.Effect.Tac FStar.Reflection.Data.decls
```

 - `Data.Serialize.Decode.generateDecodeSerialize_term_for_argSumup`
```

    args_fun: Prims.list FStar.Reflection.Types.binder ->
    arg: Data.Serialize.Types.argSumup (FStar.List.Tot.Base.length args_fun)
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Decode.generateDecodeSerialize_term_for_consSumup`
```

    encoders: Prims.list FStar.Reflection.Types.binder {FStar.List.Tot.Base.length encoders = n} ->
    cons: Data.Serialize.Types.consSumup n ->
    serialized_inp: FStar.Reflection.Types.bv
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Decode.generateDecodeSerialize_term_for_inductiveSumup`
```
s: Data.Serialize.Types.inductiveSumup -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Decode.id_tac_term`
```
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Decode.transform_name_decode`
```
n: FStar.Reflection.Types.name -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.name
```

 - `Data.Serialize.Decode.transform_name_decode'`
```
n: FStar.Reflection.Types.name -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.name
```

 - `Data.Serialize.Encode.generateDecodeSerialize_term_for_argSumup`
```

    args_fun: Prims.list FStar.Reflection.Types.binder ->
    arg: Data.Serialize.Types.argSumup (FStar.List.Tot.Base.length args_fun)
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Encode.generateEncodeSerialize`
```
name: FStar.Reflection.Types.fv -> FStar.Tactics.Effect.Tac FStar.Reflection.Data.decls
```

 - `Data.Serialize.Encode.generateEncodeSerialize_for_inductiveSumup`
```
s: Data.Serialize.Types.inductiveSumup -> FStar.Tactics.Effect.Tac FStar.Reflection.Data.decls
```

 - `Data.Serialize.Encode.generateEncodeSerialize_term_for_inductiveSumup`
```
s: Data.Serialize.Types.inductiveSumup -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Encode.transform_name_encode`
```
n: FStar.Reflection.Types.name -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.name
```

 - `Data.Serialize.Encode.transform_name_encode'`
```
n: FStar.Reflection.Types.name -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.name
```

 - `Data.Serialize.Helpers.Serialized.appendBool`
```

    b: Prims.bool ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.Helpers.Serialized.appendInt`
```

    i: Prims.int ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.Helpers.Serialized.appendList`
```

    appender:
      (_: t -> _: Data.Serialize.Types.serialized -> Prims.Tot Data.Serialize.Types.serialized) ->
    v: Prims.list t ->
    s: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.Helpers.Serialized.appendName`
```

    n: FStar.Reflection.Types.name ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.Helpers.Serialized.appendString`
```

    s: Prims.string ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.Helpers.Serialized.compose`
```

    f: (_: Data.Serialize.Types.serialized -> Prims.Tot Data.Serialize.Types.serialized) ->
    g: (_: Data.Serialize.Types.serialized -> Prims.Tot Data.Serialize.Types.serialized)
  -> Prims.Tot (_: Data.Serialize.Types.serialized -> Prims.Tot Data.Serialize.Types.serialized)
```

 - `Data.Serialize.Helpers.Serialized.emptySerialized`
```
Data.Serialize.Types.serialized
```

 - `Data.Serialize.Helpers.Serialized.readBool`
```
x: Data.Serialize.Types.serialized -> Prims.Tot (Prims.bool * Data.Serialize.Types.serialized)
```

 - `Data.Serialize.Helpers.Serialized.readInt`
```
x: Data.Serialize.Types.serialized -> Prims.Tot (Prims.int * Data.Serialize.Types.serialized)
```

 - `Data.Serialize.Helpers.Serialized.readList`
```

    f: (_: Data.Serialize.Types.serialized -> Prims.Tot (t * Data.Serialize.Types.serialized)) ->
    s: Data.Serialize.Types.serialized
  -> Prims.Tot (Prims.list t * Data.Serialize.Types.serialized)
```

 - `Data.Serialize.Helpers.Serialized.readName`
```
x: Data.Serialize.Types.serialized
  -> Prims.Tot (FStar.Reflection.Types.name * Data.Serialize.Types.serialized)
```

 - `Data.Serialize.Helpers.Serialized.readString`
```
x: Data.Serialize.Types.serialized -> Prims.Tot (Prims.string * Data.Serialize.Types.serialized)
```

 - `Data.Serialize.Helpers.Test.call1_test`
```
Prims.int
```

 - `Data.Serialize.Helpers.Test.call2_test`
```
Prims.int
```

 - `Data.Serialize.Helpers.Test.mkLet_tup'_test`
```
Prims.int
```

 - `Data.Serialize.Helpers.Test.mkMatchInductive_test_typ`
```
Type0
```

 - `Data.Serialize.Helpers.Test.mkMatchInt_test`
```
Prims.int
```

 - `Data.Serialize.Helpers.Test.mkMatchInt_test2`
```
x0: Prims.int -> Prims.Tot Prims.int
```

 - `Data.Serialize.Helpers.add_admit`
```
body: FStar.Reflection.Types.term -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Helpers.add_admit_decr_lex`
```
v: FStar.Reflection.Types.term -> body: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Helpers.admitMe`
```
n: _ -> Prims.Tot _
```

 - `Data.Serialize.Helpers.admitTerm`
```
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Helpers.argvToBinder`
```
x: FStar.Reflection.Data.argv -> FStar.Tactics.Effect.TAC FStar.Reflection.Types.binder
```

 - `Data.Serialize.Helpers.binderName`
```
n: Prims.string -> Prims.Tot FStar.Reflection.Types.binder
```

 - `Data.Serialize.Helpers.binderNth`
```
n: Prims.int -> Prims.Tot FStar.Reflection.Types.binder
```

 - `Data.Serialize.Helpers.binderToArgv`
```
b: FStar.Reflection.Types.binder
  -> FStar.Tactics.Effect.TAC (FStar.Reflection.Types.term * FStar.Reflection.Data.aqualv)
```

 - `Data.Serialize.Helpers.bvName`
```
n: Prims.string -> Prims.Tot FStar.Reflection.Types.bv
```

 - `Data.Serialize.Helpers.bvNth`
```
n: Prims.int -> Prims.Tot FStar.Reflection.Types.bv
```

 - `Data.Serialize.Helpers.call1`
```
f: FStar.Reflection.Types.term -> arg: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Helpers.call2`
```

    f: FStar.Reflection.Types.term ->
    arg1: FStar.Reflection.Types.term ->
    arg2: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Helpers.call3`
```

    f: FStar.Reflection.Types.term ->
    arg1: FStar.Reflection.Types.term ->
    arg2: FStar.Reflection.Types.term ->
    arg3: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Helpers.findIndex`
```
x: FStar.Reflection.Types.bv -> l: Prims.list FStar.Reflection.Types.bv
  -> FStar.Tactics.Effect.Tac Prims.nat
```

 - `Data.Serialize.Helpers.findIndex'`
```
x: FStar.Reflection.Types.bv -> n: Prims.nat -> l: Prims.list FStar.Reflection.Types.bv
  -> FStar.Tactics.Effect.Tac Prims.nat
```

 - `Data.Serialize.Helpers.fvOf`
```
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.TAC FStar.Reflection.Types.fv
```

 - `Data.Serialize.Helpers.last`
```
l: Prims.list 'a -> FStar.Tactics.Effect.Tac 'a
```

 - `Data.Serialize.Helpers.lex`
```
l: FStar.Reflection.Types.term -> r: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.TAC FStar.Reflection.Types.term
```

 - `Data.Serialize.Helpers.lookup_typ'`
```
env: FStar.Reflection.Types.env -> name: FStar.Reflection.Types.name
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.sigelt
```

 - `Data.Serialize.Helpers.makeEitherType`
```
types: Prims.list FStar.Reflection.Types.typ -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.typ
```

 - `Data.Serialize.Helpers.makeOptionType`
```
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.typ
```

 - `Data.Serialize.Helpers.makeTupleType`
```
types: Prims.list FStar.Reflection.Types.typ -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.typ
```

 - `Data.Serialize.Helpers.mkLet_tup`
```

    def: FStar.Reflection.Types.term ->
    body:
      (_: FStar.Reflection.Types.bv -> _: FStar.Reflection.Types.bv
          -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term)
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Helpers.mkLet_tup'`
```
def: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac
    ((_: FStar.Reflection.Types.term -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term) *
      (FStar.Reflection.Types.bv * FStar.Reflection.Types.bv))
```

 - `Data.Serialize.Helpers.mkList`
```
min: Prims.int -> max: Prims.int -> Prims.Tot (Prims.list Prims.int)
```

 - `Data.Serialize.Helpers.mkMatchInductive`
```

    s: Data.Serialize.Types.inductiveSumup ->
    head: FStar.Reflection.Types.term ->
    bodies:
      Prims.list (_: Prims.list FStar.Reflection.Types.bv
            -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term)
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Helpers.mkMatchInt`
```
n: FStar.Reflection.Types.term -> bodies: Prims.list FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Helpers.mkTupleType`
```
a: Type -> b: Type -> Prims.Tot Type
```

 - `Data.Serialize.Helpers.mkTupleTypeTac`
```
a: FStar.Reflection.Types.typ -> b: FStar.Reflection.Types.typ
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.typ
```

 - `Data.Serialize.Helpers.mk_abs`
```
bs: Prims.list FStar.Reflection.Types.binder -> body: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Helpers.mkerror`
```
_: Prims.string -> Prims.Tot t
```

 - `Data.Serialize.Helpers.nameCurMod'`
```
n: FStar.Reflection.Types.name -> f: (_: Prims.string -> Prims.Tot Prims.string)
  -> FStar.Tactics.Effect.TAC (Prims.list Prims.string)
```

 - `Data.Serialize.Helpers.name_to_term`
```
n: FStar.Reflection.Types.name -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Helpers.norm_term'`
```
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `Data.Serialize.Helpers.withIndex`
```
l: Prims.list 'a -> Prims.Tot (Prims.list (Prims.int * 'a))
```

 - `Data.Serialize.Helpers.withIndex_helper`
```
l: Prims.list 'a -> n: Prims.int -> Prims.Tot (Prims.list (Prims.int * 'a))
```

 - `Data.Serialize.Rep.makeGenericRep`
```
name: FStar.Reflection.Types.fv -> FStar.Tactics.Effect.Tac Data.Serialize.Types.inductiveSumup
```

 - `Data.Serialize.Rep.makeGenericRep'Cons`
```
iVars: Prims.nat -> name: FStar.Reflection.Types.name
  -> FStar.Tactics.Effect.Tac (Data.Serialize.Types.consSumup iVars)
```

 - `Data.Serialize.Rep.makeGenericRep'Cons'Arg`
```
iVars: Prims.nat -> bvs: Prims.list FStar.Reflection.Types.bv -> t: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac (Data.Serialize.Types.argSumup iVars)
```

 - `Data.Serialize.Typeclasses.deserialize`
```

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: Data.Serialize.Typeclasses.hasSerialize a) ->
    v: Data.Serialize.Types.serialized
  -> Prims.Tot a
```

 - `Data.Serialize.Typeclasses.deserialize_chainable`
```
(#[FStar.Tactics.Typeclasses.tcresolve ()] d: Data.Serialize.Typeclasses.hasSerialize a)
  -> Prims.Tot
    (_: Data.Serialize.Types.serialized -> Prims.Tot (a * Data.Serialize.Types.serialized))
```

 - `Data.Serialize.Typeclasses.generateSerialize`
```
t: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.TAC (Prims.list FStar.Reflection.Types.sigelt)
```

 - `Data.Serialize.Typeclasses.generateSerialize'`
```
tfv: FStar.Reflection.Types.fv
  -> FStar.Tactics.Effect.TAC (Prims.list FStar.Reflection.Types.sigelt)
```

 - `Data.Serialize.Typeclasses.hasSerialize`
```
a: Type -> Prims.Tot Type
```

 - `Data.Serialize.Typeclasses.intHasSerialize`
```
Data.Serialize.Typeclasses.hasSerialize Prims.int
```

 - `Data.Serialize.Typeclasses.listIntHasSerialize`
```
Prims.Tot (Data.Serialize.Typeclasses.hasSerialize (Prims.list a))
```

 - `Data.Serialize.Typeclasses.makeHasSerializeInstance`
```

    s: Data.Serialize.Types.inductiveSumup ->
    encode: FStar.Reflection.Types.fv ->
    decode: FStar.Reflection.Types.fv
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Data.decls
```

 - `Data.Serialize.Typeclasses.serialize`
```
(#[FStar.Tactics.Typeclasses.tcresolve ()] _: Data.Serialize.Typeclasses.hasSerialize a) -> v: a
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.Typeclasses.serialize_chainable`
```
(#[FStar.Tactics.Typeclasses.tcresolve ()] d: Data.Serialize.Typeclasses.hasSerialize a)
  -> Prims.Tot
    (_: a -> _: Data.Serialize.Types.serialized -> Prims.Tot Data.Serialize.Types.serialized)
```

 - `Data.Serialize.Typeclasses.xx`
```
_: Data.Serialize.Types.serialized -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.Types.argSumup`
```
args: Prims.nat -> Prims.Tot Type0
```

 - `Data.Serialize.Types.consSumup`
```
iVars: Prims.nat -> Prims.Tot Type0
```

 - `Data.Serialize.Types.inductiveSumup`
```
Type0
```

 - `Data.Serialize.Types.serialize_size`
```
_: Data.Serialize.Types.serialized -> Prims.Tot Prims.nat
```

 - `Data.Serialize.Types.serialized`
```
Type0
```

 - `Data.Serialize.Types.tserialized`
```
a: _ -> Prims.Tot Type0
```

 - `Data.Serialize.boolHasSerialize`
```
Data.Serialize.Typeclasses.hasSerialize Prims.bool
```

 - `Data.Serialize.bool_serialize_decode_chainable`
```
x: Data.Serialize.Types.serialized -> Prims.Tot (Prims.bool * Data.Serialize.Types.serialized)
```

 - `Data.Serialize.bool_serialize_encode_chainable`
```

    b: Prims.bool ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.deserialize`
```

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: Data.Serialize.hasSerialize a) ->
    v: Data.Serialize.Types.serialized
  -> Prims.Tot a
```

 - `Data.Serialize.either_hasSerialize`
```

    x28: Type ->
    x29: Type ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x30: Data.Serialize.Typeclasses.hasSerialize x28) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x31: Data.Serialize.Typeclasses.hasSerialize x29)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize (FStar.Pervasives.either x28 x29))
```

 - `Data.Serialize.either_serialize_decode`
```

    x0: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x1: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x2: Data.Serialize.Types.serialized
  -> Prims.Tot (FStar.Pervasives.either _ _)
```

 - `Data.Serialize.either_serialize_decode_chainable`
```

    x3: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x4: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x5: Data.Serialize.Types.serialized
  -> Prims.Tot (FStar.Pervasives.either _ _ * _)
```

 - `Data.Serialize.either_serialize_encode`
```

    x18:
      (_: _ -> _: Data.Serialize.Types.serialized
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x19:
      (_: _ -> _: Data.Serialize.Types.serialized
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x20: FStar.Pervasives.either _ _
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.either_serialize_encode_chainable`
```

    x22:
      (_: _ -> _: Data.Serialize.Types.serialized
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x23:
      (_: _ -> _: Data.Serialize.Types.serialized
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x24: FStar.Pervasives.either _ _ ->
    x25: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.fvOf`
```
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.TAC FStar.Reflection.Types.fv
```

 - `Data.Serialize.generateDecodeSerialize`
```
name: FStar.Reflection.Types.fv -> FStar.Tactics.Effect.Tac FStar.Reflection.Data.decls
```

 - `Data.Serialize.generateEncodeSerialize`
```
name: FStar.Reflection.Types.fv -> FStar.Tactics.Effect.Tac FStar.Reflection.Data.decls
```

 - `Data.Serialize.generateSerialize`
```
t: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.TAC (Prims.list FStar.Reflection.Types.sigelt)
```

 - `Data.Serialize.generateSerialize'`
```
tfv: FStar.Reflection.Types.fv
  -> FStar.Tactics.Effect.TAC (Prims.list FStar.Reflection.Types.sigelt)
```

 - `Data.Serialize.hasSerialize`
```
a: Type -> Prims.Tot Type
```

 - `Data.Serialize.intHasSerialize`
```
Data.Serialize.Typeclasses.hasSerialize Prims.int
```

 - `Data.Serialize.int_serialize_decode_chainable`
```
x: Data.Serialize.Types.serialized -> Prims.Tot (Prims.int * Data.Serialize.Types.serialized)
```

 - `Data.Serialize.int_serialize_encode_chainable`
```

    i: Prims.int ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.listHasSerialize`
```
a: Type -> (#[FStar.Tactics.Typeclasses.tcresolve ()] _: Data.Serialize.Typeclasses.hasSerialize a)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize (Prims.list a))
```

 - `Data.Serialize.list_serialize_decode_chainable`
```

    f: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * Data.Serialize.Types.serialized)) ->
    s: Data.Serialize.Types.serialized
  -> Prims.Tot (Prims.list _ * Data.Serialize.Types.serialized)
```

 - `Data.Serialize.list_serialize_encode_chainable`
```

    appender:
      (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot Data.Serialize.Types.serialized) ->
    v: Prims.list _ ->
    s: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.nameHasSerialize`
```
Data.Serialize.Typeclasses.hasSerialize FStar.Reflection.Types.name
```

 - `Data.Serialize.name_serialize_decode_chainable`
```
x: Data.Serialize.Types.serialized
  -> Prims.Tot (FStar.Reflection.Types.name * Data.Serialize.Types.serialized)
```

 - `Data.Serialize.name_serialize_encode_chainable`
```

    n: FStar.Reflection.Types.name ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.nat_serialize_decode_chainable`
```
s: Data.Serialize.Types.serialized -> Prims.Tot (Prims.nat * Data.Serialize.Types.serialized)
```

 - `Data.Serialize.nat_serialize_encode_chainable`
```

    _: Prims.nat ->
    _:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.option_hasSerialize`
```

    x19: Type ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x20: Data.Serialize.Typeclasses.hasSerialize x19)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize (FStar.Pervasives.Native.option x19))
```

 - `Data.Serialize.option_serialize_decode`
```

    x0: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * Data.Serialize.Types.serialized)) ->
    x1: Data.Serialize.Types.serialized
  -> Prims.Tot (FStar.Pervasives.Native.option _)
```

 - `Data.Serialize.option_serialize_decode_chainable`
```

    x2: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * Data.Serialize.Types.serialized)) ->
    x3: Data.Serialize.Types.serialized
  -> Prims.Tot (FStar.Pervasives.Native.option _ * Data.Serialize.Types.serialized)
```

 - `Data.Serialize.option_serialize_encode`
```

    x12:
      (_: _ -> _: Data.Serialize.Types.serialized
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x13: FStar.Pervasives.Native.option _
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.option_serialize_encode_chainable`
```

    x15:
      (_: _ -> _: Data.Serialize.Types.serialized
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x16: FStar.Pervasives.Native.option _ ->
    x17: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.serialize`
```
(#[FStar.Tactics.Typeclasses.tcresolve ()] _: Data.Serialize.hasSerialize a) -> v: a
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.serialized`
```
Type0
```

 - `Data.Serialize.stringHasSerialize`
```
Data.Serialize.Typeclasses.hasSerialize Prims.string
```

 - `Data.Serialize.string_serialize_decode_chainable`
```
x: Data.Serialize.Types.serialized -> Prims.Tot (Prims.string * Data.Serialize.Types.serialized)
```

 - `Data.Serialize.string_serialize_encode_chainable`
```

    s: Prims.string ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.tserialized`
```
a: _ -> Prims.Tot Type0
```

 - `Data.Serialize.tuple2_hasSerialize`
```

    x28: Type ->
    x29: Type ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x30: Data.Serialize.Typeclasses.hasSerialize x28) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x31: Data.Serialize.Typeclasses.hasSerialize x29)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize (x28 * x29))
```

 - `Data.Serialize.tuple2_serialize_decode`
```

    x0: (_: _ -> Prims.Tot (_ * _)) ->
    x1: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x2: Data.Serialize.Types.serialized
  -> Prims.Tot (_ * _)
```

 - `Data.Serialize.tuple2_serialize_decode_chainable`
```

    x3: (_: _ -> Prims.Tot (_ * _)) ->
    x4: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x5: Data.Serialize.Types.serialized
  -> Prims.Tot ((_ * _) * _)
```

 - `Data.Serialize.tuple2_serialize_encode`
```

    x18: (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot _) ->
    x19:
      (_: _ -> _: _
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x20: (_ * _)
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.tuple2_serialize_encode_chainable`
```

    x22: (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot _) ->
    x23:
      (_: _ -> _: _
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x24: (_ * _) ->
    x25: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.tuple3_hasSerialize`
```

    x37: Type ->
    x38: Type ->
    x39: Type ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x40: Data.Serialize.Typeclasses.hasSerialize x37) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x41: Data.Serialize.Typeclasses.hasSerialize x38) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x42: Data.Serialize.Typeclasses.hasSerialize x39)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize ((x37 * x38) * x39))
```

 - `Data.Serialize.tuple3_serialize_decode`
```

    x0: (_: _ -> Prims.Tot (_ * _)) ->
    x1: (_: _ -> Prims.Tot (_ * _)) ->
    x2: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x3: Data.Serialize.Types.serialized
  -> Prims.Tot ((_ * _) * _)
```

 - `Data.Serialize.tuple3_serialize_decode_chainable`
```

    x4: (_: _ -> Prims.Tot (_ * _)) ->
    x5: (_: _ -> Prims.Tot (_ * _)) ->
    x6: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x7: Data.Serialize.Types.serialized
  -> Prims.Tot (((_ * _) * _) * _)
```

 - `Data.Serialize.tuple3_serialize_encode`
```

    x24: (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot _) ->
    x25: (_: _ -> _: _ -> Prims.Tot _) ->
    x26:
      (_: _ -> _: _
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x27: ((_ * _) * _)
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.tuple3_serialize_encode_chainable`
```

    x29: (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot _) ->
    x30: (_: _ -> _: _ -> Prims.Tot _) ->
    x31:
      (_: _ -> _: _
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x32: ((_ * _) * _) ->
    x33: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.tuple4_hasSerialize`
```

    x46: Type ->
    x47: Type ->
    x48: Type ->
    x49: Type ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x50: Data.Serialize.Typeclasses.hasSerialize x46) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x51: Data.Serialize.Typeclasses.hasSerialize x47) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x52: Data.Serialize.Typeclasses.hasSerialize x48) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x53: Data.Serialize.Typeclasses.hasSerialize x49)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize (((x46 * x47) * x48) * x49))
```

 - `Data.Serialize.tuple4_serialize_decode`
```

    x0: (_: _ -> Prims.Tot (_ * _)) ->
    x1: (_: _ -> Prims.Tot (_ * _)) ->
    x2: (_: _ -> Prims.Tot (_ * _)) ->
    x3: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x4: Data.Serialize.Types.serialized
  -> Prims.Tot (((_ * _) * _) * _)
```

 - `Data.Serialize.tuple4_serialize_decode_chainable`
```

    x5: (_: _ -> Prims.Tot (_ * _)) ->
    x6: (_: _ -> Prims.Tot (_ * _)) ->
    x7: (_: _ -> Prims.Tot (_ * _)) ->
    x8: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x9: Data.Serialize.Types.serialized
  -> Prims.Tot ((((_ * _) * _) * _) * _)
```

 - `Data.Serialize.tuple4_serialize_encode`
```

    x30: (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot _) ->
    x31: (_: _ -> _: _ -> Prims.Tot _) ->
    x32: (_: _ -> _: _ -> Prims.Tot _) ->
    x33:
      (_: _ -> _: _
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x34: (((_ * _) * _) * _)
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.tuple4_serialize_encode_chainable`
```

    x36: (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot _) ->
    x37: (_: _ -> _: _ -> Prims.Tot _) ->
    x38: (_: _ -> _: _ -> Prims.Tot _) ->
    x39:
      (_: _ -> _: _
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x40: (((_ * _) * _) * _) ->
    x41: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.tuple5_hasSerialize`
```

    x55: Type ->
    x56: Type ->
    x57: Type ->
    x58: Type ->
    x59: Type ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x60: Data.Serialize.Typeclasses.hasSerialize x55) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x61: Data.Serialize.Typeclasses.hasSerialize x56) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x62: Data.Serialize.Typeclasses.hasSerialize x57) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x63: Data.Serialize.Typeclasses.hasSerialize x58) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x64: Data.Serialize.Typeclasses.hasSerialize x59)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize ((((x55 * x56) * x57) * x58) * x59))
```

 - `Data.Serialize.tuple5_serialize_decode`
```

    x0: (_: _ -> Prims.Tot (_ * _)) ->
    x1: (_: _ -> Prims.Tot (_ * _)) ->
    x2: (_: _ -> Prims.Tot (_ * _)) ->
    x3: (_: _ -> Prims.Tot (_ * _)) ->
    x4: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x5: Data.Serialize.Types.serialized
  -> Prims.Tot ((((_ * _) * _) * _) * _)
```

 - `Data.Serialize.tuple5_serialize_decode_chainable`
```

    x6: (_: _ -> Prims.Tot (_ * _)) ->
    x7: (_: _ -> Prims.Tot (_ * _)) ->
    x8: (_: _ -> Prims.Tot (_ * _)) ->
    x9: (_: _ -> Prims.Tot (_ * _)) ->
    x10: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x11: Data.Serialize.Types.serialized
  -> Prims.Tot (((((_ * _) * _) * _) * _) * _)
```

 - `Data.Serialize.tuple5_serialize_encode`
```

    x36: (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot _) ->
    x37: (_: _ -> _: _ -> Prims.Tot _) ->
    x38: (_: _ -> _: _ -> Prims.Tot _) ->
    x39: (_: _ -> _: _ -> Prims.Tot _) ->
    x40:
      (_: _ -> _: _
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x41: ((((_ * _) * _) * _) * _)
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.tuple5_serialize_encode_chainable`
```

    x43: (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot _) ->
    x44: (_: _ -> _: _ -> Prims.Tot _) ->
    x45: (_: _ -> _: _ -> Prims.Tot _) ->
    x46: (_: _ -> _: _ -> Prims.Tot _) ->
    x47:
      (_: _ -> _: _
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x48: ((((_ * _) * _) * _) * _) ->
    x49: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.tuple6_hasSerialize`
```

    x64: Type ->
    x65: Type ->
    x66: Type ->
    x67: Type ->
    x68: Type ->
    x69: Type ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x70: Data.Serialize.Typeclasses.hasSerialize x64) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x71: Data.Serialize.Typeclasses.hasSerialize x65) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x72: Data.Serialize.Typeclasses.hasSerialize x66) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x73: Data.Serialize.Typeclasses.hasSerialize x67) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x74: Data.Serialize.Typeclasses.hasSerialize x68) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x75: Data.Serialize.Typeclasses.hasSerialize x69)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize (((((x64 * x65) * x66) * x67) * x68) * x69))
```

 - `Data.Serialize.tuple6_serialize_decode`
```

    x0: (_: _ -> Prims.Tot (_ * _)) ->
    x1: (_: _ -> Prims.Tot (_ * _)) ->
    x2: (_: _ -> Prims.Tot (_ * _)) ->
    x3: (_: _ -> Prims.Tot (_ * _)) ->
    x4: (_: _ -> Prims.Tot (_ * _)) ->
    x5: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x6: Data.Serialize.Types.serialized
  -> Prims.Tot (((((_ * _) * _) * _) * _) * _)
```

 - `Data.Serialize.tuple6_serialize_decode_chainable`
```

    x7: (_: _ -> Prims.Tot (_ * _)) ->
    x8: (_: _ -> Prims.Tot (_ * _)) ->
    x9: (_: _ -> Prims.Tot (_ * _)) ->
    x10: (_: _ -> Prims.Tot (_ * _)) ->
    x11: (_: _ -> Prims.Tot (_ * _)) ->
    x12: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x13: Data.Serialize.Types.serialized
  -> Prims.Tot ((((((_ * _) * _) * _) * _) * _) * _)
```

 - `Data.Serialize.tuple6_serialize_encode`
```

    x42: (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot _) ->
    x43: (_: _ -> _: _ -> Prims.Tot _) ->
    x44: (_: _ -> _: _ -> Prims.Tot _) ->
    x45: (_: _ -> _: _ -> Prims.Tot _) ->
    x46: (_: _ -> _: _ -> Prims.Tot _) ->
    x47:
      (_: _ -> _: _
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x48: (((((_ * _) * _) * _) * _) * _)
  -> Prims.Tot Data.Serialize.Types.serialized
```

 - `Data.Serialize.tuple6_serialize_encode_chainable`
```

    x50: (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot _) ->
    x51: (_: _ -> _: _ -> Prims.Tot _) ->
    x52: (_: _ -> _: _ -> Prims.Tot _) ->
    x53: (_: _ -> _: _ -> Prims.Tot _) ->
    x54: (_: _ -> _: _ -> Prims.Tot _) ->
    x55:
      (_: _ -> _: _
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x56: (((((_ * _) * _) * _) * _) * _) ->
    x57: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```

## Data.Set
### Data.Set.Computable
#### Data.Set.Computable.NonOrdered
 - `Data.Set.Computable.NonOrdered.PartialOrder.csetLPO`
```
Prims.Tot (PartialOrder.hasLPartialOrder (Data.Set.Computable.NonOrdered.set c))
```

 - `Data.Set.Computable.NonOrdered.PartialOrder.gsetLPO`
```
Prims.Tot (PartialOrder.hasLPartialOrder (FStar.GSet.set c))
```

 - `Data.Set.Computable.NonOrdered.add_in_set`
```
x: t -> s: Data.Set.Computable.NonOrdered.set t -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```

 - `Data.Set.Computable.NonOrdered.add_in_set'`
```
s: Prims.list t {Data.Set.Computable.NonOrdered.no_dup s} -> x: t
  -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```

 - `Data.Set.Computable.NonOrdered.cset_to_set`
```
s: Data.Set.Computable.NonOrdered.set t -> Prims.Tot (FStar.GSet.set t)
```

 - `Data.Set.Computable.NonOrdered.dup_behead`
```
x: t -> a: Prims.list t {Data.Set.Computable.NonOrdered.no_dup (x :: a)}
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.no_dup a) []
```

 - `Data.Set.Computable.NonOrdered.equal`
```
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t
  -> Prims.Tot Type0
```

 - `Data.Set.Computable.NonOrdered.findAndRemove`
```
a: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Tot (Prims.bool * Data.Set.Computable.NonOrdered.set t)
```

 - `Data.Set.Computable.NonOrdered.intersect`
```
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t
  -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```

 - `Data.Set.Computable.NonOrdered.lemma_convert_wf`
```
s: Data.Set.Computable.NonOrdered.set t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.equal (Data.Set.Computable.NonOrdered.list_to_set (Data.Set.Computable.NonOrdered.set_to_list 
                  s))
          s)
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_intermediaire`
```
a: Data.Set.Computable.NonOrdered.set t -> h: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.intersect a [h] == [h] \/
        Data.Set.Computable.NonOrdered.intersect a [h] == [])
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_intersect_cons_l`
```
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> h: t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.no_dup (h :: a) ==>
        ~(h = x) ==>
        Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect (h :: a) b) ==
        Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b))
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_intersect_cons_r`
```

    a: Data.Set.Computable.NonOrdered.set t ->
    b: Data.Set.Computable.NonOrdered.set t ->
    h: t{Data.Set.Computable.NonOrdered.no_dup (h :: b)} ->
    x: t{~(h = x)}
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a (h :: b)) ==
        Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b))
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_intersect_empty_l`
```
a: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect [] a) = false)
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_intersect_empty_r`
```
a: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a []) = false)
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_intersect_left`
```
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b) ==>
        Data.Set.Computable.NonOrdered.mem x a)
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_intersect_mem_both`
```
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b) ==>
        Data.Set.Computable.NonOrdered.mem x a /\ Data.Set.Computable.NonOrdered.mem x b)
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_intersect_mem_equiv`
```
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b) <==>
        Data.Set.Computable.NonOrdered.mem x a /\ Data.Set.Computable.NonOrdered.mem x b)
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_intersect_not_left`
```

    a: Data.Set.Computable.NonOrdered.set t ->
    b: Data.Set.Computable.NonOrdered.set t ->
    x: t{~(Data.Set.Computable.NonOrdered.mem x a)}
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b) == false)
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_intersect_not_right`
```
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x b == false ==>
        Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b) == false
      )
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_memIntersect_memLeft`
```
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b) ==>
        Data.Set.Computable.NonOrdered.mem x a)
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_memIntersect_memRight`
```
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b) ==>
        Data.Set.Computable.NonOrdered.mem x b)
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_mem_both_intersect`
```
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x a /\ Data.Set.Computable.NonOrdered.mem x b ==>
        Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b))
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_mem_empty`
```
x: t -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.mem x [] = false) []
```

 - `Data.Set.Computable.NonOrdered.lemma_no_dup_remove`
```
l: Prims.list t {Data.Set.Computable.NonOrdered.no_dup l} -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.no_dup (Data.Set.Computable.NonOrdered.remove l x))
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_remove'`
```
a: Data.Set.Computable.NonOrdered.set t -> x: t -> y: t{x <> y}
  -> Prims.Lemma Prims.unit
      (FStar.List.Tot.Base.mem x a ==>
        Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.remove a y))
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_remove_irrevelant`
```
s: Data.Set.Computable.NonOrdered.set t -> x: t{~(Data.Set.Computable.NonOrdered.mem x s)}
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.remove s x == s) []
```

 - `Data.Set.Computable.NonOrdered.lemma_subset_add`
```

    a: Data.Set.Computable.NonOrdered.set t ->
    b: Data.Set.Computable.NonOrdered.set t {Data.Set.Computable.NonOrdered.subset a b} ->
    h: t{Data.Set.Computable.NonOrdered.no_dup (h :: a) /\ Data.Set.Computable.NonOrdered.mem h b}
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.subset (h :: a) b) []
```

 - `Data.Set.Computable.NonOrdered.lemma_subset_invert`
```

    a: Data.Set.Computable.NonOrdered.set t ->
    b: Data.Set.Computable.NonOrdered.set t ->
    h: t ->
    h':
      t
        { Data.Set.Computable.NonOrdered.no_dup (h' :: h :: a) /\
          Data.Set.Computable.NonOrdered.subset (h' :: h :: a) b }
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.subset (h :: h' :: a) b) []
```

 - `Data.Set.Computable.NonOrdered.lemma_subset_ref`
```
a: Data.Set.Computable.NonOrdered.set t
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.subset a a) []
```

 - `Data.Set.Computable.NonOrdered.lemma_subset_remove`
```

    a: Data.Set.Computable.NonOrdered.set t ->
    b: Data.Set.Computable.NonOrdered.set t ->
    h: t{Data.Set.Computable.NonOrdered.no_dup (h :: a)}
  -> Prims.Lemma Prims.unit
      b
      (Data.Set.Computable.NonOrdered.subset (h :: a) b ==>
        Data.Set.Computable.NonOrdered.subset a (Data.Set.Computable.NonOrdered.remove b h))
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_subset_remove'`
```

    a: Data.Set.Computable.NonOrdered.set t ->
    b: Data.Set.Computable.NonOrdered.set t ->
    h: t{Data.Set.Computable.NonOrdered.no_dup (h :: a) /\ Data.Set.Computable.NonOrdered.mem h b}
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.subset a (Data.Set.Computable.NonOrdered.remove b h) ==>
        Data.Set.Computable.NonOrdered.subset (h :: a) b)
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_subset_sym`
```
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.subset a b /\ Data.Set.Computable.NonOrdered.subset b a ==>
        Data.Set.Computable.NonOrdered.equal a b)
      []
```

 - `Data.Set.Computable.NonOrdered.lemma_subset_trans`
```

    a: Data.Set.Computable.NonOrdered.set t ->
    b: Data.Set.Computable.NonOrdered.set t {Data.Set.Computable.NonOrdered.subset a b} ->
    c: Data.Set.Computable.NonOrdered.set t {Data.Set.Computable.NonOrdered.subset b c}
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.subset a c) []
```

 - `Data.Set.Computable.NonOrdered.lemma_triv`
```
unit: _ -> Prims.Lemma Prims.unit (true == true <: Prims.Tot Type0) []
```

 - `Data.Set.Computable.NonOrdered.lemma_union'_no_dup`
```
s: Prims.list t {Data.Set.Computable.NonOrdered.no_dup s}
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.union' s [] == s) []
```

 - `Data.Set.Computable.NonOrdered.lemma_union_step`
```
s: Data.Set.Computable.NonOrdered.set t -> h: t{~(Data.Set.Computable.NonOrdered.mem h s)}
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.union' (h :: s) [] ==
        h :: Data.Set.Computable.NonOrdered.union' s [])
      []
```

 - `Data.Set.Computable.NonOrdered.list_to_set`
```
a: Prims.list t -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```

 - `Data.Set.Computable.NonOrdered.mem`
```
x: t -> a: Data.Set.Computable.NonOrdered.set t -> Prims.Tot Prims.bool
```

 - `Data.Set.Computable.NonOrdered.no_dup`
```
a: Prims.list t -> Prims.Tot Prims.logical
```

 - `Data.Set.Computable.NonOrdered.remove`
```
a: Data.Set.Computable.NonOrdered.set t -> x: t -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```

 - `Data.Set.Computable.NonOrdered.remove_head`
```
x: t -> a: Data.Set.Computable.NonOrdered.set t {~(Data.Set.Computable.NonOrdered.mem x a)}
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.remove (x :: a) x == a) []
```

 - `Data.Set.Computable.NonOrdered.remove_lemma`
```

    s: Data.Set.Computable.NonOrdered.set t ->
    x: t{Data.Set.Computable.NonOrdered.mem x s} ->
    h: t{~(Data.Set.Computable.NonOrdered.mem h s) /\ ~(x = h)}
  -> Prims.Lemma Prims.unit
      (h :: Data.Set.Computable.NonOrdered.remove s x ==
        Data.Set.Computable.NonOrdered.remove (h :: s) x)
      []
```

 - `Data.Set.Computable.NonOrdered.remove_mem_dec`
```
x: t -> a: Data.Set.Computable.NonOrdered.set t {Data.Set.Computable.NonOrdered.mem x a}
  -> Prims.Lemma Prims.unit
      (FStar.List.Tot.Base.length (Data.Set.Computable.NonOrdered.remove a x) =
        FStar.List.Tot.Base.length a - 1)
      []
```

 - `Data.Set.Computable.NonOrdered.set`
```
a: Prims.eqtype -> Prims.Tot Type0
```

 - `Data.Set.Computable.NonOrdered.set_to_list`
```
a: Data.Set.Computable.NonOrdered.set t -> Prims.Tot (Prims.list t)
```

 - `Data.Set.Computable.NonOrdered.singleton`
```
a: t -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```

 - `Data.Set.Computable.NonOrdered.stupid_mem`
```
s: Prims.list t -> h: t -> x: t{FStar.List.Tot.Base.mem x (h :: s) /\ x <> h}
  -> Prims.Lemma Prims.unit (FStar.List.Tot.Base.mem x s) []
```

 - `Data.Set.Computable.NonOrdered.stupid_mem2`
```
l: Data.Set.Computable.NonOrdered.set t -> x: t{~(FStar.List.Tot.Base.mem x l)} -> y: t
  -> Prims.Lemma Prims.unit
      (~(FStar.List.Tot.Base.mem x (Data.Set.Computable.NonOrdered.remove l y)))
      []
```

 - `Data.Set.Computable.NonOrdered.subset`
```
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t
  -> Prims.Tot Prims.bool
```

 - `Data.Set.Computable.NonOrdered.union`
```
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t
  -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```

 - `Data.Set.Computable.NonOrdered.union'`
```
a: Prims.list t -> b: Data.Set.Computable.NonOrdered.set t
  -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```

# DefaultValue
 - `DefaultValue.boolHasDefaultValue`
```
DefaultValue.hasDefaultValue Prims.bool
```

 - `DefaultValue.def`
```
(#[FStar.Tactics.Typeclasses.tcresolve ()] d: DefaultValue.hasDefaultValue a) -> Prims.Tot a
```

 - `DefaultValue.hasDefaultValue`
```
a: Type -> Prims.Tot Type
```

 - `DefaultValue.intHasDefaultValue`
```
DefaultValue.hasDefaultValue Prims.int
```

 - `DefaultValue.listHasDefaultValue`
```
Prims.Tot (DefaultValue.hasDefaultValue (Prims.list a))
```

 - `DefaultValue.optionHasDefaultValue`
```
(#[FStar.Tactics.Typeclasses.tcresolve ()] _: DefaultValue.hasDefaultValue a)
  -> Prims.Tot (DefaultValue.hasDefaultValue (FStar.Pervasives.Native.option a))
```

 - `DefaultValue.stringHasDefaultValue`
```
DefaultValue.hasDefaultValue Prims.string
```

# MkDoc
 - `MkDoc.compute_term`
```
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```

 - `MkDoc.doc`
```
Type0
```

 - `MkDoc.export_types_to_file`
```
matchingAttr: FStar.Pervasives.Native.option FStar.Reflection.Types.term -> filepath: Prims.string
  -> FStar.Tactics.Effect.Tac Prims.unit
```

 - `MkDoc.getTypes_asJSON`
```
matchingAttr: FStar.Pervasives.Native.option FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac Data.JSON.Types.jsonValue
```

 - `MkDoc.liststring_of_term`
```
t: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac (Prims.list (Prims.string * Prims.string))
```

 - `MkDoc.show_def_in_doc`
```
Prims.unit
```

 - `MkDoc.string_of_term`
```
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.TAC Prims.string
```

 - `MkDoc.term_eq`
```
x: FStar.Reflection.Types.term -> y: FStar.Reflection.Types.term -> Prims.Tot Prims.bool
```

 - `MkDoc.x`
```
hd: _ -> tl: Prims.list _ -> Prims.Tot (Prims.list _)
```

# PartialOrder
 - `PartialOrder.derivedLPO`
```
someInstance: PartialOrder.hasPartialOrder 'a -> Prims.Tot (PartialOrder.hasLPartialOrder 'a)
```

 - `PartialOrder.hasLPartialOrder`
```
a: Type -> Prims.Tot Type
```

 - `PartialOrder.hasPartialOrder`
```
a: Type -> Prims.Tot Type
```

 - `PartialOrder.isMonotonic`
```

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: PartialOrder.hasLPartialOrder a) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: PartialOrder.hasLPartialOrder b) ->
    f: (_: a -> Prims.Tot b)
  -> Prims.Tot Prims.logical
```

 - `PartialOrder.isPartialOrder`
```
f: (_: a -> _: a -> Prims.Tot Prims.bool) -> Prims.Tot Prims.logical
```

 - `PartialOrder.isPartialOrderL`
```
cmp: (_: a -> _: a -> Prims.Tot Type0) -> f: (_: a -> _: a -> Prims.Tot Type0)
  -> Prims.Tot Prims.logical
```

 - `PartialOrder.l_po`
```
(#[FStar.Tactics.Typeclasses.tcresolve ()] d: PartialOrder.hasLPartialOrder a)
  -> Prims.Tot
    (f:
      (_: a -> _: a -> Prims.Tot Type0)
        {PartialOrder.isPartialOrderL (MkhasLPartialOrder?.l_po_cmp d) f})
```

 - `PartialOrder.l_po_cmp`
```
(#[FStar.Tactics.Typeclasses.tcresolve ()] d: PartialOrder.hasLPartialOrder a)
  -> Prims.Tot (_: a -> _: a -> Prims.Tot Type0)
```

 - `PartialOrder.po`
```
(#[FStar.Tactics.Typeclasses.tcresolve ()] d: PartialOrder.hasPartialOrder a)
  -> Prims.Tot (f: (_: a -> _: a -> Prims.Tot Prims.bool){PartialOrder.isPartialOrder f})
```

# ToString
 - `ToString.anyListHasToString`
```
(#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString a)
  -> Prims.Tot (ToString.hasToString (Prims.list a))
```

 - `ToString.boolHasToString`
```
ToString.hasToString Prims.bool
```

 - `ToString.charHasToString`
```
ToString.hasToString FStar.String.char
```

 - `ToString.eitherHasToString`
```

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString a) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString b)
  -> Prims.Tot (ToString.hasToString (FStar.Pervasives.either a b))
```

 - `ToString.hasToString`
```
t: Type -> Prims.Tot Type
```

 - `ToString.intHasToString`
```
ToString.hasToString Prims.int
```

 - `ToString.join`
```
sep: Prims.string -> l: Prims.list Prims.string -> Prims.Tot Prims.string
```

 - `ToString.natHasToString`
```
ToString.hasToString Prims.nat
```

 - `ToString.nat_to_int`
```
i: Prims.nat -> Prims.Tot Prims.int
```

 - `ToString.op_Hat_Subtraction`
```

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString t0) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString t1) ->
    s0: t0 ->
    s1: t1
  -> Prims.Tot Prims.string
```

 - `ToString.optionHasToString`
```
(#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString a)
  -> Prims.Tot (ToString.hasToString (FStar.Pervasives.Native.option a))
```

 - `ToString.stringHasToString`
```
ToString.hasToString Prims.string
```

 - `ToString.toString`
```
(#[FStar.Tactics.Typeclasses.tcresolve ()] d: ToString.hasToString t)
  -> Prims.Tot (_: t -> Prims.Tot Prims.string)
```

 - `ToString.tupleHasToString`
```

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString t1) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString t2)
  -> Prims.Tot (ToString.hasToString (t1 * t2))
```

