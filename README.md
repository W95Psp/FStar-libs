Here is a centralized repo for some F* "libraries" I use. 

These are packed into sort of packages using https://github.com/W95Psp/fstar-nix-packer

# Data
## Data.Function
<details><summary><code>Data.Function.const</code></summary>
<p>
```fstar
x: 'a -> _: 'b -> Prims.Tot 'a
```
</p>
</details>


<details><summary><code>Data.Function.flip</code></summary>
<p>
```fstar
f: (_: 'a -> _: 'b -> Prims.Tot 'c) -> b: 'b -> a: 'a -> Prims.Tot 'c
```
</p>
</details>


<details><summary><code>Data.Function.on</code></summary>
<p>
```fstar
f: (_: 'a -> _: 'a -> Prims.Tot 'b) -> t: (_: 'c -> Prims.Tot 'a) -> v: 'c -> w: 'c -> Prims.Tot 'b
```
</p>
</details>


<details><summary><code>Data.Function.op_At_At</code></summary>
<p>
```fstar
g: (_: 'b -> Prims.Tot 'c) -> f: (_: 'a -> Prims.Tot 'b) -> a: 'a -> Prims.Tot 'c
```
</p>
</details>


<details><summary><code>Data.Function.op_Bar_Greater</code></summary>
<p>
```fstar
v: 'a -> f: (_: 'a -> Prims.Tot 'b) -> Prims.Tot 'b
```
</p>
</details>


<details><summary><code>Data.Function.op_Less_Bar</code></summary>
<p>
```fstar
f: (_: 'a -> Prims.Tot 'b) -> v: 'a -> Prims.Tot 'b
```
</p>
</details>


## Data.JSON
<details><summary><code>Data.JSON.Parser.convert</code></summary>
<p>
```fstar
c: Prims.list (n: Prims.nat{n <= 9}) -> Prims.Tot Prims.nat
```
</p>
</details>


<details><summary><code>Data.JSON.Parser.convert_digit</code></summary>
<p>
```fstar
c: FStar.Char.char -> Prims.Tot (n: Prims.nat{n <= 9})
```
</p>
</details>


<details><summary><code>Data.JSON.Parser.jsonCharParser</code></summary>
<p>
```fstar
StarCombinator.Core.parser FStar.String.char
```
</p>
</details>


<details><summary><code>Data.JSON.Parser.jsonStringParser</code></summary>
<p>
```fstar
StarCombinator.Core.parser Data.JSON.Types.jsonValue
```
</p>
</details>


<details><summary><code>Data.JSON.Parser.jsonStringParser'</code></summary>
<p>
```fstar
StarCombinator.Core.parser Prims.string
```
</p>
</details>


<details><summary><code>Data.JSON.Parser.match_list</code></summary>
<p>
```fstar

    l: FStar.String.char ->
    r: FStar.String.char ->
    s: StarCombinator.Core.parser _ ->
    i: StarCombinator.Core.parser _
  -> Prims.Tot (StarCombinator.Core.parser (Prims.list _))
```
</p>
</details>


<details><summary><code>Data.JSON.Parser.parseArray</code></summary>
<p>
```fstar
_: Prims.unit -> Prims.Tot (StarCombinator.Core.parser Data.JSON.Types.jsonValue)
```
</p>
</details>


<details><summary><code>Data.JSON.Parser.parseBool</code></summary>
<p>
```fstar
StarCombinator.Core.parser Data.JSON.Types.jsonValue
```
</p>
</details>


<details><summary><code>Data.JSON.Parser.parseDecimalNumber</code></summary>
<p>
```fstar
StarCombinator.Core.parser Data.JSON.Types.decimalNumber
```
</p>
</details>


<details><summary><code>Data.JSON.Parser.parseNull</code></summary>
<p>
```fstar
StarCombinator.Core.parser Data.JSON.Types.jsonValue
```
</p>
</details>


<details><summary><code>Data.JSON.Parser.parseNumber</code></summary>
<p>
```fstar
StarCombinator.Core.parser Data.JSON.Types.jsonValue
```
</p>
</details>


<details><summary><code>Data.JSON.Parser.parseObject</code></summary>
<p>
```fstar
_: Prims.unit -> Prims.Tot (StarCombinator.Core.parser Data.JSON.Types.jsonValue)
```
</p>
</details>


<details><summary><code>Data.JSON.Parser.parseValue</code></summary>
<p>
```fstar
_: Prims.unit -> Prims.Tot (StarCombinator.Core.parser Data.JSON.Types.jsonValue)
```
</p>
</details>


<details><summary><code>Data.JSON.Parser.parser</code></summary>
<p>
```fstar
source: Prims.string -> Prims.Tot (FStar.Pervasives.either Data.JSON.Types.jsonValue Prims.string)
```
</p>
</details>


<details><summary><code>Data.JSON.Parser.test</code></summary>
<p>
```fstar
FStar.Pervasives.either Data.JSON.Types.jsonValue Prims.string
```
</p>
</details>


<details><summary><code>Data.JSON.Stringify.escapeString</code></summary>
<p>
```fstar
s: Prims.string -> Prims.Tot Prims.string
```
</p>
</details>


<details><summary><code>Data.JSON.Stringify.joinstr</code></summary>
<p>
```fstar
_: Prims.list Prims.string -> Prims.Tot Prims.string
```
</p>
</details>


<details><summary><code>Data.JSON.Stringify.printDecimalNumber</code></summary>
<p>
```fstar
_: Data.JSON.Types.decimalNumber -> Prims.Tot Prims.string
```
</p>
</details>


<details><summary><code>Data.JSON.Stringify.printDecimalNumber_h</code></summary>
<p>
```fstar
digits: Prims.list FStar.String.char -> n: Prims.int -> exp: Prims.int -> Prims.Tot Prims.string
```
</p>
</details>


<details><summary><code>Data.JSON.Stringify.repeat</code></summary>
<p>
```fstar
n: Prims.nat -> s: Prims.string -> Prims.Tot Prims.string
```
</p>
</details>


<details><summary><code>Data.JSON.Stringify.stringify</code></summary>
<p>
```fstar
value: Data.JSON.Types.jsonValue -> spaces: Prims.string -> Prims.Tot Prims.string
```
</p>
</details>


<details><summary><code>Data.JSON.Stringify.stringify_helper</code></summary>
<p>
```fstar
jump: Prims.bool -> tab: Prims.string -> n: Prims.nat -> value: Data.JSON.Types.jsonValue
  -> Prims.Tot Prims.string
```
</p>
</details>


<details><summary><code>Data.JSON.Types.decimalNumber</code></summary>
<p>
```fstar
Type0
```
</p>
</details>


<details><summary><code>Data.JSON.Types.jsonValue</code></summary>
<p>
```fstar
Type0
```
</p>
</details>


<details><summary><code>Data.JSON.decimalNumber_hasSerialize</code></summary>
<p>
```fstar
Data.Serialize.Typeclasses.hasSerialize Data.JSON.Types.decimalNumber
```
</p>
</details>


<details><summary><code>Data.JSON.decimalNumber_serialize_decode</code></summary>
<p>
```fstar
x0: Data.Serialize.Types.serialized -> Prims.Tot Data.JSON.Types.decimalNumber
```
</p>
</details>


<details><summary><code>Data.JSON.decimalNumber_serialize_decode_chainable</code></summary>
<p>
```fstar
x1: Data.Serialize.Types.serialized
  -> Prims.Tot (Data.JSON.Types.decimalNumber * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.JSON.decimalNumber_serialize_encode</code></summary>
<p>
```fstar
x19: Data.JSON.Types.decimalNumber -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.JSON.decimalNumber_serialize_encode_chainable</code></summary>
<p>
```fstar
x21: Data.JSON.Types.decimalNumber -> x22: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.JSON.jsonValue</code></summary>
<p>
```fstar
Type0
```
</p>
</details>


<details><summary><code>Data.JSON.jsonValue_hasSerialize</code></summary>
<p>
```fstar
Data.Serialize.Typeclasses.hasSerialize Data.JSON.Types.jsonValue
```
</p>
</details>


<details><summary><code>Data.JSON.jsonValue_serialize_decode</code></summary>
<p>
```fstar
x0: Data.Serialize.Types.serialized -> Prims.Tot Data.JSON.Types.jsonValue
```
</p>
</details>


<details><summary><code>Data.JSON.jsonValue_serialize_decode_chainable</code></summary>
<p>
```fstar
x1: Data.Serialize.Types.serialized
  -> Prims.Tot (Data.JSON.Types.jsonValue * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.JSON.jsonValue_serialize_encode</code></summary>
<p>
```fstar
x30: Data.JSON.Types.jsonValue -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.JSON.jsonValue_serialize_encode_chainable</code></summary>
<p>
```fstar
x32: Data.JSON.Types.jsonValue -> x33: Data.Serialize.Types.serialized
  -> Prims.Tot
    (Prims.list FStar.Reflection.Types.name *
      (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
```
</p>
</details>


<details><summary><code>Data.JSON.nat_serialize_decode_chainable</code></summary>
<p>
```fstar
s: Data.Serialize.Types.serialized -> Prims.Tot (Prims.nat * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.JSON.nat_serialize_encode_chainable</code></summary>
<p>
```fstar

    _: Prims.nat ->
    _:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.JSON.parse</code></summary>
<p>
```fstar
source: Prims.string -> Prims.Tot (FStar.Pervasives.either Data.JSON.Types.jsonValue Prims.string)
```
</p>
</details>


<details><summary><code>Data.JSON.parse'</code></summary>
<p>
```fstar
source: Prims.string -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.JSON.stringify</code></summary>
<p>
```fstar
v: Data.JSON.Types.jsonValue -> Prims.Tot (spaces: Prims.string -> Prims.Tot Prims.string)
```
</p>
</details>


<details><summary><code>Data.JSON.stringify'</code></summary>
<p>
```fstar
v: Data.Serialize.serialized -> Prims.Tot (spaces: Prims.string -> Prims.Tot Prims.string)
```
</p>
</details>


<details><summary><code>Data.JSON.tuple2_hasSerialize</code></summary>
<p>
```fstar

    x28: Type ->
    x29: Type ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x30: Data.Serialize.Typeclasses.hasSerialize x28) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x31: Data.Serialize.Typeclasses.hasSerialize x29)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize (x28 * x29))
```
</p>
</details>


<details><summary><code>Data.JSON.tuple2_serialize_decode</code></summary>
<p>
```fstar

    x0: (_: _ -> Prims.Tot (_ * _)) ->
    x1: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x2: Data.Serialize.Types.serialized
  -> Prims.Tot (_ * _)
```
</p>
</details>


<details><summary><code>Data.JSON.tuple2_serialize_decode_chainable</code></summary>
<p>
```fstar

    x3: (_: _ -> Prims.Tot (_ * _)) ->
    x4: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x5: Data.Serialize.Types.serialized
  -> Prims.Tot ((_ * _) * _)
```
</p>
</details>


<details><summary><code>Data.JSON.tuple2_serialize_encode</code></summary>
<p>
```fstar

    x18: (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot _) ->
    x19:
      (_: _ -> _: _
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x20: (_ * _)
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.JSON.tuple2_serialize_encode_chainable</code></summary>
<p>
```fstar

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
</p>
</details>


## Data.Map
### Data.Map.Enumerable
#### Data.Map.Enumerable.NonOrdered
<details><summary><code>Data.Map.Enumerable.NonOrdered.emHasToString</code></summary>
<p>
```fstar
(#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString a)
  -> Prims.Tot (ToString.hasToString (Data.Map.Enumerable.NonOrdered.enumerableMap a))
```
</p>
</details>


<details><summary><code>Data.Map.Enumerable.NonOrdered.em_combine</code></summary>
<p>
```fstar

    m1: Data.Map.Enumerable.NonOrdered.enumerableMap 'a ->
    m2: Data.Map.Enumerable.NonOrdered.enumerableMap 'a ->
    f: (_: 'a -> _: 'a -> Prims.Tot 'a)
  -> Prims.Tot (Data.Map.Enumerable.NonOrdered.enumerableMap 'a)
```
</p>
</details>


<details><summary><code>Data.Map.Enumerable.NonOrdered.em_equal</code></summary>
<p>
```fstar

    myEq: (_: a -> _: a -> Prims.Tot Prims.bool) ->
    m1: Data.Map.Enumerable.NonOrdered.enumerableMap a ->
    m2: Data.Map.Enumerable.NonOrdered.enumerableMap a
  -> Prims.Tot Prims.bool
```
</p>
</details>


<details><summary><code>Data.Map.Enumerable.NonOrdered.em_get</code></summary>
<p>
```fstar
m: Data.Map.Enumerable.NonOrdered.enumerableMap 'a -> k: Prims.string -> Prims.Tot 'a
```
</p>
</details>


<details><summary><code>Data.Map.Enumerable.NonOrdered.em_set</code></summary>
<p>
```fstar
m: Data.Map.Enumerable.NonOrdered.enumerableMap 'a -> k: Prims.string -> v: 'a
  -> Prims.Tot (Data.Map.Enumerable.NonOrdered.enumerableMap 'a)
```
</p>
</details>


<details><summary><code>Data.Map.Enumerable.NonOrdered.enumerableMap</code></summary>
<p>
```fstar
a: Type -> Prims.Tot Type
```
</p>
</details>


<details><summary><code>Data.Map.Enumerable.NonOrdered.enumerableMap'S</code></summary>
<p>
```fstar
a: Type -> Prims.Tot Type
```
</p>
</details>


<details><summary><code>Data.Map.Enumerable.NonOrdered.enumerableMap'S'dec</code></summary>
<p>
```fstar

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: DefaultValue.hasDefaultValue a) ->
    m: Data.Map.Enumerable.NonOrdered.enumerableMap'S a
  -> Prims.Tot (Data.Map.Enumerable.NonOrdered.enumerableMap a)
```
</p>
</details>


<details><summary><code>Data.Map.Enumerable.NonOrdered.enumerableMap'S'enc</code></summary>
<p>
```fstar
m: Data.Map.Enumerable.NonOrdered.enumerableMap a
  -> Prims.Tot (Data.Map.Enumerable.NonOrdered.enumerableMap'S a)
```
</p>
</details>


<details><summary><code>Data.Map.Enumerable.NonOrdered.listToEnumerableSet</code></summary>
<p>
```fstar

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: DefaultValue.hasDefaultValue a) ->
    lst: Prims.list (Prims.string * a)
  -> Prims.Tot (Data.Map.Enumerable.NonOrdered.enumerableMap a)
```
</p>
</details>


<details><summary><code>Data.Map.Enumerable.NonOrdered.listToEnumerableSet_resolver</code></summary>
<p>
```fstar

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: DefaultValue.hasDefaultValue a) ->
    l: Prims.list (Prims.string * a) ->
    query: Prims.string
  -> Prims.Tot a
```
</p>
</details>


<details><summary><code>Data.Map.Enumerable.NonOrdered.state_to_em</code></summary>
<p>
```fstar
s: (_: Prims.string -> Prims.Tot _) -> Prims.Tot (Data.Map.Enumerable.NonOrdered.enumerableMap _)
```
</p>
</details>


## Data.Serialize
<details><summary><code>Data.Serialize.Decode.change_last</code></summary>
<p>
```fstar
f: (_: 'a -> Prims.Tot 'a) -> l: Prims.list 'a -> Prims.Tot (Prims.list 'a)
```
</p>
</details>


<details><summary><code>Data.Serialize.Decode.generateDecodeSerialize</code></summary>
<p>
```fstar
name: FStar.Reflection.Types.fv -> FStar.Tactics.Effect.Tac FStar.Reflection.Data.decls
```
</p>
</details>


<details><summary><code>Data.Serialize.Decode.generateDecodeSerialize_for_inductiveSumup</code></summary>
<p>
```fstar
s: Data.Serialize.Types.inductiveSumup -> FStar.Tactics.Effect.Tac FStar.Reflection.Data.decls
```
</p>
</details>


<details><summary><code>Data.Serialize.Decode.generateDecodeSerialize_term_for_argSumup</code></summary>
<p>
```fstar

    args_fun: Prims.list FStar.Reflection.Types.binder ->
    arg: Data.Serialize.Types.argSumup (FStar.List.Tot.Base.length args_fun)
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Decode.generateDecodeSerialize_term_for_consSumup</code></summary>
<p>
```fstar

    encoders: Prims.list FStar.Reflection.Types.binder {FStar.List.Tot.Base.length encoders = n} ->
    cons: Data.Serialize.Types.consSumup n ->
    serialized_inp: FStar.Reflection.Types.bv
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Decode.generateDecodeSerialize_term_for_inductiveSumup</code></summary>
<p>
```fstar
s: Data.Serialize.Types.inductiveSumup -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Decode.id_tac_term</code></summary>
<p>
```fstar
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Decode.transform_name_decode</code></summary>
<p>
```fstar
n: FStar.Reflection.Types.name -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.name
```
</p>
</details>


<details><summary><code>Data.Serialize.Decode.transform_name_decode'</code></summary>
<p>
```fstar
n: FStar.Reflection.Types.name -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.name
```
</p>
</details>


<details><summary><code>Data.Serialize.Encode.generateDecodeSerialize_term_for_argSumup</code></summary>
<p>
```fstar

    args_fun: Prims.list FStar.Reflection.Types.binder ->
    arg: Data.Serialize.Types.argSumup (FStar.List.Tot.Base.length args_fun)
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Encode.generateEncodeSerialize</code></summary>
<p>
```fstar
name: FStar.Reflection.Types.fv -> FStar.Tactics.Effect.Tac FStar.Reflection.Data.decls
```
</p>
</details>


<details><summary><code>Data.Serialize.Encode.generateEncodeSerialize_for_inductiveSumup</code></summary>
<p>
```fstar
s: Data.Serialize.Types.inductiveSumup -> FStar.Tactics.Effect.Tac FStar.Reflection.Data.decls
```
</p>
</details>


<details><summary><code>Data.Serialize.Encode.generateEncodeSerialize_term_for_inductiveSumup</code></summary>
<p>
```fstar
s: Data.Serialize.Types.inductiveSumup -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Encode.transform_name_encode</code></summary>
<p>
```fstar
n: FStar.Reflection.Types.name -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.name
```
</p>
</details>


<details><summary><code>Data.Serialize.Encode.transform_name_encode'</code></summary>
<p>
```fstar
n: FStar.Reflection.Types.name -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.name
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Serialized.appendBool</code></summary>
<p>
```fstar

    b: Prims.bool ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Serialized.appendInt</code></summary>
<p>
```fstar

    i: Prims.int ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Serialized.appendList</code></summary>
<p>
```fstar

    appender:
      (_: t -> _: Data.Serialize.Types.serialized -> Prims.Tot Data.Serialize.Types.serialized) ->
    v: Prims.list t ->
    s: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Serialized.appendName</code></summary>
<p>
```fstar

    n: FStar.Reflection.Types.name ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Serialized.appendString</code></summary>
<p>
```fstar

    s: Prims.string ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Serialized.compose</code></summary>
<p>
```fstar

    f: (_: Data.Serialize.Types.serialized -> Prims.Tot Data.Serialize.Types.serialized) ->
    g: (_: Data.Serialize.Types.serialized -> Prims.Tot Data.Serialize.Types.serialized)
  -> Prims.Tot (_: Data.Serialize.Types.serialized -> Prims.Tot Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Serialized.emptySerialized</code></summary>
<p>
```fstar
Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Serialized.readBool</code></summary>
<p>
```fstar
x: Data.Serialize.Types.serialized -> Prims.Tot (Prims.bool * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Serialized.readInt</code></summary>
<p>
```fstar
x: Data.Serialize.Types.serialized -> Prims.Tot (Prims.int * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Serialized.readList</code></summary>
<p>
```fstar

    f: (_: Data.Serialize.Types.serialized -> Prims.Tot (t * Data.Serialize.Types.serialized)) ->
    s: Data.Serialize.Types.serialized
  -> Prims.Tot (Prims.list t * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Serialized.readName</code></summary>
<p>
```fstar
x: Data.Serialize.Types.serialized
  -> Prims.Tot (FStar.Reflection.Types.name * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Serialized.readString</code></summary>
<p>
```fstar
x: Data.Serialize.Types.serialized -> Prims.Tot (Prims.string * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Test.call1_test</code></summary>
<p>
```fstar
Prims.int
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Test.call2_test</code></summary>
<p>
```fstar
Prims.int
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Test.mkLet_tup'_test</code></summary>
<p>
```fstar
Prims.int
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Test.mkMatchInductive_test_typ</code></summary>
<p>
```fstar
Type0
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Test.mkMatchInt_test</code></summary>
<p>
```fstar
Prims.int
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.Test.mkMatchInt_test2</code></summary>
<p>
```fstar
x0: Prims.int -> Prims.Tot Prims.int
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.add_admit</code></summary>
<p>
```fstar
body: FStar.Reflection.Types.term -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.add_admit_decr_lex</code></summary>
<p>
```fstar
v: FStar.Reflection.Types.term -> body: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.admitMe</code></summary>
<p>
```fstar
n: _ -> Prims.Tot _
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.admitTerm</code></summary>
<p>
```fstar
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.argvToBinder</code></summary>
<p>
```fstar
x: FStar.Reflection.Data.argv -> FStar.Tactics.Effect.TAC FStar.Reflection.Types.binder
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.binderName</code></summary>
<p>
```fstar
n: Prims.string -> Prims.Tot FStar.Reflection.Types.binder
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.binderNth</code></summary>
<p>
```fstar
n: Prims.int -> Prims.Tot FStar.Reflection.Types.binder
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.binderToArgv</code></summary>
<p>
```fstar
b: FStar.Reflection.Types.binder
  -> FStar.Tactics.Effect.TAC (FStar.Reflection.Types.term * FStar.Reflection.Data.aqualv)
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.bvName</code></summary>
<p>
```fstar
n: Prims.string -> Prims.Tot FStar.Reflection.Types.bv
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.bvNth</code></summary>
<p>
```fstar
n: Prims.int -> Prims.Tot FStar.Reflection.Types.bv
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.call1</code></summary>
<p>
```fstar
f: FStar.Reflection.Types.term -> arg: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.call2</code></summary>
<p>
```fstar

    f: FStar.Reflection.Types.term ->
    arg1: FStar.Reflection.Types.term ->
    arg2: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.call3</code></summary>
<p>
```fstar

    f: FStar.Reflection.Types.term ->
    arg1: FStar.Reflection.Types.term ->
    arg2: FStar.Reflection.Types.term ->
    arg3: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.findIndex</code></summary>
<p>
```fstar
x: FStar.Reflection.Types.bv -> l: Prims.list FStar.Reflection.Types.bv
  -> FStar.Tactics.Effect.Tac Prims.nat
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.findIndex'</code></summary>
<p>
```fstar
x: FStar.Reflection.Types.bv -> n: Prims.nat -> l: Prims.list FStar.Reflection.Types.bv
  -> FStar.Tactics.Effect.Tac Prims.nat
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.fvOf</code></summary>
<p>
```fstar
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.TAC FStar.Reflection.Types.fv
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.last</code></summary>
<p>
```fstar
l: Prims.list 'a -> FStar.Tactics.Effect.Tac 'a
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.lex</code></summary>
<p>
```fstar
l: FStar.Reflection.Types.term -> r: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.TAC FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.lookup_typ'</code></summary>
<p>
```fstar
env: FStar.Reflection.Types.env -> name: FStar.Reflection.Types.name
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.sigelt
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.makeEitherType</code></summary>
<p>
```fstar
types: Prims.list FStar.Reflection.Types.typ -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.typ
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.makeOptionType</code></summary>
<p>
```fstar
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.typ
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.makeTupleType</code></summary>
<p>
```fstar
types: Prims.list FStar.Reflection.Types.typ -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.typ
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.mkLet_tup</code></summary>
<p>
```fstar

    def: FStar.Reflection.Types.term ->
    body:
      (_: FStar.Reflection.Types.bv -> _: FStar.Reflection.Types.bv
          -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term)
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.mkLet_tup'</code></summary>
<p>
```fstar
def: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac
    ((_: FStar.Reflection.Types.term -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term) *
      (FStar.Reflection.Types.bv * FStar.Reflection.Types.bv))
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.mkList</code></summary>
<p>
```fstar
min: Prims.int -> max: Prims.int -> Prims.Tot (Prims.list Prims.int)
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.mkMatchInductive</code></summary>
<p>
```fstar

    s: Data.Serialize.Types.inductiveSumup ->
    head: FStar.Reflection.Types.term ->
    bodies:
      Prims.list (_: Prims.list FStar.Reflection.Types.bv
            -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term)
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.mkMatchInt</code></summary>
<p>
```fstar
n: FStar.Reflection.Types.term -> bodies: Prims.list FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.mkTupleType</code></summary>
<p>
```fstar
a: Type -> b: Type -> Prims.Tot Type
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.mkTupleTypeTac</code></summary>
<p>
```fstar
a: FStar.Reflection.Types.typ -> b: FStar.Reflection.Types.typ
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.typ
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.mk_abs</code></summary>
<p>
```fstar
bs: Prims.list FStar.Reflection.Types.binder -> body: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.mkerror</code></summary>
<p>
```fstar
_: Prims.string -> Prims.Tot t
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.nameCurMod'</code></summary>
<p>
```fstar
n: FStar.Reflection.Types.name -> f: (_: Prims.string -> Prims.Tot Prims.string)
  -> FStar.Tactics.Effect.TAC (Prims.list Prims.string)
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.name_to_term</code></summary>
<p>
```fstar
n: FStar.Reflection.Types.name -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.norm_term'</code></summary>
<p>
```fstar
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.withIndex</code></summary>
<p>
```fstar
l: Prims.list 'a -> Prims.Tot (Prims.list (Prims.int * 'a))
```
</p>
</details>


<details><summary><code>Data.Serialize.Helpers.withIndex_helper</code></summary>
<p>
```fstar
l: Prims.list 'a -> n: Prims.int -> Prims.Tot (Prims.list (Prims.int * 'a))
```
</p>
</details>


<details><summary><code>Data.Serialize.Rep.makeGenericRep</code></summary>
<p>
```fstar
name: FStar.Reflection.Types.fv -> FStar.Tactics.Effect.Tac Data.Serialize.Types.inductiveSumup
```
</p>
</details>


<details><summary><code>Data.Serialize.Rep.makeGenericRep'Cons</code></summary>
<p>
```fstar
iVars: Prims.nat -> name: FStar.Reflection.Types.name
  -> FStar.Tactics.Effect.Tac (Data.Serialize.Types.consSumup iVars)
```
</p>
</details>


<details><summary><code>Data.Serialize.Rep.makeGenericRep'Cons'Arg</code></summary>
<p>
```fstar
iVars: Prims.nat -> bvs: Prims.list FStar.Reflection.Types.bv -> t: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac (Data.Serialize.Types.argSumup iVars)
```
</p>
</details>


<details><summary><code>Data.Serialize.Typeclasses.deserialize</code></summary>
<p>
```fstar

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: Data.Serialize.Typeclasses.hasSerialize a) ->
    v: Data.Serialize.Types.serialized
  -> Prims.Tot a
```
</p>
</details>


<details><summary><code>Data.Serialize.Typeclasses.deserialize_chainable</code></summary>
<p>
```fstar
(#[FStar.Tactics.Typeclasses.tcresolve ()] d: Data.Serialize.Typeclasses.hasSerialize a)
  -> Prims.Tot
    (_: Data.Serialize.Types.serialized -> Prims.Tot (a * Data.Serialize.Types.serialized))
```
</p>
</details>


<details><summary><code>Data.Serialize.Typeclasses.generateSerialize</code></summary>
<p>
```fstar
t: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.TAC (Prims.list FStar.Reflection.Types.sigelt)
```
</p>
</details>


<details><summary><code>Data.Serialize.Typeclasses.generateSerialize'</code></summary>
<p>
```fstar
tfv: FStar.Reflection.Types.fv
  -> FStar.Tactics.Effect.TAC (Prims.list FStar.Reflection.Types.sigelt)
```
</p>
</details>


<details><summary><code>Data.Serialize.Typeclasses.hasSerialize</code></summary>
<p>
```fstar
a: Type -> Prims.Tot Type
```
</p>
</details>


<details><summary><code>Data.Serialize.Typeclasses.intHasSerialize</code></summary>
<p>
```fstar
Data.Serialize.Typeclasses.hasSerialize Prims.int
```
</p>
</details>


<details><summary><code>Data.Serialize.Typeclasses.listIntHasSerialize</code></summary>
<p>
```fstar
Prims.Tot (Data.Serialize.Typeclasses.hasSerialize (Prims.list a))
```
</p>
</details>


<details><summary><code>Data.Serialize.Typeclasses.makeHasSerializeInstance</code></summary>
<p>
```fstar

    s: Data.Serialize.Types.inductiveSumup ->
    encode: FStar.Reflection.Types.fv ->
    decode: FStar.Reflection.Types.fv
  -> FStar.Tactics.Effect.Tac FStar.Reflection.Data.decls
```
</p>
</details>


<details><summary><code>Data.Serialize.Typeclasses.serialize</code></summary>
<p>
```fstar
(#[FStar.Tactics.Typeclasses.tcresolve ()] _: Data.Serialize.Typeclasses.hasSerialize a) -> v: a
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.Typeclasses.serialize_chainable</code></summary>
<p>
```fstar
(#[FStar.Tactics.Typeclasses.tcresolve ()] d: Data.Serialize.Typeclasses.hasSerialize a)
  -> Prims.Tot
    (_: a -> _: Data.Serialize.Types.serialized -> Prims.Tot Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.Serialize.Typeclasses.xx</code></summary>
<p>
```fstar
_: Data.Serialize.Types.serialized -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.Types.argSumup</code></summary>
<p>
```fstar
args: Prims.nat -> Prims.Tot Type0
```
</p>
</details>


<details><summary><code>Data.Serialize.Types.consSumup</code></summary>
<p>
```fstar
iVars: Prims.nat -> Prims.Tot Type0
```
</p>
</details>


<details><summary><code>Data.Serialize.Types.inductiveSumup</code></summary>
<p>
```fstar
Type0
```
</p>
</details>


<details><summary><code>Data.Serialize.Types.serialize_size</code></summary>
<p>
```fstar
_: Data.Serialize.Types.serialized -> Prims.Tot Prims.nat
```
</p>
</details>


<details><summary><code>Data.Serialize.Types.serialized</code></summary>
<p>
```fstar
Type0
```
</p>
</details>


<details><summary><code>Data.Serialize.Types.tserialized</code></summary>
<p>
```fstar
a: _ -> Prims.Tot Type0
```
</p>
</details>


<details><summary><code>Data.Serialize.boolHasSerialize</code></summary>
<p>
```fstar
Data.Serialize.Typeclasses.hasSerialize Prims.bool
```
</p>
</details>


<details><summary><code>Data.Serialize.bool_serialize_decode_chainable</code></summary>
<p>
```fstar
x: Data.Serialize.Types.serialized -> Prims.Tot (Prims.bool * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.Serialize.bool_serialize_encode_chainable</code></summary>
<p>
```fstar

    b: Prims.bool ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.deserialize</code></summary>
<p>
```fstar

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: Data.Serialize.hasSerialize a) ->
    v: Data.Serialize.Types.serialized
  -> Prims.Tot a
```
</p>
</details>


<details><summary><code>Data.Serialize.either_hasSerialize</code></summary>
<p>
```fstar

    x28: Type ->
    x29: Type ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x30: Data.Serialize.Typeclasses.hasSerialize x28) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x31: Data.Serialize.Typeclasses.hasSerialize x29)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize (FStar.Pervasives.either x28 x29))
```
</p>
</details>


<details><summary><code>Data.Serialize.either_serialize_decode</code></summary>
<p>
```fstar

    x0: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x1: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x2: Data.Serialize.Types.serialized
  -> Prims.Tot (FStar.Pervasives.either _ _)
```
</p>
</details>


<details><summary><code>Data.Serialize.either_serialize_decode_chainable</code></summary>
<p>
```fstar

    x3: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x4: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x5: Data.Serialize.Types.serialized
  -> Prims.Tot (FStar.Pervasives.either _ _ * _)
```
</p>
</details>


<details><summary><code>Data.Serialize.either_serialize_encode</code></summary>
<p>
```fstar

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
</p>
</details>


<details><summary><code>Data.Serialize.either_serialize_encode_chainable</code></summary>
<p>
```fstar

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
</p>
</details>


<details><summary><code>Data.Serialize.fvOf</code></summary>
<p>
```fstar
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.TAC FStar.Reflection.Types.fv
```
</p>
</details>


<details><summary><code>Data.Serialize.generateDecodeSerialize</code></summary>
<p>
```fstar
name: FStar.Reflection.Types.fv -> FStar.Tactics.Effect.Tac FStar.Reflection.Data.decls
```
</p>
</details>


<details><summary><code>Data.Serialize.generateEncodeSerialize</code></summary>
<p>
```fstar
name: FStar.Reflection.Types.fv -> FStar.Tactics.Effect.Tac FStar.Reflection.Data.decls
```
</p>
</details>


<details><summary><code>Data.Serialize.generateSerialize</code></summary>
<p>
```fstar
t: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.TAC (Prims.list FStar.Reflection.Types.sigelt)
```
</p>
</details>


<details><summary><code>Data.Serialize.generateSerialize'</code></summary>
<p>
```fstar
tfv: FStar.Reflection.Types.fv
  -> FStar.Tactics.Effect.TAC (Prims.list FStar.Reflection.Types.sigelt)
```
</p>
</details>


<details><summary><code>Data.Serialize.hasSerialize</code></summary>
<p>
```fstar
a: Type -> Prims.Tot Type
```
</p>
</details>


<details><summary><code>Data.Serialize.intHasSerialize</code></summary>
<p>
```fstar
Data.Serialize.Typeclasses.hasSerialize Prims.int
```
</p>
</details>


<details><summary><code>Data.Serialize.int_serialize_decode_chainable</code></summary>
<p>
```fstar
x: Data.Serialize.Types.serialized -> Prims.Tot (Prims.int * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.Serialize.int_serialize_encode_chainable</code></summary>
<p>
```fstar

    i: Prims.int ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.listHasSerialize</code></summary>
<p>
```fstar
a: Type -> (#[FStar.Tactics.Typeclasses.tcresolve ()] _: Data.Serialize.Typeclasses.hasSerialize a)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize (Prims.list a))
```
</p>
</details>


<details><summary><code>Data.Serialize.list_serialize_decode_chainable</code></summary>
<p>
```fstar

    f: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * Data.Serialize.Types.serialized)) ->
    s: Data.Serialize.Types.serialized
  -> Prims.Tot (Prims.list _ * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.Serialize.list_serialize_encode_chainable</code></summary>
<p>
```fstar

    appender:
      (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot Data.Serialize.Types.serialized) ->
    v: Prims.list _ ->
    s: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.nameHasSerialize</code></summary>
<p>
```fstar
Data.Serialize.Typeclasses.hasSerialize FStar.Reflection.Types.name
```
</p>
</details>


<details><summary><code>Data.Serialize.name_serialize_decode_chainable</code></summary>
<p>
```fstar
x: Data.Serialize.Types.serialized
  -> Prims.Tot (FStar.Reflection.Types.name * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.Serialize.name_serialize_encode_chainable</code></summary>
<p>
```fstar

    n: FStar.Reflection.Types.name ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.nat_serialize_decode_chainable</code></summary>
<p>
```fstar
s: Data.Serialize.Types.serialized -> Prims.Tot (Prims.nat * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.Serialize.nat_serialize_encode_chainable</code></summary>
<p>
```fstar

    _: Prims.nat ->
    _:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.option_hasSerialize</code></summary>
<p>
```fstar

    x19: Type ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x20: Data.Serialize.Typeclasses.hasSerialize x19)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize (FStar.Pervasives.Native.option x19))
```
</p>
</details>


<details><summary><code>Data.Serialize.option_serialize_decode</code></summary>
<p>
```fstar

    x0: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * Data.Serialize.Types.serialized)) ->
    x1: Data.Serialize.Types.serialized
  -> Prims.Tot (FStar.Pervasives.Native.option _)
```
</p>
</details>


<details><summary><code>Data.Serialize.option_serialize_decode_chainable</code></summary>
<p>
```fstar

    x2: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * Data.Serialize.Types.serialized)) ->
    x3: Data.Serialize.Types.serialized
  -> Prims.Tot (FStar.Pervasives.Native.option _ * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.Serialize.option_serialize_encode</code></summary>
<p>
```fstar

    x12:
      (_: _ -> _: Data.Serialize.Types.serialized
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x13: FStar.Pervasives.Native.option _
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.option_serialize_encode_chainable</code></summary>
<p>
```fstar

    x15:
      (_: _ -> _: Data.Serialize.Types.serialized
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x16: FStar.Pervasives.Native.option _ ->
    x17: Data.Serialize.Types.serialized
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.serialize</code></summary>
<p>
```fstar
(#[FStar.Tactics.Typeclasses.tcresolve ()] _: Data.Serialize.hasSerialize a) -> v: a
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.serialized</code></summary>
<p>
```fstar
Type0
```
</p>
</details>


<details><summary><code>Data.Serialize.stringHasSerialize</code></summary>
<p>
```fstar
Data.Serialize.Typeclasses.hasSerialize Prims.string
```
</p>
</details>


<details><summary><code>Data.Serialize.string_serialize_decode_chainable</code></summary>
<p>
```fstar
x: Data.Serialize.Types.serialized -> Prims.Tot (Prims.string * Data.Serialize.Types.serialized)
```
</p>
</details>


<details><summary><code>Data.Serialize.string_serialize_encode_chainable</code></summary>
<p>
```fstar

    s: Prims.string ->
    x:
      (Prims.list FStar.Reflection.Types.name *
        (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.tserialized</code></summary>
<p>
```fstar
a: _ -> Prims.Tot Type0
```
</p>
</details>


<details><summary><code>Data.Serialize.tuple2_hasSerialize</code></summary>
<p>
```fstar

    x28: Type ->
    x29: Type ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x30: Data.Serialize.Typeclasses.hasSerialize x28) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x31: Data.Serialize.Typeclasses.hasSerialize x29)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize (x28 * x29))
```
</p>
</details>


<details><summary><code>Data.Serialize.tuple2_serialize_decode</code></summary>
<p>
```fstar

    x0: (_: _ -> Prims.Tot (_ * _)) ->
    x1: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x2: Data.Serialize.Types.serialized
  -> Prims.Tot (_ * _)
```
</p>
</details>


<details><summary><code>Data.Serialize.tuple2_serialize_decode_chainable</code></summary>
<p>
```fstar

    x3: (_: _ -> Prims.Tot (_ * _)) ->
    x4: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x5: Data.Serialize.Types.serialized
  -> Prims.Tot ((_ * _) * _)
```
</p>
</details>


<details><summary><code>Data.Serialize.tuple2_serialize_encode</code></summary>
<p>
```fstar

    x18: (_: _ -> _: Data.Serialize.Types.serialized -> Prims.Tot _) ->
    x19:
      (_: _ -> _: _
          -> Prims.Tot
            (Prims.list FStar.Reflection.Types.name *
              (Prims.list Prims.int * (Prims.list Prims.string * Prims.list Prims.bool)))) ->
    x20: (_ * _)
  -> Prims.Tot Data.Serialize.Types.serialized
```
</p>
</details>


<details><summary><code>Data.Serialize.tuple2_serialize_encode_chainable</code></summary>
<p>
```fstar

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
</p>
</details>


<details><summary><code>Data.Serialize.tuple3_hasSerialize</code></summary>
<p>
```fstar

    x37: Type ->
    x38: Type ->
    x39: Type ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x40: Data.Serialize.Typeclasses.hasSerialize x37) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x41: Data.Serialize.Typeclasses.hasSerialize x38) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] x42: Data.Serialize.Typeclasses.hasSerialize x39)
  -> Prims.Tot (Data.Serialize.Typeclasses.hasSerialize ((x37 * x38) * x39))
```
</p>
</details>


<details><summary><code>Data.Serialize.tuple3_serialize_decode</code></summary>
<p>
```fstar

    x0: (_: _ -> Prims.Tot (_ * _)) ->
    x1: (_: _ -> Prims.Tot (_ * _)) ->
    x2: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x3: Data.Serialize.Types.serialized
  -> Prims.Tot ((_ * _) * _)
```
</p>
</details>


<details><summary><code>Data.Serialize.tuple3_serialize_decode_chainable</code></summary>
<p>
```fstar

    x4: (_: _ -> Prims.Tot (_ * _)) ->
    x5: (_: _ -> Prims.Tot (_ * _)) ->
    x6: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x7: Data.Serialize.Types.serialized
  -> Prims.Tot (((_ * _) * _) * _)
```
</p>
</details>


<details><summary><code>Data.Serialize.tuple3_serialize_encode</code></summary>
<p>
```fstar

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
</p>
</details>


<details><summary><code>Data.Serialize.tuple3_serialize_encode_chainable</code></summary>
<p>
```fstar

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
</p>
</details>


<details><summary><code>Data.Serialize.tuple4_hasSerialize</code></summary>
<p>
```fstar

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
</p>
</details>


<details><summary><code>Data.Serialize.tuple4_serialize_decode</code></summary>
<p>
```fstar

    x0: (_: _ -> Prims.Tot (_ * _)) ->
    x1: (_: _ -> Prims.Tot (_ * _)) ->
    x2: (_: _ -> Prims.Tot (_ * _)) ->
    x3: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x4: Data.Serialize.Types.serialized
  -> Prims.Tot (((_ * _) * _) * _)
```
</p>
</details>


<details><summary><code>Data.Serialize.tuple4_serialize_decode_chainable</code></summary>
<p>
```fstar

    x5: (_: _ -> Prims.Tot (_ * _)) ->
    x6: (_: _ -> Prims.Tot (_ * _)) ->
    x7: (_: _ -> Prims.Tot (_ * _)) ->
    x8: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x9: Data.Serialize.Types.serialized
  -> Prims.Tot ((((_ * _) * _) * _) * _)
```
</p>
</details>


<details><summary><code>Data.Serialize.tuple4_serialize_encode</code></summary>
<p>
```fstar

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
</p>
</details>


<details><summary><code>Data.Serialize.tuple4_serialize_encode_chainable</code></summary>
<p>
```fstar

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
</p>
</details>


<details><summary><code>Data.Serialize.tuple5_hasSerialize</code></summary>
<p>
```fstar

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
</p>
</details>


<details><summary><code>Data.Serialize.tuple5_serialize_decode</code></summary>
<p>
```fstar

    x0: (_: _ -> Prims.Tot (_ * _)) ->
    x1: (_: _ -> Prims.Tot (_ * _)) ->
    x2: (_: _ -> Prims.Tot (_ * _)) ->
    x3: (_: _ -> Prims.Tot (_ * _)) ->
    x4: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x5: Data.Serialize.Types.serialized
  -> Prims.Tot ((((_ * _) * _) * _) * _)
```
</p>
</details>


<details><summary><code>Data.Serialize.tuple5_serialize_decode_chainable</code></summary>
<p>
```fstar

    x6: (_: _ -> Prims.Tot (_ * _)) ->
    x7: (_: _ -> Prims.Tot (_ * _)) ->
    x8: (_: _ -> Prims.Tot (_ * _)) ->
    x9: (_: _ -> Prims.Tot (_ * _)) ->
    x10: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x11: Data.Serialize.Types.serialized
  -> Prims.Tot (((((_ * _) * _) * _) * _) * _)
```
</p>
</details>


<details><summary><code>Data.Serialize.tuple5_serialize_encode</code></summary>
<p>
```fstar

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
</p>
</details>


<details><summary><code>Data.Serialize.tuple5_serialize_encode_chainable</code></summary>
<p>
```fstar

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
</p>
</details>


<details><summary><code>Data.Serialize.tuple6_hasSerialize</code></summary>
<p>
```fstar

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
</p>
</details>


<details><summary><code>Data.Serialize.tuple6_serialize_decode</code></summary>
<p>
```fstar

    x0: (_: _ -> Prims.Tot (_ * _)) ->
    x1: (_: _ -> Prims.Tot (_ * _)) ->
    x2: (_: _ -> Prims.Tot (_ * _)) ->
    x3: (_: _ -> Prims.Tot (_ * _)) ->
    x4: (_: _ -> Prims.Tot (_ * _)) ->
    x5: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x6: Data.Serialize.Types.serialized
  -> Prims.Tot (((((_ * _) * _) * _) * _) * _)
```
</p>
</details>


<details><summary><code>Data.Serialize.tuple6_serialize_decode_chainable</code></summary>
<p>
```fstar

    x7: (_: _ -> Prims.Tot (_ * _)) ->
    x8: (_: _ -> Prims.Tot (_ * _)) ->
    x9: (_: _ -> Prims.Tot (_ * _)) ->
    x10: (_: _ -> Prims.Tot (_ * _)) ->
    x11: (_: _ -> Prims.Tot (_ * _)) ->
    x12: (_: Data.Serialize.Types.serialized -> Prims.Tot (_ * _)) ->
    x13: Data.Serialize.Types.serialized
  -> Prims.Tot ((((((_ * _) * _) * _) * _) * _) * _)
```
</p>
</details>


<details><summary><code>Data.Serialize.tuple6_serialize_encode</code></summary>
<p>
```fstar

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
</p>
</details>


<details><summary><code>Data.Serialize.tuple6_serialize_encode_chainable</code></summary>
<p>
```fstar

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
</p>
</details>


## Data.Set
### Data.Set.Computable
#### Data.Set.Computable.NonOrdered
<details><summary><code>Data.Set.Computable.NonOrdered.PartialOrder.csetLPO</code></summary>
<p>
```fstar
Prims.Tot (PartialOrder.hasLPartialOrder (Data.Set.Computable.NonOrdered.set c))
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.PartialOrder.gsetLPO</code></summary>
<p>
```fstar
Prims.Tot (PartialOrder.hasLPartialOrder (FStar.GSet.set c))
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.add_in_set</code></summary>
<p>
```fstar
x: t -> s: Data.Set.Computable.NonOrdered.set t -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.add_in_set'</code></summary>
<p>
```fstar
s: Prims.list t {Data.Set.Computable.NonOrdered.no_dup s} -> x: t
  -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.cset_to_set</code></summary>
<p>
```fstar
s: Data.Set.Computable.NonOrdered.set t -> Prims.Tot (FStar.GSet.set t)
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.dup_behead</code></summary>
<p>
```fstar
x: t -> a: Prims.list t {Data.Set.Computable.NonOrdered.no_dup (x :: a)}
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.no_dup a) []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.equal</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t
  -> Prims.Tot Type0
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.findAndRemove</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Tot (Prims.bool * Data.Set.Computable.NonOrdered.set t)
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.intersect</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t
  -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_convert_wf</code></summary>
<p>
```fstar
s: Data.Set.Computable.NonOrdered.set t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.equal (Data.Set.Computable.NonOrdered.list_to_set (Data.Set.Computable.NonOrdered.set_to_list 
                  s))
          s)
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_intermediaire</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> h: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.intersect a [h] == [h] \/
        Data.Set.Computable.NonOrdered.intersect a [h] == [])
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_intersect_cons_l</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> h: t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.no_dup (h :: a) ==>
        ~(h = x) ==>
        Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect (h :: a) b) ==
        Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b))
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_intersect_cons_r</code></summary>
<p>
```fstar

    a: Data.Set.Computable.NonOrdered.set t ->
    b: Data.Set.Computable.NonOrdered.set t ->
    h: t{Data.Set.Computable.NonOrdered.no_dup (h :: b)} ->
    x: t{~(h = x)}
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a (h :: b)) ==
        Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b))
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_intersect_empty_l</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect [] a) = false)
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_intersect_empty_r</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a []) = false)
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_intersect_left</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b) ==>
        Data.Set.Computable.NonOrdered.mem x a)
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_intersect_mem_both</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b) ==>
        Data.Set.Computable.NonOrdered.mem x a /\ Data.Set.Computable.NonOrdered.mem x b)
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_intersect_mem_equiv</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b) <==>
        Data.Set.Computable.NonOrdered.mem x a /\ Data.Set.Computable.NonOrdered.mem x b)
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_intersect_not_left</code></summary>
<p>
```fstar

    a: Data.Set.Computable.NonOrdered.set t ->
    b: Data.Set.Computable.NonOrdered.set t ->
    x: t{~(Data.Set.Computable.NonOrdered.mem x a)}
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b) == false)
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_intersect_not_right</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x b == false ==>
        Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b) == false
      )
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_memIntersect_memLeft</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b) ==>
        Data.Set.Computable.NonOrdered.mem x a)
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_memIntersect_memRight</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b) ==>
        Data.Set.Computable.NonOrdered.mem x b)
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_mem_both_intersect</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.mem x a /\ Data.Set.Computable.NonOrdered.mem x b ==>
        Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.intersect a b))
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_mem_empty</code></summary>
<p>
```fstar
x: t -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.mem x [] = false) []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_no_dup_remove</code></summary>
<p>
```fstar
l: Prims.list t {Data.Set.Computable.NonOrdered.no_dup l} -> x: t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.no_dup (Data.Set.Computable.NonOrdered.remove l x))
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_remove'</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> x: t -> y: t{x <> y}
  -> Prims.Lemma Prims.unit
      (FStar.List.Tot.Base.mem x a ==>
        Data.Set.Computable.NonOrdered.mem x (Data.Set.Computable.NonOrdered.remove a y))
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_remove_irrevelant</code></summary>
<p>
```fstar
s: Data.Set.Computable.NonOrdered.set t -> x: t{~(Data.Set.Computable.NonOrdered.mem x s)}
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.remove s x == s) []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_subset_add</code></summary>
<p>
```fstar

    a: Data.Set.Computable.NonOrdered.set t ->
    b: Data.Set.Computable.NonOrdered.set t {Data.Set.Computable.NonOrdered.subset a b} ->
    h: t{Data.Set.Computable.NonOrdered.no_dup (h :: a) /\ Data.Set.Computable.NonOrdered.mem h b}
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.subset (h :: a) b) []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_subset_invert</code></summary>
<p>
```fstar

    a: Data.Set.Computable.NonOrdered.set t ->
    b: Data.Set.Computable.NonOrdered.set t ->
    h: t ->
    h':
      t
        { Data.Set.Computable.NonOrdered.no_dup (h' :: h :: a) /\
          Data.Set.Computable.NonOrdered.subset (h' :: h :: a) b }
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.subset (h :: h' :: a) b) []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_subset_ref</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.subset a a) []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_subset_remove</code></summary>
<p>
```fstar

    a: Data.Set.Computable.NonOrdered.set t ->
    b: Data.Set.Computable.NonOrdered.set t ->
    h: t{Data.Set.Computable.NonOrdered.no_dup (h :: a)}
  -> Prims.Lemma Prims.unit
      b
      (Data.Set.Computable.NonOrdered.subset (h :: a) b ==>
        Data.Set.Computable.NonOrdered.subset a (Data.Set.Computable.NonOrdered.remove b h))
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_subset_remove'</code></summary>
<p>
```fstar

    a: Data.Set.Computable.NonOrdered.set t ->
    b: Data.Set.Computable.NonOrdered.set t ->
    h: t{Data.Set.Computable.NonOrdered.no_dup (h :: a) /\ Data.Set.Computable.NonOrdered.mem h b}
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.subset a (Data.Set.Computable.NonOrdered.remove b h) ==>
        Data.Set.Computable.NonOrdered.subset (h :: a) b)
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_subset_sym</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.subset a b /\ Data.Set.Computable.NonOrdered.subset b a ==>
        Data.Set.Computable.NonOrdered.equal a b)
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_subset_trans</code></summary>
<p>
```fstar

    a: Data.Set.Computable.NonOrdered.set t ->
    b: Data.Set.Computable.NonOrdered.set t {Data.Set.Computable.NonOrdered.subset a b} ->
    c: Data.Set.Computable.NonOrdered.set t {Data.Set.Computable.NonOrdered.subset b c}
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.subset a c) []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_triv</code></summary>
<p>
```fstar
unit: _ -> Prims.Lemma Prims.unit (true == true <: Prims.Tot Type0) []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_union'_no_dup</code></summary>
<p>
```fstar
s: Prims.list t {Data.Set.Computable.NonOrdered.no_dup s}
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.union' s [] == s) []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.lemma_union_step</code></summary>
<p>
```fstar
s: Data.Set.Computable.NonOrdered.set t -> h: t{~(Data.Set.Computable.NonOrdered.mem h s)}
  -> Prims.Lemma Prims.unit
      (Data.Set.Computable.NonOrdered.union' (h :: s) [] ==
        h :: Data.Set.Computable.NonOrdered.union' s [])
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.list_to_set</code></summary>
<p>
```fstar
a: Prims.list t -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.mem</code></summary>
<p>
```fstar
x: t -> a: Data.Set.Computable.NonOrdered.set t -> Prims.Tot Prims.bool
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.no_dup</code></summary>
<p>
```fstar
a: Prims.list t -> Prims.Tot Prims.logical
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.remove</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> x: t -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.remove_head</code></summary>
<p>
```fstar
x: t -> a: Data.Set.Computable.NonOrdered.set t {~(Data.Set.Computable.NonOrdered.mem x a)}
  -> Prims.Lemma Prims.unit (Data.Set.Computable.NonOrdered.remove (x :: a) x == a) []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.remove_lemma</code></summary>
<p>
```fstar

    s: Data.Set.Computable.NonOrdered.set t ->
    x: t{Data.Set.Computable.NonOrdered.mem x s} ->
    h: t{~(Data.Set.Computable.NonOrdered.mem h s) /\ ~(x = h)}
  -> Prims.Lemma Prims.unit
      (h :: Data.Set.Computable.NonOrdered.remove s x ==
        Data.Set.Computable.NonOrdered.remove (h :: s) x)
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.remove_mem_dec</code></summary>
<p>
```fstar
x: t -> a: Data.Set.Computable.NonOrdered.set t {Data.Set.Computable.NonOrdered.mem x a}
  -> Prims.Lemma Prims.unit
      (FStar.List.Tot.Base.length (Data.Set.Computable.NonOrdered.remove a x) =
        FStar.List.Tot.Base.length a - 1)
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.set</code></summary>
<p>
```fstar
a: Prims.eqtype -> Prims.Tot Type0
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.set_to_list</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> Prims.Tot (Prims.list t)
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.singleton</code></summary>
<p>
```fstar
a: t -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.stupid_mem</code></summary>
<p>
```fstar
s: Prims.list t -> h: t -> x: t{FStar.List.Tot.Base.mem x (h :: s) /\ x <> h}
  -> Prims.Lemma Prims.unit (FStar.List.Tot.Base.mem x s) []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.stupid_mem2</code></summary>
<p>
```fstar
l: Data.Set.Computable.NonOrdered.set t -> x: t{~(FStar.List.Tot.Base.mem x l)} -> y: t
  -> Prims.Lemma Prims.unit
      (~(FStar.List.Tot.Base.mem x (Data.Set.Computable.NonOrdered.remove l y)))
      []
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.subset</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t
  -> Prims.Tot Prims.bool
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.union</code></summary>
<p>
```fstar
a: Data.Set.Computable.NonOrdered.set t -> b: Data.Set.Computable.NonOrdered.set t
  -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```
</p>
</details>


<details><summary><code>Data.Set.Computable.NonOrdered.union'</code></summary>
<p>
```fstar
a: Prims.list t -> b: Data.Set.Computable.NonOrdered.set t
  -> Prims.Tot (Data.Set.Computable.NonOrdered.set t)
```
</p>
</details>


# DefaultValue
<details><summary><code>DefaultValue.boolHasDefaultValue</code></summary>
<p>
```fstar
DefaultValue.hasDefaultValue Prims.bool
```
</p>
</details>


<details><summary><code>DefaultValue.def</code></summary>
<p>
```fstar
(#[FStar.Tactics.Typeclasses.tcresolve ()] d: DefaultValue.hasDefaultValue a) -> Prims.Tot a
```
</p>
</details>


<details><summary><code>DefaultValue.hasDefaultValue</code></summary>
<p>
```fstar
a: Type -> Prims.Tot Type
```
</p>
</details>


<details><summary><code>DefaultValue.intHasDefaultValue</code></summary>
<p>
```fstar
DefaultValue.hasDefaultValue Prims.int
```
</p>
</details>


<details><summary><code>DefaultValue.listHasDefaultValue</code></summary>
<p>
```fstar
Prims.Tot (DefaultValue.hasDefaultValue (Prims.list a))
```
</p>
</details>


<details><summary><code>DefaultValue.optionHasDefaultValue</code></summary>
<p>
```fstar
(#[FStar.Tactics.Typeclasses.tcresolve ()] _: DefaultValue.hasDefaultValue a)
  -> Prims.Tot (DefaultValue.hasDefaultValue (FStar.Pervasives.Native.option a))
```
</p>
</details>


<details><summary><code>DefaultValue.stringHasDefaultValue</code></summary>
<p>
```fstar
DefaultValue.hasDefaultValue Prims.string
```
</p>
</details>


# MkDoc
<details><summary><code>MkDoc.compute_term</code></summary>
<p>
```fstar
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.Tac FStar.Reflection.Types.term
```
</p>
</details>


<details><summary><code>MkDoc.doc</code></summary>
<p>
```fstar
Type0
```
</p>
</details>


<details><summary><code>MkDoc.export_types_to_file</code></summary>
<p>
```fstar
matchingAttr: FStar.Pervasives.Native.option FStar.Reflection.Types.term -> filepath: Prims.string
  -> FStar.Tactics.Effect.Tac Prims.unit
```
</p>
</details>


<details><summary><code>MkDoc.getTypes_asJSON</code></summary>
<p>
```fstar
matchingAttr: FStar.Pervasives.Native.option FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac Data.JSON.Types.jsonValue
```
</p>
</details>


<details><summary><code>MkDoc.liststring_of_term</code></summary>
<p>
```fstar
t: FStar.Reflection.Types.term
  -> FStar.Tactics.Effect.Tac (Prims.list (Prims.string * Prims.string))
```
</p>
</details>


<details><summary><code>MkDoc.show_def_in_doc</code></summary>
<p>
```fstar
Prims.unit
```
</p>
</details>


<details><summary><code>MkDoc.string_of_term</code></summary>
<p>
```fstar
t: FStar.Reflection.Types.term -> FStar.Tactics.Effect.TAC Prims.string
```
</p>
</details>


<details><summary><code>MkDoc.term_eq</code></summary>
<p>
```fstar
x: FStar.Reflection.Types.term -> y: FStar.Reflection.Types.term -> Prims.Tot Prims.bool
```
</p>
</details>


<details><summary><code>MkDoc.x</code></summary>
<p>
```fstar
hd: _ -> tl: Prims.list _ -> Prims.Tot (Prims.list _)
```
</p>
</details>


# PartialOrder
<details><summary><code>PartialOrder.derivedLPO</code></summary>
<p>
```fstar
someInstance: PartialOrder.hasPartialOrder 'a -> Prims.Tot (PartialOrder.hasLPartialOrder 'a)
```
</p>
</details>


<details><summary><code>PartialOrder.hasLPartialOrder</code></summary>
<p>
```fstar
a: Type -> Prims.Tot Type
```
</p>
</details>


<details><summary><code>PartialOrder.hasPartialOrder</code></summary>
<p>
```fstar
a: Type -> Prims.Tot Type
```
</p>
</details>


<details><summary><code>PartialOrder.isMonotonic</code></summary>
<p>
```fstar

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: PartialOrder.hasLPartialOrder a) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: PartialOrder.hasLPartialOrder b) ->
    f: (_: a -> Prims.Tot b)
  -> Prims.Tot Prims.logical
```
</p>
</details>


<details><summary><code>PartialOrder.isPartialOrder</code></summary>
<p>
```fstar
f: (_: a -> _: a -> Prims.Tot Prims.bool) -> Prims.Tot Prims.logical
```
</p>
</details>


<details><summary><code>PartialOrder.isPartialOrderL</code></summary>
<p>
```fstar
cmp: (_: a -> _: a -> Prims.Tot Type0) -> f: (_: a -> _: a -> Prims.Tot Type0)
  -> Prims.Tot Prims.logical
```
</p>
</details>


<details><summary><code>PartialOrder.l_po</code></summary>
<p>
```fstar
(#[FStar.Tactics.Typeclasses.tcresolve ()] d: PartialOrder.hasLPartialOrder a)
  -> Prims.Tot
    (f:
      (_: a -> _: a -> Prims.Tot Type0)
        {PartialOrder.isPartialOrderL (MkhasLPartialOrder?.l_po_cmp d) f})
```
</p>
</details>


<details><summary><code>PartialOrder.l_po_cmp</code></summary>
<p>
```fstar
(#[FStar.Tactics.Typeclasses.tcresolve ()] d: PartialOrder.hasLPartialOrder a)
  -> Prims.Tot (_: a -> _: a -> Prims.Tot Type0)
```
</p>
</details>


<details><summary><code>PartialOrder.po</code></summary>
<p>
```fstar
(#[FStar.Tactics.Typeclasses.tcresolve ()] d: PartialOrder.hasPartialOrder a)
  -> Prims.Tot (f: (_: a -> _: a -> Prims.Tot Prims.bool){PartialOrder.isPartialOrder f})
```
</p>
</details>


# ToString
<details><summary><code>ToString.anyListHasToString</code></summary>
<p>
```fstar
(#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString a)
  -> Prims.Tot (ToString.hasToString (Prims.list a))
```
</p>
</details>


<details><summary><code>ToString.boolHasToString</code></summary>
<p>
```fstar
ToString.hasToString Prims.bool
```
</p>
</details>


<details><summary><code>ToString.charHasToString</code></summary>
<p>
```fstar
ToString.hasToString FStar.String.char
```
</p>
</details>


<details><summary><code>ToString.eitherHasToString</code></summary>
<p>
```fstar

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString a) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString b)
  -> Prims.Tot (ToString.hasToString (FStar.Pervasives.either a b))
```
</p>
</details>


<details><summary><code>ToString.hasToString</code></summary>
<p>
```fstar
t: Type -> Prims.Tot Type
```
</p>
</details>


<details><summary><code>ToString.intHasToString</code></summary>
<p>
```fstar
ToString.hasToString Prims.int
```
</p>
</details>


<details><summary><code>ToString.join</code></summary>
<p>
```fstar
sep: Prims.string -> l: Prims.list Prims.string -> Prims.Tot Prims.string
```
</p>
</details>


<details><summary><code>ToString.natHasToString</code></summary>
<p>
```fstar
ToString.hasToString Prims.nat
```
</p>
</details>


<details><summary><code>ToString.nat_to_int</code></summary>
<p>
```fstar
i: Prims.nat -> Prims.Tot Prims.int
```
</p>
</details>


<details><summary><code>ToString.op_Hat_Subtraction</code></summary>
<p>
```fstar

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString t0) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString t1) ->
    s0: t0 ->
    s1: t1
  -> Prims.Tot Prims.string
```
</p>
</details>


<details><summary><code>ToString.optionHasToString</code></summary>
<p>
```fstar
(#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString a)
  -> Prims.Tot (ToString.hasToString (FStar.Pervasives.Native.option a))
```
</p>
</details>


<details><summary><code>ToString.stringHasToString</code></summary>
<p>
```fstar
ToString.hasToString Prims.string
```
</p>
</details>


<details><summary><code>ToString.toString</code></summary>
<p>
```fstar
(#[FStar.Tactics.Typeclasses.tcresolve ()] d: ToString.hasToString t)
  -> Prims.Tot (_: t -> Prims.Tot Prims.string)
```
</p>
</details>


<details><summary><code>ToString.tupleHasToString</code></summary>
<p>
```fstar

    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString t1) ->
    (#[FStar.Tactics.Typeclasses.tcresolve ()] _: ToString.hasToString t2)
  -> Prims.Tot (ToString.hasToString (t1 * t2))
```
</p>
</details>


