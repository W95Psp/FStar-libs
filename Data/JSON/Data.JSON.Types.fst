module Data.JSON.Types

type decimalNumber =
  | DecimalNumber : (base: int) -> (commaPosition: nat) -> (exp: int) -> decimalNumber

type jsonValue =
  | JsonObject : list (string * jsonValue) -> jsonValue
  | JsonArray : list jsonValue -> jsonValue
  | JsonString : string -> jsonValue
  | JsonNumber : decimalNumber -> jsonValue
  | JsonBool : bool -> jsonValue
  | JsonNull

// open Data.Serialize

// %splice[] (generateSerialize (`tuple2))
// let nat_serialize_decode_chainable = int_serialize_decode
// let nat_serialize_encode_chainable = int_serialize_encode
// %splice[] (generateSerialize (`decimalNumber))
// %splice[] (generateSerialize (`jsonValue))

// TODO

