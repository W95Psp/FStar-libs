open Prims
let (readName :
  Data_Serialize_Types.serialized ->
    (FStar_Reflection_Types.name * Data_Serialize_Types.serialized))
  =
  fun x ->
    let uu____8100 = x in
    match uu____8100 with | (n1::tlN, o) -> (n1, (tlN, o))
let (readInt :
  Data_Serialize_Types.serialized ->
    (Prims.int * Data_Serialize_Types.serialized))
  =
  fun x ->
    let uu____8184 = x in
    match uu____8184 with | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o)))
let (readString :
  Data_Serialize_Types.serialized ->
    (Prims.string * Data_Serialize_Types.serialized))
  =
  fun x ->
    let uu____8292 = x in
    match uu____8292 with
    | (ns, (is, (s::tlS, o))) -> (s, (ns, (is, (tlS, o))))
let (readBool :
  Data_Serialize_Types.serialized ->
    (Prims.bool * Data_Serialize_Types.serialized))
  =
  fun x ->
    let uu____8409 = x in
    match uu____8409 with
    | (ns, (is, (ss, b::tlB))) -> (b, (ns, (is, (ss, tlB))))
let (appendName :
  FStar_Reflection_Types.name ->
    (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
      (Prims.string Prims.list * Prims.bool Prims.list))) ->
      Data_Serialize_Types.serialized)
  =
  fun n1 ->
    fun x ->
      let uu____8574 = x in
      match uu____8574 with | (names, r) -> ((n1 :: names), r)
let (appendInt :
  Prims.int ->
    (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
      (Prims.string Prims.list * Prims.bool Prims.list))) ->
      Data_Serialize_Types.serialized)
  =
  fun i ->
    fun x ->
      let uu____8729 = x in
      match uu____8729 with | (names, (ints, r)) -> (names, ((i :: ints), r))
let (appendString :
  Prims.string ->
    (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
      (Prims.string Prims.list * Prims.bool Prims.list))) ->
      Data_Serialize_Types.serialized)
  =
  fun s ->
    fun x ->
      let uu____8908 = x in
      match uu____8908 with
      | (names, (ints, (strings, bools))) ->
          (names, (ints, ((s :: strings), bools)))
let (appendBool :
  Prims.bool ->
    (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
      (Prims.string Prims.list * Prims.bool Prims.list))) ->
      Data_Serialize_Types.serialized)
  =
  fun b ->
    fun x ->
      let uu____9096 = x in
      match uu____9096 with
      | (names, (ints, (strings, bools))) ->
          (names, (ints, (strings, (b :: bools))))
let appendList :
  'At .
    ('At ->
       Data_Serialize_Types.serialized -> Data_Serialize_Types.serialized)
      ->
      'At Prims.list ->
        Data_Serialize_Types.serialized -> Data_Serialize_Types.serialized
  =
  fun appender ->
    fun v1 ->
      fun s ->
        let s1 =
          FStar_List_Tot_Base.fold_left (fun s1 -> fun v2 -> appender v2 s1)
            s v1 in
        let uu____9273 = s1 in
        match uu____9273 with
        | (names, (ints, r)) ->
            (names, (((FStar_List_Tot_Base.length v1) :: ints), r))
let (compose :
  (Data_Serialize_Types.serialized -> Data_Serialize_Types.serialized) ->
    (Data_Serialize_Types.serialized -> Data_Serialize_Types.serialized) ->
      Data_Serialize_Types.serialized -> Data_Serialize_Types.serialized)
  = fun f -> fun g -> fun x -> f (g x)
let (emptySerialized : Data_Serialize_Types.serialized) =
  ([], ([], ([], [])))
let readList :
  'At .
    (Data_Serialize_Types.serialized ->
       ('At * Data_Serialize_Types.serialized))
      ->
      Data_Serialize_Types.serialized ->
        ('At Prims.list * Data_Serialize_Types.serialized)
  =
  fun f ->
    fun s ->
      let uu____9516 =
        let uu____9522 = s in
        match uu____9522 with | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
      match uu____9516 with
      | (len, s1) ->
          FStar_List_Tot_Base.fold_left
            (fun uu____9630 ->
               fun uu____9631 ->
                 match uu____9630 with
                 | (l, s2) ->
                     let uu____9645 = f s2 in
                     (match uu____9645 with | (x, s3) -> ((x :: l), s3)))
            ([], s1) (Data_Serialize_Helpers.mkList Prims.int_one len)