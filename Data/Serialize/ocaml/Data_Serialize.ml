open Prims
type serialized = Data_Serialize_Types.serialized
type ('Auu____158332, 'Aa) tserialized =
  ('Auu____158332, unit) Data_Serialize_Types.tserialized
let (fvOf :
  FStar_Reflection_Types.term ->
    (FStar_Reflection_Types.fv, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  = Data_Serialize_Helpers.fvOf
let (name_serialize_decode_chainable :
  Data_Serialize_Types.serialized ->
    (FStar_Reflection_Types.name * Data_Serialize_Types.serialized))
  =
  fun x ->
    let uu____158389 = x in
    match uu____158389 with | (n1::tlN, o) -> (n1, (tlN, o))
let (name_serialize_encode_chainable :
  FStar_Reflection_Types.name ->
    (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
      (Prims.string Prims.list * Prims.bool Prims.list))) ->
      Data_Serialize_Types.serialized)
  =
  fun n1 ->
    fun x ->
      let uu____158522 = x in
      match uu____158522 with | (names, r) -> ((n1 :: names), r)
let (nameHasSerialize :
  FStar_Reflection_Types.name Data_Serialize_Typeclasses.hasSerialize) =
  {
    Data_Serialize_Typeclasses.serialize_chainable =
      name_serialize_encode_chainable;
    Data_Serialize_Typeclasses.deserialize_chainable =
      name_serialize_decode_chainable
  }
let (int_serialize_decode_chainable :
  Data_Serialize_Types.serialized ->
    (Prims.int * Data_Serialize_Types.serialized))
  =
  fun x ->
    let uu____158632 = x in
    match uu____158632 with | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o)))
let (int_serialize_encode_chainable :
  Prims.int ->
    (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
      (Prims.string Prims.list * Prims.bool Prims.list))) ->
      Data_Serialize_Types.serialized)
  =
  fun i ->
    fun x ->
      let uu____158789 = x in
      match uu____158789 with
      | (names, (ints, r)) -> (names, ((i :: ints), r))
let (intHasSerialize : Prims.int Data_Serialize_Typeclasses.hasSerialize) =
  {
    Data_Serialize_Typeclasses.serialize_chainable =
      int_serialize_encode_chainable;
    Data_Serialize_Typeclasses.deserialize_chainable =
      int_serialize_decode_chainable
  }
let (string_serialize_decode_chainable :
  Data_Serialize_Types.serialized ->
    (Prims.string * Data_Serialize_Types.serialized))
  =
  fun x ->
    let uu____158923 = x in
    match uu____158923 with
    | (ns, (is, (s::tlS, o))) -> (s, (ns, (is, (tlS, o))))
let (string_serialize_encode_chainable :
  Prims.string ->
    (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
      (Prims.string Prims.list * Prims.bool Prims.list))) ->
      Data_Serialize_Types.serialized)
  =
  fun s ->
    fun x ->
      let uu____159090 = x in
      match uu____159090 with
      | (names, (ints, (strings, bools))) ->
          (names, (ints, ((s :: strings), bools)))
let (stringHasSerialize :
  Prims.string Data_Serialize_Typeclasses.hasSerialize) =
  {
    Data_Serialize_Typeclasses.serialize_chainable =
      string_serialize_encode_chainable;
    Data_Serialize_Typeclasses.deserialize_chainable =
      string_serialize_decode_chainable
  }
let (bool_serialize_decode_chainable :
  Data_Serialize_Types.serialized ->
    (Prims.bool * Data_Serialize_Types.serialized))
  =
  fun x ->
    let uu____159233 = x in
    match uu____159233 with
    | (ns, (is, (ss, b::tlB))) -> (b, (ns, (is, (ss, tlB))))
let (bool_serialize_encode_chainable :
  Prims.bool ->
    (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
      (Prims.string Prims.list * Prims.bool Prims.list))) ->
      Data_Serialize_Types.serialized)
  =
  fun b ->
    fun x ->
      let uu____159400 = x in
      match uu____159400 with
      | (names, (ints, (strings, bools))) ->
          (names, (ints, (strings, (b :: bools))))
let (boolHasSerialize : Prims.bool Data_Serialize_Typeclasses.hasSerialize) =
  {
    Data_Serialize_Typeclasses.serialize_chainable =
      bool_serialize_encode_chainable;
    Data_Serialize_Typeclasses.deserialize_chainable =
      bool_serialize_decode_chainable
  }
let list_serialize_decode_chainable :
  'Auu____159531 .
    unit ->
      (Data_Serialize_Types.serialized ->
         ('Auu____159531 * Data_Serialize_Types.serialized))
        ->
        Data_Serialize_Types.serialized ->
          ('Auu____159531 Prims.list * Data_Serialize_Types.serialized)
  = fun uu____159551 -> Data_Serialize_Helpers_Serialized.readList
let list_serialize_encode_chainable :
  'Auu____159559 .
    unit ->
      ('Auu____159559 ->
         Data_Serialize_Types.serialized -> Data_Serialize_Types.serialized)
        ->
        'Auu____159559 Prims.list ->
          Data_Serialize_Types.serialized -> Data_Serialize_Types.serialized
  = fun uu____159577 -> Data_Serialize_Helpers_Serialized.appendList
let listHasSerialize :
  'Aa .
    'Aa Data_Serialize_Typeclasses.hasSerialize ->
      'Aa Prims.list Data_Serialize_Typeclasses.hasSerialize
  =
  fun uu___0_159596 ->
    {
      Data_Serialize_Typeclasses.serialize_chainable =
        ((list_serialize_encode_chainable ())
           (Data_Serialize_Typeclasses.serialize_chainable uu___0_159596));
      Data_Serialize_Typeclasses.deserialize_chainable =
        ((list_serialize_decode_chainable ())
           (Data_Serialize_Typeclasses.deserialize_chainable uu___0_159596))
    }
let (generateSerialize' :
  FStar_Reflection_Types.fv ->
    (FStar_Reflection_Types.sigelt Prims.list, unit)
      FStar_Tactics_Effect._dm4f_TAC_repr)
  = Data_Serialize_Typeclasses.generateSerialize'
let (generateSerialize :
  FStar_Reflection_Types.term ->
    (FStar_Reflection_Types.sigelt Prims.list, unit)
      FStar_Tactics_Effect._dm4f_TAC_repr)
  = Data_Serialize_Typeclasses.generateSerialize
let rec option_serialize_decode_chainable :
  'Auu____159853 .
    (Data_Serialize_Types.serialized ->
       ('Auu____159853 * Data_Serialize_Types.serialized))
      ->
      Data_Serialize_Types.serialized ->
        ('Auu____159853 FStar_Pervasives_Native.option *
          Data_Serialize_Types.serialized)
  =
  fun x2 ->
    fun x3 ->
      let x4 =
        let uu____159889 = x3 in
        match uu____159889 with | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
      let x5 = FStar_Pervasives_Native.fst x4 in
      let x6 = FStar_Pervasives_Native.snd x4 in
      match x5 with
      | _159984 when _159984 = Prims.int_zero ->
          (FStar_Pervasives_Native.None, x6)
      | _159987 when _159987 = Prims.int_one ->
          let x8 = x2 x6 in
          let x9 = FStar_Pervasives_Native.fst x8 in
          let x10 = FStar_Pervasives_Native.snd x8 in
          ((FStar_Pervasives_Native.Some x9), x10)
      | x11 -> Data_Serialize_Helpers.mkerror "mkMatchInt"
let rec option_serialize_decode :
  'Auu____160030 .
    (Data_Serialize_Types.serialized ->
       ('Auu____160030 * Data_Serialize_Types.serialized))
      ->
      Data_Serialize_Types.serialized ->
        'Auu____160030 FStar_Pervasives_Native.option
  =
  fun x0 ->
    fun x1 ->
      FStar_Pervasives_Native.fst
        (let x4 =
           let uu____160067 = x1 in
           match uu____160067 with | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
         let x5 = FStar_Pervasives_Native.fst x4 in
         let x6 = FStar_Pervasives_Native.snd x4 in
         match x5 with
         | _160162 when _160162 = Prims.int_zero ->
             (FStar_Pervasives_Native.None, x6)
         | _160165 when _160165 = Prims.int_one ->
             let x8 = x0 x6 in
             let x9 = FStar_Pervasives_Native.fst x8 in
             let x10 = FStar_Pervasives_Native.snd x8 in
             ((FStar_Pervasives_Native.Some x9), x10)
         | x11 -> Data_Serialize_Helpers.mkerror "mkMatchInt")
let rec option_serialize_encode_chainable :
  'Auu____160209 .
    ('Auu____160209 ->
       Data_Serialize_Types.serialized ->
         (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
           (Prims.string Prims.list * Prims.bool Prims.list))))
      ->
      'Auu____160209 FStar_Pervasives_Native.option ->
        Data_Serialize_Types.serialized -> Data_Serialize_Types.serialized
  =
  fun x15 ->
    fun x16 ->
      fun x17 ->
        match x16 with
        | FStar_Pervasives_Native.None ->
            let uu____160287 = x17 in
            (match uu____160287 with
             | (names, (ints, r)) -> (names, ((Prims.int_zero :: ints), r)))
        | FStar_Pervasives_Native.Some x18 ->
            let uu____160400 = x15 x18 x17 in
            (match uu____160400 with
             | (names, (ints, r)) -> (names, ((Prims.int_one :: ints), r)))
let rec option_serialize_encode :
  'Auu____160534 .
    ('Auu____160534 ->
       Data_Serialize_Types.serialized ->
         (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
           (Prims.string Prims.list * Prims.bool Prims.list))))
      ->
      'Auu____160534 FStar_Pervasives_Native.option ->
        Data_Serialize_Types.serialized
  =
  fun x12 ->
    fun x13 ->
      match x13 with
      | FStar_Pervasives_Native.None ->
          let uu____160607 =
            Data_Serialize_Helpers_Serialized.emptySerialized in
          (match uu____160607 with
           | (names, (ints, r)) -> (names, ((Prims.int_zero :: ints), r)))
      | FStar_Pervasives_Native.Some x18 ->
          let uu____160720 =
            x12 x18 Data_Serialize_Helpers_Serialized.emptySerialized in
          (match uu____160720 with
           | (names, (ints, r)) -> (names, ((Prims.int_one :: ints), r)))
let option_hasSerialize :
  'Ax19 .
    'Ax19 Data_Serialize_Typeclasses.hasSerialize ->
      'Ax19 FStar_Pervasives_Native.option
        Data_Serialize_Typeclasses.hasSerialize
  =
  fun x20 ->
    {
      Data_Serialize_Typeclasses.serialize_chainable =
        (option_serialize_encode_chainable
           (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
              x20));
      Data_Serialize_Typeclasses.deserialize_chainable =
        (option_serialize_decode_chainable
           (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
              x20))
    }
let rec either_serialize_decode_chainable :
  'Auu____160894 'Auu____160895 'Auu____160896 .
    (Data_Serialize_Types.serialized -> ('Auu____160894 * 'Auu____160895)) ->
      (Data_Serialize_Types.serialized -> ('Auu____160896 * 'Auu____160895))
        ->
        Data_Serialize_Types.serialized ->
          (('Auu____160894, 'Auu____160896) FStar_Pervasives.either *
            'Auu____160895)
  =
  fun x3 ->
    fun x4 ->
      fun x5 ->
        let x6 =
          let uu____160952 = x5 in
          match uu____160952 with | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
        let x7 = FStar_Pervasives_Native.fst x6 in
        let x8 = FStar_Pervasives_Native.snd x6 in
        match x7 with
        | _161047 when _161047 = Prims.int_zero ->
            let x10 = x3 x8 in
            let x11 = FStar_Pervasives_Native.fst x10 in
            let x12 = FStar_Pervasives_Native.snd x10 in
            ((FStar_Pervasives.Inl x11), x12)
        | _161059 when _161059 = Prims.int_one ->
            let x14 = x4 x8 in
            let x15 = FStar_Pervasives_Native.fst x14 in
            let x16 = FStar_Pervasives_Native.snd x14 in
            ((FStar_Pervasives.Inr x15), x16)
        | x17 -> Data_Serialize_Helpers.mkerror "mkMatchInt"
let rec either_serialize_decode :
  'Auu____161121 'Auu____161122 'Auu____161123 .
    (Data_Serialize_Types.serialized -> ('Auu____161121 * 'Auu____161122)) ->
      (Data_Serialize_Types.serialized -> ('Auu____161123 * 'Auu____161122))
        ->
        Data_Serialize_Types.serialized ->
          ('Auu____161121, 'Auu____161123) FStar_Pervasives.either
  =
  fun x0 ->
    fun x1 ->
      fun x2 ->
        FStar_Pervasives_Native.fst
          (let x6 =
             let uu____161185 = x2 in
             match uu____161185 with
             | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
           let x7 = FStar_Pervasives_Native.fst x6 in
           let x8 = FStar_Pervasives_Native.snd x6 in
           match x7 with
           | _161280 when _161280 = Prims.int_zero ->
               let x10 = x0 x8 in
               let x11 = FStar_Pervasives_Native.fst x10 in
               let x12 = FStar_Pervasives_Native.snd x10 in
               ((FStar_Pervasives.Inl x11), x12)
           | _161292 when _161292 = Prims.int_one ->
               let x14 = x1 x8 in
               let x15 = FStar_Pervasives_Native.fst x14 in
               let x16 = FStar_Pervasives_Native.snd x14 in
               ((FStar_Pervasives.Inr x15), x16)
           | x17 -> Data_Serialize_Helpers.mkerror "mkMatchInt")
let rec either_serialize_encode_chainable :
  'Auu____161350 'Auu____161351 .
    ('Auu____161350 ->
       Data_Serialize_Types.serialized ->
         (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
           (Prims.string Prims.list * Prims.bool Prims.list))))
      ->
      ('Auu____161351 ->
         Data_Serialize_Types.serialized ->
           (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
             (Prims.string Prims.list * Prims.bool Prims.list))))
        ->
        ('Auu____161350, 'Auu____161351) FStar_Pervasives.either ->
          Data_Serialize_Types.serialized -> Data_Serialize_Types.serialized
  =
  fun x22 ->
    fun x23 ->
      fun x24 ->
        fun x25 ->
          match x24 with
          | FStar_Pervasives.Inl x26 ->
              let uu____161497 = x22 x26 x25 in
              (match uu____161497 with
               | (names, (ints, r)) -> (names, ((Prims.int_zero :: ints), r)))
          | FStar_Pervasives.Inr x27 ->
              let uu____161610 = x23 x27 x25 in
              (match uu____161610 with
               | (names, (ints, r)) -> (names, ((Prims.int_one :: ints), r)))
let rec either_serialize_encode :
  'Auu____161754 'Auu____161755 .
    ('Auu____161754 ->
       Data_Serialize_Types.serialized ->
         (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
           (Prims.string Prims.list * Prims.bool Prims.list))))
      ->
      ('Auu____161755 ->
         Data_Serialize_Types.serialized ->
           (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
             (Prims.string Prims.list * Prims.bool Prims.list))))
        ->
        ('Auu____161754, 'Auu____161755) FStar_Pervasives.either ->
          Data_Serialize_Types.serialized
  =
  fun x18 ->
    fun x19 ->
      fun x20 ->
        match x20 with
        | FStar_Pervasives.Inl x26 ->
            let uu____161896 =
              x18 x26 Data_Serialize_Helpers_Serialized.emptySerialized in
            (match uu____161896 with
             | (names, (ints, r)) -> (names, ((Prims.int_zero :: ints), r)))
        | FStar_Pervasives.Inr x27 ->
            let uu____162009 =
              x19 x27 Data_Serialize_Helpers_Serialized.emptySerialized in
            (match uu____162009 with
             | (names, (ints, r)) -> (names, ((Prims.int_one :: ints), r)))
let either_hasSerialize :
  'Ax28 'Ax29 .
    'Ax28 Data_Serialize_Typeclasses.hasSerialize ->
      'Ax29 Data_Serialize_Typeclasses.hasSerialize ->
        ('Ax28, 'Ax29) FStar_Pervasives.either
          Data_Serialize_Typeclasses.hasSerialize
  =
  fun x30 ->
    fun x31 ->
      {
        Data_Serialize_Typeclasses.serialize_chainable =
          (either_serialize_encode_chainable
             (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                x30)
             (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                x31));
        Data_Serialize_Typeclasses.deserialize_chainable =
          (either_serialize_decode_chainable
             (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                x30)
             (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                x31))
      }
let rec tuple2_serialize_decode_chainable :
  'Auu____162207 'Auu____162208 'Auu____162209 'Auu____162210 .
    ('Auu____162207 -> ('Auu____162208 * 'Auu____162209)) ->
      (Data_Serialize_Types.serialized -> ('Auu____162210 * 'Auu____162207))
        ->
        Data_Serialize_Types.serialized ->
          (('Auu____162208 * 'Auu____162210) * 'Auu____162209)
  =
  fun x3 ->
    fun x4 ->
      fun x5 ->
        let x6 =
          let uu____162266 = x5 in
          match uu____162266 with | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
        let x7 = FStar_Pervasives_Native.fst x6 in
        let x8 = FStar_Pervasives_Native.snd x6 in
        match x7 with
        | _162361 when _162361 = Prims.int_zero ->
            let x10 = x4 x8 in
            let x11 = FStar_Pervasives_Native.fst x10 in
            let x12 = FStar_Pervasives_Native.snd x10 in
            let x14 = x3 x12 in
            let x15 = FStar_Pervasives_Native.fst x14 in
            let x16 = FStar_Pervasives_Native.snd x14 in ((x15, x11), x16)
        | x17 -> Data_Serialize_Helpers.mkerror "mkMatchInt"
let rec tuple2_serialize_decode :
  'Auu____162434 'Auu____162435 'Auu____162436 'Auu____162437 .
    ('Auu____162434 -> ('Auu____162435 * 'Auu____162436)) ->
      (Data_Serialize_Types.serialized -> ('Auu____162437 * 'Auu____162434))
        ->
        Data_Serialize_Types.serialized -> ('Auu____162435 * 'Auu____162437)
  =
  fun x0 ->
    fun x1 ->
      fun x2 ->
        FStar_Pervasives_Native.fst
          (let x6 =
             let uu____162502 = x2 in
             match uu____162502 with
             | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
           let x7 = FStar_Pervasives_Native.fst x6 in
           let x8 = FStar_Pervasives_Native.snd x6 in
           match x7 with
           | _162597 when _162597 = Prims.int_zero ->
               let x10 = x1 x8 in
               let x11 = FStar_Pervasives_Native.fst x10 in
               let x12 = FStar_Pervasives_Native.snd x10 in
               let x14 = x0 x12 in
               let x15 = FStar_Pervasives_Native.fst x14 in
               let x16 = FStar_Pervasives_Native.snd x14 in ((x15, x11), x16)
           | x17 -> Data_Serialize_Helpers.mkerror "mkMatchInt")
let rec tuple2_serialize_encode_chainable :
  'Auu____162662 'Auu____162663 'Auu____162664 .
    ('Auu____162662 -> Data_Serialize_Types.serialized -> 'Auu____162663) ->
      ('Auu____162664 ->
         'Auu____162663 ->
           (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
             (Prims.string Prims.list * Prims.bool Prims.list))))
        ->
        ('Auu____162662 * 'Auu____162664) ->
          Data_Serialize_Types.serialized -> Data_Serialize_Types.serialized
  =
  fun x22 ->
    fun x23 ->
      fun x24 ->
        fun x25 ->
          match x24 with
          | (x26, x27) ->
              let uu____162763 = x23 x27 (x22 x26 x25) in
              (match uu____162763 with
               | (names, (ints, r)) -> (names, ((Prims.int_zero :: ints), r)))
let rec tuple2_serialize_encode :
  'Auu____162907 'Auu____162908 'Auu____162909 .
    ('Auu____162907 -> Data_Serialize_Types.serialized -> 'Auu____162908) ->
      ('Auu____162909 ->
         'Auu____162908 ->
           (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
             (Prims.string Prims.list * Prims.bool Prims.list))))
        ->
        ('Auu____162907 * 'Auu____162909) -> Data_Serialize_Types.serialized
  =
  fun x18 ->
    fun x19 ->
      fun x20 ->
        match x20 with
        | (x26, x27) ->
            let uu____163003 =
              x19 x27
                (x18 x26 Data_Serialize_Helpers_Serialized.emptySerialized) in
            (match uu____163003 with
             | (names, (ints, r)) -> (names, ((Prims.int_zero :: ints), r)))
let tuple2_hasSerialize :
  'Ax28 'Ax29 .
    'Ax28 Data_Serialize_Typeclasses.hasSerialize ->
      'Ax29 Data_Serialize_Typeclasses.hasSerialize ->
        ('Ax28 * 'Ax29) Data_Serialize_Typeclasses.hasSerialize
  =
  fun x30 ->
    fun x31 ->
      {
        Data_Serialize_Typeclasses.serialize_chainable =
          (tuple2_serialize_encode_chainable
             (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                x30)
             (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                x31));
        Data_Serialize_Typeclasses.deserialize_chainable =
          (tuple2_serialize_decode_chainable
             (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                x30)
             (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                x31))
      }
let rec tuple3_serialize_decode_chainable :
  'Auu____163216 'Auu____163217 'Auu____163218 'Auu____163219 'Auu____163220
    'Auu____163221 .
    ('Auu____163216 -> ('Auu____163217 * 'Auu____163218)) ->
      ('Auu____163219 -> ('Auu____163220 * 'Auu____163216)) ->
        (Data_Serialize_Types.serialized -> ('Auu____163221 * 'Auu____163219))
          ->
          Data_Serialize_Types.serialized ->
            (('Auu____163217 * 'Auu____163220 * 'Auu____163221) *
              'Auu____163218)
  =
  fun x4 ->
    fun x5 ->
      fun x6 ->
        fun x7 ->
          let x8 =
            let uu____163297 = x7 in
            match uu____163297 with
            | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
          let x9 = FStar_Pervasives_Native.fst x8 in
          let x10 = FStar_Pervasives_Native.snd x8 in
          match x9 with
          | _163392 when _163392 = Prims.int_zero ->
              let x12 = x6 x10 in
              let x13 = FStar_Pervasives_Native.fst x12 in
              let x14 = FStar_Pervasives_Native.snd x12 in
              let x16 = x5 x14 in
              let x17 = FStar_Pervasives_Native.fst x16 in
              let x18 = FStar_Pervasives_Native.snd x16 in
              let x20 = x4 x18 in
              let x21 = FStar_Pervasives_Native.fst x20 in
              let x22 = FStar_Pervasives_Native.snd x20 in
              ((x21, x17, x13), x22)
          | x23 -> Data_Serialize_Helpers.mkerror "mkMatchInt"
let rec tuple3_serialize_decode :
  'Auu____163491 'Auu____163492 'Auu____163493 'Auu____163494 'Auu____163495
    'Auu____163496 .
    ('Auu____163491 -> ('Auu____163492 * 'Auu____163493)) ->
      ('Auu____163494 -> ('Auu____163495 * 'Auu____163491)) ->
        (Data_Serialize_Types.serialized -> ('Auu____163496 * 'Auu____163494))
          ->
          Data_Serialize_Types.serialized ->
            ('Auu____163492 * 'Auu____163495 * 'Auu____163496)
  =
  fun x0 ->
    fun x1 ->
      fun x2 ->
        fun x3 ->
          FStar_Pervasives_Native.fst
            (let x8 =
               let uu____163586 = x3 in
               match uu____163586 with
               | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
             let x9 = FStar_Pervasives_Native.fst x8 in
             let x10 = FStar_Pervasives_Native.snd x8 in
             match x9 with
             | _163681 when _163681 = Prims.int_zero ->
                 let x12 = x2 x10 in
                 let x13 = FStar_Pervasives_Native.fst x12 in
                 let x14 = FStar_Pervasives_Native.snd x12 in
                 let x16 = x1 x14 in
                 let x17 = FStar_Pervasives_Native.fst x16 in
                 let x18 = FStar_Pervasives_Native.snd x16 in
                 let x20 = x0 x18 in
                 let x21 = FStar_Pervasives_Native.fst x20 in
                 let x22 = FStar_Pervasives_Native.snd x20 in
                 ((x21, x17, x13), x22)
             | x23 -> Data_Serialize_Helpers.mkerror "mkMatchInt")
let rec tuple3_serialize_encode_chainable :
  'Auu____163771 'Auu____163772 'Auu____163773 'Auu____163774 'Auu____163775
    .
    ('Auu____163771 -> Data_Serialize_Types.serialized -> 'Auu____163772) ->
      ('Auu____163773 -> 'Auu____163772 -> 'Auu____163774) ->
        ('Auu____163775 ->
           'Auu____163774 ->
             (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list
               * (Prims.string Prims.list * Prims.bool Prims.list))))
          ->
          ('Auu____163771 * 'Auu____163773 * 'Auu____163775) ->
            Data_Serialize_Types.serialized ->
              Data_Serialize_Types.serialized
  =
  fun x29 ->
    fun x30 ->
      fun x31 ->
        fun x32 ->
          fun x33 ->
            match x32 with
            | (x34, x35, x36) ->
                let uu____163894 = x31 x36 (x30 x35 (x29 x34 x33)) in
                (match uu____163894 with
                 | (names, (ints, r)) ->
                     (names, ((Prims.int_zero :: ints), r)))
let rec tuple3_serialize_encode :
  'Auu____164052 'Auu____164053 'Auu____164054 'Auu____164055 'Auu____164056
    .
    ('Auu____164052 -> Data_Serialize_Types.serialized -> 'Auu____164053) ->
      ('Auu____164054 -> 'Auu____164053 -> 'Auu____164055) ->
        ('Auu____164056 ->
           'Auu____164055 ->
             (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list
               * (Prims.string Prims.list * Prims.bool Prims.list))))
          ->
          ('Auu____164052 * 'Auu____164054 * 'Auu____164056) ->
            Data_Serialize_Types.serialized
  =
  fun x24 ->
    fun x25 ->
      fun x26 ->
        fun x27 ->
          match x27 with
          | (x34, x35, x36) ->
              let uu____164170 =
                x26 x36
                  (x25 x35
                     (x24 x34
                        Data_Serialize_Helpers_Serialized.emptySerialized)) in
              (match uu____164170 with
               | (names, (ints, r)) -> (names, ((Prims.int_zero :: ints), r)))
let tuple3_hasSerialize :
  'Ax37 'Ax38 'Ax39 .
    'Ax37 Data_Serialize_Typeclasses.hasSerialize ->
      'Ax38 Data_Serialize_Typeclasses.hasSerialize ->
        'Ax39 Data_Serialize_Typeclasses.hasSerialize ->
          ('Ax37 * 'Ax38 * 'Ax39) Data_Serialize_Typeclasses.hasSerialize
  =
  fun x40 ->
    fun x41 ->
      fun x42 ->
        {
          Data_Serialize_Typeclasses.serialize_chainable =
            (tuple3_serialize_encode_chainable
               (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                  x40)
               (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                  x41)
               (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                  x42));
          Data_Serialize_Typeclasses.deserialize_chainable =
            (tuple3_serialize_decode_chainable
               (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                  x40)
               (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                  x41)
               (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                  x42))
        }
let rec tuple4_serialize_decode_chainable :
  'Auu____164418 'Auu____164419 'Auu____164420 'Auu____164421 'Auu____164422
    'Auu____164423 'Auu____164424 'Auu____164425 .
    ('Auu____164418 -> ('Auu____164419 * 'Auu____164420)) ->
      ('Auu____164421 -> ('Auu____164422 * 'Auu____164418)) ->
        ('Auu____164423 -> ('Auu____164424 * 'Auu____164421)) ->
          (Data_Serialize_Types.serialized ->
             ('Auu____164425 * 'Auu____164423))
            ->
            Data_Serialize_Types.serialized ->
              (('Auu____164419 * 'Auu____164422 * 'Auu____164424 *
                'Auu____164425) * 'Auu____164420)
  =
  fun x5 ->
    fun x6 ->
      fun x7 ->
        fun x8 ->
          fun x9 ->
            let x10 =
              let uu____164521 = x9 in
              match uu____164521 with
              | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
            let x11 = FStar_Pervasives_Native.fst x10 in
            let x12 = FStar_Pervasives_Native.snd x10 in
            match x11 with
            | _164616 when _164616 = Prims.int_zero ->
                let x14 = x8 x12 in
                let x15 = FStar_Pervasives_Native.fst x14 in
                let x16 = FStar_Pervasives_Native.snd x14 in
                let x18 = x7 x16 in
                let x19 = FStar_Pervasives_Native.fst x18 in
                let x20 = FStar_Pervasives_Native.snd x18 in
                let x22 = x6 x20 in
                let x23 = FStar_Pervasives_Native.fst x22 in
                let x24 = FStar_Pervasives_Native.snd x22 in
                let x26 = x5 x24 in
                let x27 = FStar_Pervasives_Native.fst x26 in
                let x28 = FStar_Pervasives_Native.snd x26 in
                ((x27, x23, x19, x15), x28)
            | x29 -> Data_Serialize_Helpers.mkerror "mkMatchInt"
let rec tuple4_serialize_decode :
  'Auu____164741 'Auu____164742 'Auu____164743 'Auu____164744 'Auu____164745
    'Auu____164746 'Auu____164747 'Auu____164748 .
    ('Auu____164741 -> ('Auu____164742 * 'Auu____164743)) ->
      ('Auu____164744 -> ('Auu____164745 * 'Auu____164741)) ->
        ('Auu____164746 -> ('Auu____164747 * 'Auu____164744)) ->
          (Data_Serialize_Types.serialized ->
             ('Auu____164748 * 'Auu____164746))
            ->
            Data_Serialize_Types.serialized ->
              ('Auu____164742 * 'Auu____164745 * 'Auu____164747 *
                'Auu____164748)
  =
  fun x0 ->
    fun x1 ->
      fun x2 ->
        fun x3 ->
          fun x4 ->
            FStar_Pervasives_Native.fst
              (let x10 =
                 let uu____164863 = x4 in
                 match uu____164863 with
                 | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
               let x11 = FStar_Pervasives_Native.fst x10 in
               let x12 = FStar_Pervasives_Native.snd x10 in
               match x11 with
               | _164958 when _164958 = Prims.int_zero ->
                   let x14 = x3 x12 in
                   let x15 = FStar_Pervasives_Native.fst x14 in
                   let x16 = FStar_Pervasives_Native.snd x14 in
                   let x18 = x2 x16 in
                   let x19 = FStar_Pervasives_Native.fst x18 in
                   let x20 = FStar_Pervasives_Native.snd x18 in
                   let x22 = x1 x20 in
                   let x23 = FStar_Pervasives_Native.fst x22 in
                   let x24 = FStar_Pervasives_Native.snd x22 in
                   let x26 = x0 x24 in
                   let x27 = FStar_Pervasives_Native.fst x26 in
                   let x28 = FStar_Pervasives_Native.snd x26 in
                   ((x27, x23, x19, x15), x28)
               | x29 -> Data_Serialize_Helpers.mkerror "mkMatchInt")
let rec tuple4_serialize_encode_chainable :
  'Auu____165073 'Auu____165074 'Auu____165075 'Auu____165076 'Auu____165077
    'Auu____165078 'Auu____165079 .
    ('Auu____165073 -> Data_Serialize_Types.serialized -> 'Auu____165074) ->
      ('Auu____165075 -> 'Auu____165074 -> 'Auu____165076) ->
        ('Auu____165077 -> 'Auu____165076 -> 'Auu____165078) ->
          ('Auu____165079 ->
             'Auu____165078 ->
               (FStar_Reflection_Types.name Prims.list * (Prims.int
                 Prims.list * (Prims.string Prims.list * Prims.bool
                 Prims.list))))
            ->
            ('Auu____165073 * 'Auu____165075 * 'Auu____165077 *
              'Auu____165079) ->
              Data_Serialize_Types.serialized ->
                Data_Serialize_Types.serialized
  =
  fun x36 ->
    fun x37 ->
      fun x38 ->
        fun x39 ->
          fun x40 ->
            fun x41 ->
              match x40 with
              | (x42, x43, x44, x45) ->
                  let uu____165218 =
                    x39 x45 (x38 x44 (x37 x43 (x36 x42 x41))) in
                  (match uu____165218 with
                   | (names, (ints, r)) ->
                       (names, ((Prims.int_zero :: ints), r)))
let rec tuple4_serialize_encode :
  'Auu____165390 'Auu____165391 'Auu____165392 'Auu____165393 'Auu____165394
    'Auu____165395 'Auu____165396 .
    ('Auu____165390 -> Data_Serialize_Types.serialized -> 'Auu____165391) ->
      ('Auu____165392 -> 'Auu____165391 -> 'Auu____165393) ->
        ('Auu____165394 -> 'Auu____165393 -> 'Auu____165395) ->
          ('Auu____165396 ->
             'Auu____165395 ->
               (FStar_Reflection_Types.name Prims.list * (Prims.int
                 Prims.list * (Prims.string Prims.list * Prims.bool
                 Prims.list))))
            ->
            ('Auu____165390 * 'Auu____165392 * 'Auu____165394 *
              'Auu____165396) -> Data_Serialize_Types.serialized
  =
  fun x30 ->
    fun x31 ->
      fun x32 ->
        fun x33 ->
          fun x34 ->
            match x34 with
            | (x42, x43, x44, x45) ->
                let uu____165530 =
                  x33 x45
                    (x32 x44
                       (x31 x43
                          (x30 x42
                             Data_Serialize_Helpers_Serialized.emptySerialized))) in
                (match uu____165530 with
                 | (names, (ints, r)) ->
                     (names, ((Prims.int_zero :: ints), r)))
let tuple4_hasSerialize :
  'Ax46 'Ax47 'Ax48 'Ax49 .
    'Ax46 Data_Serialize_Typeclasses.hasSerialize ->
      'Ax47 Data_Serialize_Typeclasses.hasSerialize ->
        'Ax48 Data_Serialize_Typeclasses.hasSerialize ->
          'Ax49 Data_Serialize_Typeclasses.hasSerialize ->
            ('Ax46 * 'Ax47 * 'Ax48 * 'Ax49)
              Data_Serialize_Typeclasses.hasSerialize
  =
  fun x50 ->
    fun x51 ->
      fun x52 ->
        fun x53 ->
          {
            Data_Serialize_Typeclasses.serialize_chainable =
              (tuple4_serialize_encode_chainable
                 (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                    x50)
                 (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                    x51)
                 (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                    x52)
                 (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                    x53));
            Data_Serialize_Typeclasses.deserialize_chainable =
              (tuple4_serialize_decode_chainable
                 (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                    x50)
                 (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                    x51)
                 (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                    x52)
                 (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                    x53))
          }
let rec tuple5_serialize_decode_chainable :
  'Auu____165813 'Auu____165814 'Auu____165815 'Auu____165816 'Auu____165817
    'Auu____165818 'Auu____165819 'Auu____165820 'Auu____165821
    'Auu____165822 .
    ('Auu____165813 -> ('Auu____165814 * 'Auu____165815)) ->
      ('Auu____165816 -> ('Auu____165817 * 'Auu____165813)) ->
        ('Auu____165818 -> ('Auu____165819 * 'Auu____165816)) ->
          ('Auu____165820 -> ('Auu____165821 * 'Auu____165818)) ->
            (Data_Serialize_Types.serialized ->
               ('Auu____165822 * 'Auu____165820))
              ->
              Data_Serialize_Types.serialized ->
                (('Auu____165814 * 'Auu____165817 * 'Auu____165819 *
                  'Auu____165821 * 'Auu____165822) * 'Auu____165815)
  =
  fun x6 ->
    fun x7 ->
      fun x8 ->
        fun x9 ->
          fun x10 ->
            fun x11 ->
              let x12 =
                let uu____165938 = x11 in
                match uu____165938 with
                | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
              let x13 = FStar_Pervasives_Native.fst x12 in
              let x14 = FStar_Pervasives_Native.snd x12 in
              match x13 with
              | _166033 when _166033 = Prims.int_zero ->
                  let x16 = x10 x14 in
                  let x17 = FStar_Pervasives_Native.fst x16 in
                  let x18 = FStar_Pervasives_Native.snd x16 in
                  let x20 = x9 x18 in
                  let x21 = FStar_Pervasives_Native.fst x20 in
                  let x22 = FStar_Pervasives_Native.snd x20 in
                  let x24 = x8 x22 in
                  let x25 = FStar_Pervasives_Native.fst x24 in
                  let x26 = FStar_Pervasives_Native.snd x24 in
                  let x28 = x7 x26 in
                  let x29 = FStar_Pervasives_Native.fst x28 in
                  let x30 = FStar_Pervasives_Native.snd x28 in
                  let x32 = x6 x30 in
                  let x33 = FStar_Pervasives_Native.fst x32 in
                  let x34 = FStar_Pervasives_Native.snd x32 in
                  ((x33, x29, x25, x21, x17), x34)
              | x35 -> Data_Serialize_Helpers.mkerror "mkMatchInt"
let rec tuple5_serialize_decode :
  'Auu____166184 'Auu____166185 'Auu____166186 'Auu____166187 'Auu____166188
    'Auu____166189 'Auu____166190 'Auu____166191 'Auu____166192
    'Auu____166193 .
    ('Auu____166184 -> ('Auu____166185 * 'Auu____166186)) ->
      ('Auu____166187 -> ('Auu____166188 * 'Auu____166184)) ->
        ('Auu____166189 -> ('Auu____166190 * 'Auu____166187)) ->
          ('Auu____166191 -> ('Auu____166192 * 'Auu____166189)) ->
            (Data_Serialize_Types.serialized ->
               ('Auu____166193 * 'Auu____166191))
              ->
              Data_Serialize_Types.serialized ->
                ('Auu____166185 * 'Auu____166188 * 'Auu____166190 *
                  'Auu____166192 * 'Auu____166193)
  =
  fun x0 ->
    fun x1 ->
      fun x2 ->
        fun x3 ->
          fun x4 ->
            fun x5 ->
              FStar_Pervasives_Native.fst
                (let x12 =
                   let uu____166333 = x5 in
                   match uu____166333 with
                   | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
                 let x13 = FStar_Pervasives_Native.fst x12 in
                 let x14 = FStar_Pervasives_Native.snd x12 in
                 match x13 with
                 | _166428 when _166428 = Prims.int_zero ->
                     let x16 = x4 x14 in
                     let x17 = FStar_Pervasives_Native.fst x16 in
                     let x18 = FStar_Pervasives_Native.snd x16 in
                     let x20 = x3 x18 in
                     let x21 = FStar_Pervasives_Native.fst x20 in
                     let x22 = FStar_Pervasives_Native.snd x20 in
                     let x24 = x2 x22 in
                     let x25 = FStar_Pervasives_Native.fst x24 in
                     let x26 = FStar_Pervasives_Native.snd x24 in
                     let x28 = x1 x26 in
                     let x29 = FStar_Pervasives_Native.fst x28 in
                     let x30 = FStar_Pervasives_Native.snd x28 in
                     let x32 = x0 x30 in
                     let x33 = FStar_Pervasives_Native.fst x32 in
                     let x34 = FStar_Pervasives_Native.snd x32 in
                     ((x33, x29, x25, x21, x17), x34)
                 | x35 -> Data_Serialize_Helpers.mkerror "mkMatchInt")
let rec tuple5_serialize_encode_chainable :
  'Auu____166568 'Auu____166569 'Auu____166570 'Auu____166571 'Auu____166572
    'Auu____166573 'Auu____166574 'Auu____166575 'Auu____166576 .
    ('Auu____166568 -> Data_Serialize_Types.serialized -> 'Auu____166569) ->
      ('Auu____166570 -> 'Auu____166569 -> 'Auu____166571) ->
        ('Auu____166572 -> 'Auu____166571 -> 'Auu____166573) ->
          ('Auu____166574 -> 'Auu____166573 -> 'Auu____166575) ->
            ('Auu____166576 ->
               'Auu____166575 ->
                 (FStar_Reflection_Types.name Prims.list * (Prims.int
                   Prims.list * (Prims.string Prims.list * Prims.bool
                   Prims.list))))
              ->
              ('Auu____166568 * 'Auu____166570 * 'Auu____166572 *
                'Auu____166574 * 'Auu____166576) ->
                Data_Serialize_Types.serialized ->
                  Data_Serialize_Types.serialized
  =
  fun x43 ->
    fun x44 ->
      fun x45 ->
        fun x46 ->
          fun x47 ->
            fun x48 ->
              fun x49 ->
                match x48 with
                | (x50, x51, x52, x53, x54) ->
                    let uu____166735 =
                      x47 x54 (x46 x53 (x45 x52 (x44 x51 (x43 x50 x49)))) in
                    (match uu____166735 with
                     | (names, (ints, r)) ->
                         (names, ((Prims.int_zero :: ints), r)))
let rec tuple5_serialize_encode :
  'Auu____166921 'Auu____166922 'Auu____166923 'Auu____166924 'Auu____166925
    'Auu____166926 'Auu____166927 'Auu____166928 'Auu____166929 .
    ('Auu____166921 -> Data_Serialize_Types.serialized -> 'Auu____166922) ->
      ('Auu____166923 -> 'Auu____166922 -> 'Auu____166924) ->
        ('Auu____166925 -> 'Auu____166924 -> 'Auu____166926) ->
          ('Auu____166927 -> 'Auu____166926 -> 'Auu____166928) ->
            ('Auu____166929 ->
               'Auu____166928 ->
                 (FStar_Reflection_Types.name Prims.list * (Prims.int
                   Prims.list * (Prims.string Prims.list * Prims.bool
                   Prims.list))))
              ->
              ('Auu____166921 * 'Auu____166923 * 'Auu____166925 *
                'Auu____166927 * 'Auu____166929) ->
                Data_Serialize_Types.serialized
  =
  fun x36 ->
    fun x37 ->
      fun x38 ->
        fun x39 ->
          fun x40 ->
            fun x41 ->
              match x41 with
              | (x50, x51, x52, x53, x54) ->
                  let uu____167083 =
                    x40 x54
                      (x39 x53
                         (x38 x52
                            (x37 x51
                               (x36 x50
                                  Data_Serialize_Helpers_Serialized.emptySerialized)))) in
                  (match uu____167083 with
                   | (names, (ints, r)) ->
                       (names, ((Prims.int_zero :: ints), r)))
let tuple5_hasSerialize :
  'Ax55 'Ax56 'Ax57 'Ax58 'Ax59 .
    'Ax55 Data_Serialize_Typeclasses.hasSerialize ->
      'Ax56 Data_Serialize_Typeclasses.hasSerialize ->
        'Ax57 Data_Serialize_Typeclasses.hasSerialize ->
          'Ax58 Data_Serialize_Typeclasses.hasSerialize ->
            'Ax59 Data_Serialize_Typeclasses.hasSerialize ->
              ('Ax55 * 'Ax56 * 'Ax57 * 'Ax58 * 'Ax59)
                Data_Serialize_Typeclasses.hasSerialize
  =
  fun x60 ->
    fun x61 ->
      fun x62 ->
        fun x63 ->
          fun x64 ->
            {
              Data_Serialize_Typeclasses.serialize_chainable =
                (tuple5_serialize_encode_chainable
                   (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                      x60)
                   (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                      x61)
                   (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                      x62)
                   (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                      x63)
                   (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                      x64));
              Data_Serialize_Typeclasses.deserialize_chainable =
                (tuple5_serialize_decode_chainable
                   (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                      x60)
                   (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                      x61)
                   (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                      x62)
                   (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                      x63)
                   (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                      x64))
            }
let rec tuple6_serialize_decode_chainable :
  'Auu____167401 'Auu____167402 'Auu____167403 'Auu____167404 'Auu____167405
    'Auu____167406 'Auu____167407 'Auu____167408 'Auu____167409
    'Auu____167410 'Auu____167411 'Auu____167412 .
    ('Auu____167401 -> ('Auu____167402 * 'Auu____167403)) ->
      ('Auu____167404 -> ('Auu____167405 * 'Auu____167401)) ->
        ('Auu____167406 -> ('Auu____167407 * 'Auu____167404)) ->
          ('Auu____167408 -> ('Auu____167409 * 'Auu____167406)) ->
            ('Auu____167410 -> ('Auu____167411 * 'Auu____167408)) ->
              (Data_Serialize_Types.serialized ->
                 ('Auu____167412 * 'Auu____167410))
                ->
                Data_Serialize_Types.serialized ->
                  (('Auu____167402 * 'Auu____167405 * 'Auu____167407 *
                    'Auu____167409 * 'Auu____167411 * 'Auu____167412) *
                    'Auu____167403)
  =
  fun x7 ->
    fun x8 ->
      fun x9 ->
        fun x10 ->
          fun x11 ->
            fun x12 ->
              fun x13 ->
                let x14 =
                  let uu____167548 = x13 in
                  match uu____167548 with
                  | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
                let x15 = FStar_Pervasives_Native.fst x14 in
                let x16 = FStar_Pervasives_Native.snd x14 in
                match x15 with
                | _167643 when _167643 = Prims.int_zero ->
                    let x18 = x12 x16 in
                    let x19 = FStar_Pervasives_Native.fst x18 in
                    let x20 = FStar_Pervasives_Native.snd x18 in
                    let x22 = x11 x20 in
                    let x23 = FStar_Pervasives_Native.fst x22 in
                    let x24 = FStar_Pervasives_Native.snd x22 in
                    let x26 = x10 x24 in
                    let x27 = FStar_Pervasives_Native.fst x26 in
                    let x28 = FStar_Pervasives_Native.snd x26 in
                    let x30 = x9 x28 in
                    let x31 = FStar_Pervasives_Native.fst x30 in
                    let x32 = FStar_Pervasives_Native.snd x30 in
                    let x34 = x8 x32 in
                    let x35 = FStar_Pervasives_Native.fst x34 in
                    let x36 = FStar_Pervasives_Native.snd x34 in
                    let x38 = x7 x36 in
                    let x39 = FStar_Pervasives_Native.fst x38 in
                    let x40 = FStar_Pervasives_Native.snd x38 in
                    ((x39, x35, x31, x27, x23, x19), x40)
                | x41 -> Data_Serialize_Helpers.mkerror "mkMatchInt"
let rec tuple6_serialize_decode :
  'Auu____167820 'Auu____167821 'Auu____167822 'Auu____167823 'Auu____167824
    'Auu____167825 'Auu____167826 'Auu____167827 'Auu____167828
    'Auu____167829 'Auu____167830 'Auu____167831 .
    ('Auu____167820 -> ('Auu____167821 * 'Auu____167822)) ->
      ('Auu____167823 -> ('Auu____167824 * 'Auu____167820)) ->
        ('Auu____167825 -> ('Auu____167826 * 'Auu____167823)) ->
          ('Auu____167827 -> ('Auu____167828 * 'Auu____167825)) ->
            ('Auu____167829 -> ('Auu____167830 * 'Auu____167827)) ->
              (Data_Serialize_Types.serialized ->
                 ('Auu____167831 * 'Auu____167829))
                ->
                Data_Serialize_Types.serialized ->
                  ('Auu____167821 * 'Auu____167824 * 'Auu____167826 *
                    'Auu____167828 * 'Auu____167830 * 'Auu____167831)
  =
  fun x0 ->
    fun x1 ->
      fun x2 ->
        fun x3 ->
          fun x4 ->
            fun x5 ->
              fun x6 ->
                FStar_Pervasives_Native.fst
                  (let x14 =
                     let uu____167996 = x6 in
                     match uu____167996 with
                     | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))) in
                   let x15 = FStar_Pervasives_Native.fst x14 in
                   let x16 = FStar_Pervasives_Native.snd x14 in
                   match x15 with
                   | _168091 when _168091 = Prims.int_zero ->
                       let x18 = x5 x16 in
                       let x19 = FStar_Pervasives_Native.fst x18 in
                       let x20 = FStar_Pervasives_Native.snd x18 in
                       let x22 = x4 x20 in
                       let x23 = FStar_Pervasives_Native.fst x22 in
                       let x24 = FStar_Pervasives_Native.snd x22 in
                       let x26 = x3 x24 in
                       let x27 = FStar_Pervasives_Native.fst x26 in
                       let x28 = FStar_Pervasives_Native.snd x26 in
                       let x30 = x2 x28 in
                       let x31 = FStar_Pervasives_Native.fst x30 in
                       let x32 = FStar_Pervasives_Native.snd x30 in
                       let x34 = x1 x32 in
                       let x35 = FStar_Pervasives_Native.fst x34 in
                       let x36 = FStar_Pervasives_Native.snd x34 in
                       let x38 = x0 x36 in
                       let x39 = FStar_Pervasives_Native.fst x38 in
                       let x40 = FStar_Pervasives_Native.snd x38 in
                       ((x39, x35, x31, x27, x23, x19), x40)
                   | x41 -> Data_Serialize_Helpers.mkerror "mkMatchInt")
let rec tuple6_serialize_encode_chainable :
  'Auu____168256 'Auu____168257 'Auu____168258 'Auu____168259 'Auu____168260
    'Auu____168261 'Auu____168262 'Auu____168263 'Auu____168264
    'Auu____168265 'Auu____168266 .
    ('Auu____168256 -> Data_Serialize_Types.serialized -> 'Auu____168257) ->
      ('Auu____168258 -> 'Auu____168257 -> 'Auu____168259) ->
        ('Auu____168260 -> 'Auu____168259 -> 'Auu____168261) ->
          ('Auu____168262 -> 'Auu____168261 -> 'Auu____168263) ->
            ('Auu____168264 -> 'Auu____168263 -> 'Auu____168265) ->
              ('Auu____168266 ->
                 'Auu____168265 ->
                   (FStar_Reflection_Types.name Prims.list * (Prims.int
                     Prims.list * (Prims.string Prims.list * Prims.bool
                     Prims.list))))
                ->
                ('Auu____168256 * 'Auu____168258 * 'Auu____168260 *
                  'Auu____168262 * 'Auu____168264 * 'Auu____168266) ->
                  Data_Serialize_Types.serialized ->
                    Data_Serialize_Types.serialized
  =
  fun x50 ->
    fun x51 ->
      fun x52 ->
        fun x53 ->
          fun x54 ->
            fun x55 ->
              fun x56 ->
                fun x57 ->
                  match x56 with
                  | (x58, x59, x60, x61, x62, x63) ->
                      let uu____168445 =
                        x55 x63
                          (x54 x62
                             (x53 x61 (x52 x60 (x51 x59 (x50 x58 x57))))) in
                      (match uu____168445 with
                       | (names, (ints, r)) ->
                           (names, ((Prims.int_zero :: ints), r)))
let rec tuple6_serialize_encode :
  'Auu____168645 'Auu____168646 'Auu____168647 'Auu____168648 'Auu____168649
    'Auu____168650 'Auu____168651 'Auu____168652 'Auu____168653
    'Auu____168654 'Auu____168655 .
    ('Auu____168645 -> Data_Serialize_Types.serialized -> 'Auu____168646) ->
      ('Auu____168647 -> 'Auu____168646 -> 'Auu____168648) ->
        ('Auu____168649 -> 'Auu____168648 -> 'Auu____168650) ->
          ('Auu____168651 -> 'Auu____168650 -> 'Auu____168652) ->
            ('Auu____168653 -> 'Auu____168652 -> 'Auu____168654) ->
              ('Auu____168655 ->
                 'Auu____168654 ->
                   (FStar_Reflection_Types.name Prims.list * (Prims.int
                     Prims.list * (Prims.string Prims.list * Prims.bool
                     Prims.list))))
                ->
                ('Auu____168645 * 'Auu____168647 * 'Auu____168649 *
                  'Auu____168651 * 'Auu____168653 * 'Auu____168655) ->
                  Data_Serialize_Types.serialized
  =
  fun x42 ->
    fun x43 ->
      fun x44 ->
        fun x45 ->
          fun x46 ->
            fun x47 ->
              fun x48 ->
                match x48 with
                | (x58, x59, x60, x61, x62, x63) ->
                    let uu____168829 =
                      x47 x63
                        (x46 x62
                           (x45 x61
                              (x44 x60
                                 (x43 x59
                                    (x42 x58
                                       Data_Serialize_Helpers_Serialized.emptySerialized))))) in
                    (match uu____168829 with
                     | (names, (ints, r)) ->
                         (names, ((Prims.int_zero :: ints), r)))
let tuple6_hasSerialize :
  'Ax64 'Ax65 'Ax66 'Ax67 'Ax68 'Ax69 .
    'Ax64 Data_Serialize_Typeclasses.hasSerialize ->
      'Ax65 Data_Serialize_Typeclasses.hasSerialize ->
        'Ax66 Data_Serialize_Typeclasses.hasSerialize ->
          'Ax67 Data_Serialize_Typeclasses.hasSerialize ->
            'Ax68 Data_Serialize_Typeclasses.hasSerialize ->
              'Ax69 Data_Serialize_Typeclasses.hasSerialize ->
                ('Ax64 * 'Ax65 * 'Ax66 * 'Ax67 * 'Ax68 * 'Ax69)
                  Data_Serialize_Typeclasses.hasSerialize
  =
  fun x70 ->
    fun x71 ->
      fun x72 ->
        fun x73 ->
          fun x74 ->
            fun x75 ->
              {
                Data_Serialize_Typeclasses.serialize_chainable =
                  (tuple6_serialize_encode_chainable
                     (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                        x70)
                     (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                        x71)
                     (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                        x72)
                     (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                        x73)
                     (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                        x74)
                     (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__serialize_chainable
                        x75));
                Data_Serialize_Typeclasses.deserialize_chainable =
                  (tuple6_serialize_decode_chainable
                     (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                        x70)
                     (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                        x71)
                     (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                        x72)
                     (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                        x73)
                     (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                        x74)
                     (Data_Serialize_Typeclasses.__proj__MkhasSerialize__item__deserialize_chainable
                        x75))
              }
let (nat_serialize_decode_chainable :
  Data_Serialize_Types.serialized ->
    (Prims.nat * Data_Serialize_Types.serialized))
  =
  fun s ->
    match int_serialize_decode_chainable s with
    | (i, s1) ->
        if i >= Prims.int_zero
        then (i, s1)
        else
          Data_Serialize_Helpers.mkerror
            "nat_serialize_decode: got an negative integer"
let (nat_serialize_encode_chainable :
  Prims.nat ->
    (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
      (Prims.string Prims.list * Prims.bool Prims.list))) ->
      Data_Serialize_Types.serialized)
  = int_serialize_encode_chainable
type 'Aa hasSerialize = 'Aa Data_Serialize_Typeclasses.hasSerialize
let serialize :
  'Aa . 'Aa hasSerialize -> 'Aa -> Data_Serialize_Types.serialized =
  fun uu___1_169150 ->
    fun v1 -> Data_Serialize_Typeclasses.serialize uu___1_169150 v1
let deserialize :
  'Aa . 'Aa hasSerialize -> Data_Serialize_Types.serialized -> 'Aa =
  fun uu___2_169175 ->
    fun v1 -> Data_Serialize_Typeclasses.deserialize uu___2_169175 v1
let (generateEncodeSerialize :
  FStar_Reflection_Types.fv ->
    (FStar_Reflection_Data.decls, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  = Data_Serialize_Encode.generateEncodeSerialize
let (generateDecodeSerialize :
  FStar_Reflection_Types.fv ->
    (FStar_Reflection_Data.decls, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  = Data_Serialize_Decode.generateDecodeSerialize