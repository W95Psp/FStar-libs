open Prims
type 'Aa hasSerialize =
  {
  serialize_chainable:
    'Aa -> Data_Serialize_Types.serialized -> Data_Serialize_Types.serialized ;
  deserialize_chainable:
    Data_Serialize_Types.serialized ->
      ('Aa * Data_Serialize_Types.serialized)
    }
let __proj__MkhasSerialize__item__serialize_chainable :
  'Aa .
    'Aa hasSerialize ->
      'Aa ->
        Data_Serialize_Types.serialized -> Data_Serialize_Types.serialized
  =
  fun projectee ->
    match projectee with
    | { serialize_chainable; deserialize_chainable;_} -> serialize_chainable
let __proj__MkhasSerialize__item__deserialize_chainable :
  'Aa .
    'Aa hasSerialize ->
      Data_Serialize_Types.serialized ->
        ('Aa * Data_Serialize_Types.serialized)
  =
  fun projectee ->
    match projectee with
    | { serialize_chainable; deserialize_chainable;_} ->
        deserialize_chainable
let serialize_chainable :
  'Aa .
    'Aa hasSerialize ->
      'Aa ->
        Data_Serialize_Types.serialized -> Data_Serialize_Types.serialized
  = fun d -> __proj__MkhasSerialize__item__serialize_chainable d
let deserialize_chainable :
  'Aa .
    'Aa hasSerialize ->
      Data_Serialize_Types.serialized ->
        ('Aa * Data_Serialize_Types.serialized)
  = fun d -> __proj__MkhasSerialize__item__deserialize_chainable d
let (intHasSerialize : Prims.int hasSerialize) =
  {
    serialize_chainable =
      (fun i ->
         fun x ->
           let uu____29775 = x in
           match uu____29775 with
           | (names, (ints, r)) -> (names, ((i :: ints), r)));
    deserialize_chainable =
      (fun x ->
         let uu____29893 = x in
         match uu____29893 with | (ns, (i::tlI, o)) -> (i, (ns, (tlI, o))))
  }
let listIntHasSerialize :
  'Aa . 'Aa hasSerialize -> 'Aa Prims.list hasSerialize =
  fun i ->
    {
      serialize_chainable =
        (Data_Serialize_Helpers_Serialized.appendList (serialize_chainable i));
      deserialize_chainable =
        (Data_Serialize_Helpers_Serialized.readList (deserialize_chainable i))
    }
let (xx : Data_Serialize_Types.serialized -> Data_Serialize_Types.serialized)
  =
  serialize_chainable (listIntHasSerialize intHasSerialize)
    [Prims.int_one; (Prims.of_int (3))]
let (makeHasSerializeInstance :
  Data_Serialize_Types.inductiveSumup ->
    FStar_Reflection_Types.fv ->
      FStar_Reflection_Types.fv ->
        (FStar_Reflection_Data.decls, unit)
          FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun s ->
    fun encode ->
      fun decode ->
        fun ps ->
          match match (Data_Serialize_Helpers.nameCurMod'
                         s.Data_Serialize_Types.iName
                         (fun x -> Prims.strcat x "_hasSerialize"))
                        (FStar_Tactics_Types.incr_depth
                           (FStar_Tactics_Types.set_proofstate_range
                              (FStar_Tactics_Types.incr_depth
                                 (FStar_Tactics_Types.set_proofstate_range ps
                                    (FStar_Range.prims_to_fstar_range
                                       (Prims.mk_range
                                          "Data.Serialize.Typeclasses.fst"
                                          (Prims.of_int (37))
                                          (Prims.of_int (15))
                                          (Prims.of_int (37))
                                          (Prims.of_int (75))))))
                              (FStar_Range.prims_to_fstar_range
                                 (Prims.mk_range
                                    "Data.Serialize.Typeclasses.fst"
                                    (Prims.of_int (37)) (Prims.of_int (23))
                                    (Prims.of_int (37)) (Prims.of_int (75))))))
                with
                | FStar_Tactics_Result.Success (a, ps') ->
                    (match () with
                     | () ->
                         FStar_Tactics_Result.Success
                           ((FStar_Reflection_Basic.pack_fv a),
                             (FStar_Tactics_Types.decr_depth
                                (FStar_Tactics_Types.set_proofstate_range ps'
                                   (FStar_Range.prims_to_fstar_range
                                      (Prims.mk_range
                                         "Data.Serialize.Typeclasses.fst"
                                         (Prims.of_int (37))
                                         (Prims.of_int (15))
                                         (Prims.of_int (37))
                                         (Prims.of_int (75))))))))
                | FStar_Tactics_Result.Failed (e, ps') ->
                    FStar_Tactics_Result.Failed (e, ps')
          with
          | FStar_Tactics_Result.Success (a, ps') ->
              (match () with
               | () ->
                   (match () with
                    | () ->
                        ((match s with
                          | { Data_Serialize_Types.iName = iName;
                              Data_Serialize_Types.iVars = iVars;
                              Data_Serialize_Types.iCons = iCons;_} ->
                              (fun ps1 ->
                                 match (FStar_Tactics_Util.map
                                          (fun uu____30580 ->
                                             fun ps2 ->
                                               match (FStar_Tactics_Derived.fresh_bv
                                                        (FStar_Reflection_Basic.pack_ln
                                                           (FStar_Reflection_Data.Tv_Type
                                                              ())))
                                                       (FStar_Tactics_Types.incr_depth
                                                          (FStar_Tactics_Types.set_proofstate_range
                                                             ps2
                                                             (FStar_Range.prims_to_fstar_range
                                                                (Prims.mk_range
                                                                   "Data.Serialize.Typeclasses.fst"
                                                                   (Prims.of_int (39))
                                                                   (Prims.of_int (42))
                                                                   (Prims.of_int (39))
                                                                   (Prims.of_int (60))))))
                                               with
                                               | FStar_Tactics_Result.Success
                                                   (a1, ps'1) ->
                                                   (match () with
                                                    | () ->
                                                        FStar_Tactics_Result.Success
                                                          ((FStar_Reflection_Basic.pack_binder
                                                              a1
                                                              FStar_Reflection_Data.Q_Explicit),
                                                            (FStar_Tactics_Types.decr_depth
                                                               (FStar_Tactics_Types.set_proofstate_range
                                                                  ps'1
                                                                  (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (39))
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (39))
                                                                    (Prims.of_int (71))))))))
                                               | FStar_Tactics_Result.Failed
                                                   (e, ps'1) ->
                                                   FStar_Tactics_Result.Failed
                                                     (e, ps'1))
                                          (Data_Serialize_Helpers.mkList
                                             Prims.int_zero
                                             (s.Data_Serialize_Types.iVars -
                                                Prims.int_one)))
                                         (FStar_Tactics_Types.incr_depth
                                            (FStar_Tactics_Types.set_proofstate_range
                                               ps1
                                               (FStar_Range.prims_to_fstar_range
                                                  (Prims.mk_range
                                                     "Data.Serialize.Typeclasses.fst"
                                                     (Prims.of_int (39))
                                                     (Prims.of_int (16))
                                                     (Prims.of_int (39))
                                                     (Prims.of_int (95))))))
                                 with
                                 | FStar_Tactics_Result.Success (a1, ps'1) ->
                                     (match () with
                                      | () ->
                                          (match (FStar_Tactics_Util.map
                                                    (fun t ->
                                                       fun ps2 ->
                                                         match match 
                                                                 match 
                                                                   (FStar_Tactics_Derived.binder_to_term
                                                                    t)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (43))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (15))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (44))
                                                                    (Prims.of_int (16))
                                                                    (Prims.of_int (46))
                                                                    (Prims.of_int (17))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (18))
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (36))))))
                                                                 with
                                                                 | FStar_Tactics_Result.Success
                                                                    (a2,
                                                                    ps'2) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Data";
                                                                    "Serialize";
                                                                    "Typeclasses";
                                                                    "hasSerialize"]))),
                                                                    (a2,
                                                                    FStar_Reflection_Data.Q_Explicit))))
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (37)))))))
                                                                 | FStar_Tactics_Result.Failed
                                                                    (e, ps'2)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'2)
                                                               with
                                                               | FStar_Tactics_Result.Success
                                                                   (a2, ps'2)
                                                                   ->
                                                                   (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (FStar_Tactics_Derived.fresh_bv
                                                                    a2)
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (43))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (15)))))))
                                                               | FStar_Tactics_Result.Failed
                                                                   (e, ps'2)
                                                                   ->
                                                                   FStar_Tactics_Result.Failed
                                                                    (e, ps'2)
                                                         with
                                                         | FStar_Tactics_Result.Success
                                                             (a2, ps'2) ->
                                                             (match () with
                                                              | () ->
                                                                  FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Basic.pack_binder
                                                                    a2
                                                                    (FStar_Reflection_Data.Q_Meta
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["FStar";
                                                                    "Tactics";
                                                                    "Typeclasses";
                                                                    "tcresolve"]))))),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (42))
                                                                    (Prims.of_int (12))
                                                                    (Prims.of_int (48))
                                                                    (Prims.of_int (35))))))))
                                                         | FStar_Tactics_Result.Failed
                                                             (e, ps'2) ->
                                                             FStar_Tactics_Result.Failed
                                                               (e, ps'2)) a1)
                                                   (FStar_Tactics_Types.incr_depth
                                                      (FStar_Tactics_Types.set_proofstate_range
                                                         (FStar_Tactics_Types.decr_depth
                                                            (FStar_Tactics_Types.set_proofstate_range
                                                               ps'1
                                                               (FStar_Range.prims_to_fstar_range
                                                                  (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (40))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (65))
                                                                    (Prims.of_int (8))))))
                                                         (FStar_Range.prims_to_fstar_range
                                                            (Prims.mk_range
                                                               "Data.Serialize.Typeclasses.fst"
                                                               (Prims.of_int (41))
                                                               (Prims.of_int (8))
                                                               (Prims.of_int (49))
                                                               (Prims.of_int (15))))))
                                           with
                                           | FStar_Tactics_Result.Success
                                               (a2, ps'2) ->
                                               (match () with
                                                | () ->
                                                    (match match match 
                                                                   match 
                                                                    match 
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    encode))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (65))
                                                                    (Prims.of_int (8))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (51))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (9))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (52))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (9))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (53))
                                                                    (Prims.of_int (10))
                                                                    (Prims.of_int (60))
                                                                    (Prims.of_int (11))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (54))
                                                                    (Prims.of_int (12))
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (25))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (54))
                                                                    (Prims.of_int (22))
                                                                    (Prims.of_int (54))
                                                                    (Prims.of_int (45))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a3,
                                                                    ps'3) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    (FStar_Tactics_Util.map
                                                                    (fun i ->
                                                                    fun ps2
                                                                    ->
                                                                    match 
                                                                    (FStar_Tactics_Derived.binder_to_term
                                                                    i)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (55))
                                                                    (Prims.of_int (75))
                                                                    (Prims.of_int (55))
                                                                    (Prims.of_int (93))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a4,
                                                                    ps'4) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Data";
                                                                    "Serialize";
                                                                    "Typeclasses";
                                                                    "__proj__MkhasSerialize__item__serialize_chainable"]))),
                                                                    (a4,
                                                                    FStar_Reflection_Data.Q_Explicit))))
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (37)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4))
                                                                    a2)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (54))
                                                                    (Prims.of_int (12))
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (25))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (54))
                                                                    (Prims.of_int (46))
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (24))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a4,
                                                                    ps'4) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Derived.mk_e_app
                                                                    a3 a4),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (54))
                                                                    (Prims.of_int (12))
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (25))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'3)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'3)
                                                                   with
                                                                   | 
                                                                   FStar_Tactics_Result.Success
                                                                    (a3,
                                                                    ps'3) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    match 
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    decode))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (53))
                                                                    (Prims.of_int (10))
                                                                    (Prims.of_int (60))
                                                                    (Prims.of_int (11))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (57))
                                                                    (Prims.of_int (12))
                                                                    (Prims.of_int (59))
                                                                    (Prims.of_int (25))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (57))
                                                                    (Prims.of_int (22))
                                                                    (Prims.of_int (57))
                                                                    (Prims.of_int (45))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a4,
                                                                    ps'4) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    (FStar_Tactics_Util.map
                                                                    (fun i ->
                                                                    fun ps2
                                                                    ->
                                                                    match 
                                                                    (FStar_Tactics_Derived.binder_to_term
                                                                    i)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (58))
                                                                    (Prims.of_int (75))
                                                                    (Prims.of_int (58))
                                                                    (Prims.of_int (93))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a5,
                                                                    ps'5) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Data";
                                                                    "Serialize";
                                                                    "Typeclasses";
                                                                    "__proj__MkhasSerialize__item__deserialize_chainable"]))),
                                                                    (a5,
                                                                    FStar_Reflection_Data.Q_Explicit))))
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (37)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5))
                                                                    a2)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (57))
                                                                    (Prims.of_int (12))
                                                                    (Prims.of_int (59))
                                                                    (Prims.of_int (25))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (57))
                                                                    (Prims.of_int (46))
                                                                    (Prims.of_int (59))
                                                                    (Prims.of_int (24))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a5,
                                                                    ps'5) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Derived.mk_e_app
                                                                    a4 a5),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (57))
                                                                    (Prims.of_int (12))
                                                                    (Prims.of_int (59))
                                                                    (Prims.of_int (25))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a4,
                                                                    ps'4) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Data";
                                                                    "Serialize";
                                                                    "Typeclasses";
                                                                    "MkhasSerialize"]))),
                                                                    (a3,
                                                                    FStar_Reflection_Data.Q_Explicit))))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (91))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (91))
                                                                    (Prims.of_int (29))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (91))
                                                                    (Prims.of_int (10))
                                                                    (Prims.of_int (91))
                                                                    (Prims.of_int (24))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a5,
                                                                    ps'5) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    (a5,
                                                                    (a4,
                                                                    FStar_Reflection_Data.Q_Explicit))))
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (37)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)))
                                                                   | 
                                                                   FStar_Tactics_Result.Failed
                                                                    (e, ps'3)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'3)
                                                                 with
                                                                 | FStar_Tactics_Result.Success
                                                                    (a3,
                                                                    ps'3) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (Data_Serialize_Helpers.mk_abs
                                                                    (FStar_List_Tot_Base.append
                                                                    a1 a2) a3)
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (52))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (9)))))))
                                                                 | FStar_Tactics_Result.Failed
                                                                    (e, ps'3)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'3)
                                                           with
                                                           | FStar_Tactics_Result.Success
                                                               (a3, ps'3) ->
                                                               (match () with
                                                                | () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Data.Sg_Let
                                                                    (false,
                                                                    a, [],
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    FStar_Reflection_Data.Tv_Unknown),
                                                                    a3)),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (51))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (9))))))))
                                                           | FStar_Tactics_Result.Failed
                                                               (e, ps'3) ->
                                                               FStar_Tactics_Result.Failed
                                                                 (e, ps'3)
                                                     with
                                                     | FStar_Tactics_Result.Success
                                                         (a3, ps'3) ->
                                                         (match () with
                                                          | () ->
                                                              (match () with
                                                               | () ->
                                                                   (match 
                                                                    match 
                                                                    match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ([
                                                                    Obj.magic
                                                                    (failwith
                                                                    "Cannot evaluate open quotation at runtime")],
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (63))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (65))
                                                                    (Prims.of_int (8))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (63))
                                                                    (Prims.of_int (13))
                                                                    (Prims.of_int (63))
                                                                    (Prims.of_int (27))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (64))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (65))
                                                                    (Prims.of_int (8))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (64))
                                                                    (Prims.of_int (13))
                                                                    (Prims.of_int (64))
                                                                    (Prims.of_int (51))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (64))
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (64))
                                                                    (Prims.of_int (48))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (64))
                                                                    (Prims.of_int (31))
                                                                    (Prims.of_int (64))
                                                                    (Prims.of_int (47))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (64))
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (64))
                                                                    (Prims.of_int (48)))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a4,
                                                                    ps'4) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Basic.set_sigelt_attrs
                                                                    a4
                                                                    (FStar_Reflection_Basic.pack_sigelt
                                                                    a3)),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (64))
                                                                    (Prims.of_int (13))
                                                                    (Prims.of_int (64))
                                                                    (Prims.of_int (51))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a4,
                                                                    ps'4) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ([a4],
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (65))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (65))
                                                                    (Prims.of_int (8))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4))))
                                                     | FStar_Tactics_Result.Failed
                                                         (e, ps'3) ->
                                                         FStar_Tactics_Result.Failed
                                                           (e, ps'3)))
                                           | FStar_Tactics_Result.Failed
                                               (e, ps'2) ->
                                               FStar_Tactics_Result.Failed
                                                 (e, ps'2)))
                                 | FStar_Tactics_Result.Failed (e, ps'1) ->
                                     FStar_Tactics_Result.Failed (e, ps'1))))
                          (FStar_Tactics_Types.decr_depth
                             (FStar_Tactics_Types.set_proofstate_range
                                (FStar_Tactics_Types.incr_depth
                                   (FStar_Tactics_Types.set_proofstate_range
                                      (FStar_Tactics_Types.decr_depth
                                         (FStar_Tactics_Types.set_proofstate_range
                                            ps'
                                            (FStar_Range.prims_to_fstar_range
                                               (Prims.mk_range
                                                  "Data.Serialize.Typeclasses.fst"
                                                  (Prims.of_int (38))
                                                  (Prims.of_int (4))
                                                  (Prims.of_int (65))
                                                  (Prims.of_int (8))))))
                                      (FStar_Range.prims_to_fstar_range
                                         (Prims.mk_range
                                            "Data.Serialize.Typeclasses.fst"
                                            (Prims.of_int (38))
                                            (Prims.of_int (32))
                                            (Prims.of_int (38))
                                            (Prims.of_int (33))))))
                                (FStar_Range.prims_to_fstar_range
                                   (Prims.mk_range
                                      "Data.Serialize.Typeclasses.fst"
                                      (Prims.of_int (38)) (Prims.of_int (4))
                                      (Prims.of_int (65)) (Prims.of_int (8))))))))
          | FStar_Tactics_Result.Failed (e, ps') ->
              FStar_Tactics_Result.Failed (e, ps')
let (generateSerialize' :
  FStar_Reflection_Types.fv ->
    (FStar_Reflection_Types.sigelt Prims.list, unit)
      FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun tfv ->
    fun ps ->
      match (Data_Serialize_Rep.makeGenericRep tfv)
              (FStar_Tactics_Types.incr_depth
                 (FStar_Tactics_Types.set_proofstate_range ps
                    (FStar_Range.prims_to_fstar_range
                       (Prims.mk_range "Data.Serialize.Typeclasses.fst"
                          (Prims.of_int (69)) (Prims.of_int (14))
                          (Prims.of_int (69)) (Prims.of_int (32))))))
      with
      | FStar_Tactics_Result.Success (a, ps') ->
          (match () with
           | () ->
               (match (Data_Serialize_Decode.generateDecodeSerialize tfv)
                        (FStar_Tactics_Types.incr_depth
                           (FStar_Tactics_Types.set_proofstate_range
                              (FStar_Tactics_Types.decr_depth
                                 (FStar_Tactics_Types.set_proofstate_range
                                    ps'
                                    (FStar_Range.prims_to_fstar_range
                                       (Prims.mk_range
                                          "Data.Serialize.Typeclasses.fst"
                                          (Prims.of_int (70))
                                          (Prims.of_int (6))
                                          (Prims.of_int (76))
                                          (Prims.of_int (7))))))
                              (FStar_Range.prims_to_fstar_range
                                 (Prims.mk_range
                                    "Data.Serialize.Typeclasses.fst"
                                    (Prims.of_int (70)) (Prims.of_int (6))
                                    (Prims.of_int (70)) (Prims.of_int (37))))))
                with
                | FStar_Tactics_Result.Success (a1, ps'1) ->
                    (match () with
                     | () ->
                         (match match (Data_Serialize_Encode.generateEncodeSerialize
                                         tfv)
                                        (FStar_Tactics_Types.incr_depth
                                           (FStar_Tactics_Types.set_proofstate_range
                                              (FStar_Tactics_Types.incr_depth
                                                 (FStar_Tactics_Types.set_proofstate_range
                                                    (FStar_Tactics_Types.decr_depth
                                                       (FStar_Tactics_Types.set_proofstate_range
                                                          ps'1
                                                          (FStar_Range.prims_to_fstar_range
                                                             (Prims.mk_range
                                                                "Data.Serialize.Typeclasses.fst"
                                                                (Prims.of_int (70))
                                                                (Prims.of_int (6))
                                                                (Prims.of_int (76))
                                                                (Prims.of_int (7))))))
                                                    (FStar_Range.prims_to_fstar_range
                                                       (Prims.mk_range
                                                          "Data.Serialize.Typeclasses.fst"
                                                          (Prims.of_int (71))
                                                          (Prims.of_int (6))
                                                          (Prims.of_int (76))
                                                          (Prims.of_int (7))))))
                                              (FStar_Range.prims_to_fstar_range
                                                 (Prims.mk_range
                                                    "Data.Serialize.Typeclasses.fst"
                                                    (Prims.of_int (71))
                                                    (Prims.of_int (6))
                                                    (Prims.of_int (71))
                                                    (Prims.of_int (37))))))
                                with
                                | FStar_Tactics_Result.Success (a2, ps'2) ->
                                    (match () with
                                     | () ->
                                         (match match match (Data_Serialize_Encode.transform_name_encode'
                                                               (FStar_Reflection_Basic.inspect_fv
                                                                  tfv))
                                                              (FStar_Tactics_Types.incr_depth
                                                                 (FStar_Tactics_Types.set_proofstate_range
                                                                    (
                                                                    FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (71))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (76))
                                                                    (Prims.of_int (7))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (72))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (76))
                                                                    (Prims.of_int (7))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (61))))))
                                                                    (
                                                                    FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (17))
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (60))))))
                                                      with
                                                      | FStar_Tactics_Result.Success
                                                          (a3, ps'3) ->
                                                          (match () with
                                                           | () ->
                                                               FStar_Tactics_Result.Success
                                                                 ((FStar_Reflection_Basic.pack_fv
                                                                    a3),
                                                                   (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (61))))))))
                                                      | FStar_Tactics_Result.Failed
                                                          (e, ps'3) ->
                                                          FStar_Tactics_Result.Failed
                                                            (e, ps'3)
                                                with
                                                | FStar_Tactics_Result.Success
                                                    (a3, ps'3) ->
                                                    (match () with
                                                     | () ->
                                                         (match match 
                                                                  (Data_Serialize_Decode.transform_name_decode'
                                                                    (FStar_Reflection_Basic.inspect_fv
                                                                    tfv))
                                                                    (
                                                                    FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (72))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (76))
                                                                    (Prims.of_int (7))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (75))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (75))
                                                                    (Prims.of_int (61))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (75))
                                                                    (Prims.of_int (17))
                                                                    (Prims.of_int (75))
                                                                    (Prims.of_int (60))))))
                                                                with
                                                                | FStar_Tactics_Result.Success
                                                                    (a4,
                                                                    ps'4) ->
                                                                    (
                                                                    match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Basic.pack_fv
                                                                    a4),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (75))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (75))
                                                                    (Prims.of_int (61))))))))
                                                                | FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                          with
                                                          | FStar_Tactics_Result.Success
                                                              (a4, ps'4) ->
                                                              (match () with
                                                               | () ->
                                                                   (makeHasSerializeInstance
                                                                    a a3 a4)
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Typeclasses.fst"
                                                                    (Prims.of_int (72))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (76))
                                                                    (Prims.of_int (7)))))))
                                                          | FStar_Tactics_Result.Failed
                                                              (e, ps'4) ->
                                                              FStar_Tactics_Result.Failed
                                                                (e, ps'4)))
                                                | FStar_Tactics_Result.Failed
                                                    (e, ps'3) ->
                                                    FStar_Tactics_Result.Failed
                                                      (e, ps'3)
                                          with
                                          | FStar_Tactics_Result.Success
                                              (a3, ps'3) ->
                                              (match () with
                                               | () ->
                                                   FStar_Tactics_Result.Success
                                                     ((FStar_List_Tot_Base.append
                                                         a2 a3),
                                                       (FStar_Tactics_Types.decr_depth
                                                          (FStar_Tactics_Types.set_proofstate_range
                                                             ps'3
                                                             (FStar_Range.prims_to_fstar_range
                                                                (Prims.mk_range
                                                                   "Data.Serialize.Typeclasses.fst"
                                                                   (Prims.of_int (71))
                                                                   (Prims.of_int (6))
                                                                   (Prims.of_int (76))
                                                                   (Prims.of_int (7))))))))
                                          | FStar_Tactics_Result.Failed
                                              (e, ps'3) ->
                                              FStar_Tactics_Result.Failed
                                                (e, ps'3)))
                                | FStar_Tactics_Result.Failed (e, ps'2) ->
                                    FStar_Tactics_Result.Failed (e, ps'2)
                          with
                          | FStar_Tactics_Result.Success (a2, ps'2) ->
                              (match () with
                               | () ->
                                   FStar_Tactics_Result.Success
                                     ((FStar_List_Tot_Base.append a1 a2),
                                       (FStar_Tactics_Types.decr_depth
                                          (FStar_Tactics_Types.set_proofstate_range
                                             ps'2
                                             (FStar_Range.prims_to_fstar_range
                                                (Prims.mk_range
                                                   "Data.Serialize.Typeclasses.fst"
                                                   (Prims.of_int (70))
                                                   (Prims.of_int (6))
                                                   (Prims.of_int (76))
                                                   (Prims.of_int (7))))))))
                          | FStar_Tactics_Result.Failed (e, ps'2) ->
                              FStar_Tactics_Result.Failed (e, ps'2)))
                | FStar_Tactics_Result.Failed (e, ps'1) ->
                    FStar_Tactics_Result.Failed (e, ps'1)))
      | FStar_Tactics_Result.Failed (e, ps') ->
          FStar_Tactics_Result.Failed (e, ps')
let (generateSerialize :
  FStar_Reflection_Types.term ->
    (FStar_Reflection_Types.sigelt Prims.list, unit)
      FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun t ->
    fun ps ->
      match (Data_Serialize_Helpers.fvOf t)
              (FStar_Tactics_Types.incr_depth
                 (FStar_Tactics_Types.set_proofstate_range ps
                    (FStar_Range.prims_to_fstar_range
                       (Prims.mk_range "Data.Serialize.Typeclasses.fst"
                          (Prims.of_int (78)) (Prims.of_int (23))
                          (Prims.of_int (78)) (Prims.of_int (31))))))
      with
      | FStar_Tactics_Result.Success (a, ps') ->
          (match () with
           | () ->
               (generateSerialize' a)
                 (FStar_Tactics_Types.decr_depth
                    (FStar_Tactics_Types.set_proofstate_range ps'
                       (FStar_Range.prims_to_fstar_range
                          (Prims.mk_range "Data.Serialize.Typeclasses.fst"
                             (Prims.of_int (78)) (Prims.of_int (4))
                             (Prims.of_int (78)) (Prims.of_int (31)))))))
      | FStar_Tactics_Result.Failed (e, ps') ->
          FStar_Tactics_Result.Failed (e, ps')
let serialize :
  'Aa . 'Aa hasSerialize -> 'Aa -> Data_Serialize_Types.serialized =
  fun uu___0_31636 ->
    fun v1 -> serialize_chainable uu___0_31636 v1 ([], ([], ([], [])))
let deserialize :
  'Aa . 'Aa hasSerialize -> Data_Serialize_Types.serialized -> 'Aa =
  fun uu___1_31704 ->
    fun v1 ->
      FStar_Pervasives_Native.fst (deserialize_chainable uu___1_31704 v1)