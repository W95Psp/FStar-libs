open Prims
let (transform_name_encode' :
  FStar_Reflection_Types.name ->
    (FStar_Reflection_Types.name, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun n1 ->
    Data_Serialize_Helpers.nameCurMod' n1
      (fun x -> Prims.strcat x "_serialize_encode_chainable")
let (transform_name_encode :
  FStar_Reflection_Types.name ->
    (FStar_Reflection_Types.name, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun n1 ->
    Data_Serialize_Helpers.nameCurMod' n1
      (fun x -> Prims.strcat x "_serialize_encode")
let rec (generateDecodeSerialize_term_for_argSumup :
  FStar_Reflection_Types.binder Prims.list ->
    unit Data_Serialize_Types.argSumup ->
      (FStar_Reflection_Types.term, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun args_fun ->
    fun arg ->
      match arg with
      | Data_Serialize_Types.AS_Int ->
          (fun s ->
             FStar_Tactics_Result.Success
               ((FStar_Reflection_Basic.pack_ln
                   (FStar_Reflection_Data.Tv_FVar
                      (FStar_Reflection_Basic.pack_fv
                         ["Data";
                         "Serialize";
                         "Helpers";
                         "Serialized";
                         "appendInt"]))), s))
      | Data_Serialize_Types.AS_String ->
          (fun s ->
             FStar_Tactics_Result.Success
               ((FStar_Reflection_Basic.pack_ln
                   (FStar_Reflection_Data.Tv_FVar
                      (FStar_Reflection_Basic.pack_fv
                         ["Data";
                         "Serialize";
                         "Helpers";
                         "Serialized";
                         "appendString"]))), s))
      | Data_Serialize_Types.AS_Bool ->
          (fun s ->
             FStar_Tactics_Result.Success
               ((FStar_Reflection_Basic.pack_ln
                   (FStar_Reflection_Data.Tv_FVar
                      (FStar_Reflection_Basic.pack_fv
                         ["Data";
                         "Serialize";
                         "Helpers";
                         "Serialized";
                         "appendBool"]))), s))
      | Data_Serialize_Types.AS_List typ ->
          (fun ps ->
             match (generateDecodeSerialize_term_for_argSumup args_fun typ)
                     (FStar_Tactics_Types.incr_depth
                        (FStar_Tactics_Types.set_proofstate_range ps
                           (FStar_Range.prims_to_fstar_range
                              (Prims.mk_range "Data.Serialize.Encode.fst"
                                 (Prims.of_int (27)) (Prims.of_int (39))
                                 (Prims.of_int (27)) (Prims.of_int (95))))))
             with
             | FStar_Tactics_Result.Success (a, ps') ->
                 (match () with
                  | () ->
                      (FStar_Tactics_Builtins.pack
                         (FStar_Reflection_Data.Tv_App
                            ((FStar_Reflection_Basic.pack_ln
                                (FStar_Reflection_Data.Tv_FVar
                                   (FStar_Reflection_Basic.pack_fv
                                      ["Data";
                                      "Serialize";
                                      "Helpers";
                                      "Serialized";
                                      "appendList"]))),
                              (a, FStar_Reflection_Data.Q_Explicit))))
                        (FStar_Tactics_Types.decr_depth
                           (FStar_Tactics_Types.set_proofstate_range ps'
                              (FStar_Range.prims_to_fstar_range
                                 (Prims.mk_range "Data.Serialize.Helpers.fst"
                                    (Prims.of_int (89)) (Prims.of_int (4))
                                    (Prims.of_int (89)) (Prims.of_int (37)))))))
             | FStar_Tactics_Result.Failed (e, ps') ->
                 FStar_Tactics_Result.Failed (e, ps'))
      | Data_Serialize_Types.AS_TVar i ->
          FStar_Tactics_Derived.binder_to_term
            (FStar_List_Tot_Base.index args_fun i)
      | Data_Serialize_Types.AS_Inductive (tname, args) ->
          (fun ps ->
             match match (transform_name_encode' tname)
                           (FStar_Tactics_Types.incr_depth
                              (FStar_Tactics_Types.set_proofstate_range
                                 (FStar_Tactics_Types.incr_depth
                                    (FStar_Tactics_Types.set_proofstate_range
                                       ps
                                       (FStar_Range.prims_to_fstar_range
                                          (Prims.mk_range
                                             "Data.Serialize.Encode.fst"
                                             (Prims.of_int (30))
                                             (Prims.of_int (12))
                                             (Prims.of_int (30))
                                             (Prims.of_int (55))))))
                                 (FStar_Range.prims_to_fstar_range
                                    (Prims.mk_range
                                       "Data.Serialize.Encode.fst"
                                       (Prims.of_int (30))
                                       (Prims.of_int (25))
                                       (Prims.of_int (30))
                                       (Prims.of_int (55))))))
                   with
                   | FStar_Tactics_Result.Success (a, ps') ->
                       (match () with
                        | () ->
                            (Data_Serialize_Helpers.name_to_term a)
                              (FStar_Tactics_Types.decr_depth
                                 (FStar_Tactics_Types.set_proofstate_range
                                    ps'
                                    (FStar_Range.prims_to_fstar_range
                                       (Prims.mk_range
                                          "Data.Serialize.Encode.fst"
                                          (Prims.of_int (30))
                                          (Prims.of_int (12))
                                          (Prims.of_int (30))
                                          (Prims.of_int (55)))))))
                   | FStar_Tactics_Result.Failed (e, ps') ->
                       FStar_Tactics_Result.Failed (e, ps')
             with
             | FStar_Tactics_Result.Success (a, ps') ->
                 (match () with
                  | () ->
                      (match (Data_Serialize_Helpers.add_admit a)
                               (FStar_Tactics_Types.incr_depth
                                  (FStar_Tactics_Types.set_proofstate_range
                                     (FStar_Tactics_Types.decr_depth
                                        (FStar_Tactics_Types.set_proofstate_range
                                           ps'
                                           (FStar_Range.prims_to_fstar_range
                                              (Prims.mk_range
                                                 "Data.Serialize.Encode.fst"
                                                 (Prims.of_int (31))
                                                 (Prims.of_int (4))
                                                 (Prims.of_int (32))
                                                 (Prims.of_int (78))))))
                                     (FStar_Range.prims_to_fstar_range
                                        (Prims.mk_range
                                           "Data.Serialize.Encode.fst"
                                           (Prims.of_int (31))
                                           (Prims.of_int (12))
                                           (Prims.of_int (31))
                                           (Prims.of_int (23))))))
                       with
                       | FStar_Tactics_Result.Success (a1, ps'1) ->
                           (match () with
                            | () ->
                                (match (FStar_Tactics_Util.map
                                          (generateDecodeSerialize_term_for_argSumup
                                             args_fun) args)
                                         (FStar_Tactics_Types.incr_depth
                                            (FStar_Tactics_Types.set_proofstate_range
                                               (FStar_Tactics_Types.decr_depth
                                                  (FStar_Tactics_Types.set_proofstate_range
                                                     ps'1
                                                     (FStar_Range.prims_to_fstar_range
                                                        (Prims.mk_range
                                                           "Data.Serialize.Encode.fst"
                                                           (Prims.of_int (32))
                                                           (Prims.of_int (4))
                                                           (Prims.of_int (32))
                                                           (Prims.of_int (78))))))
                                               (FStar_Range.prims_to_fstar_range
                                                  (Prims.mk_range
                                                     "Data.Serialize.Encode.fst"
                                                     (Prims.of_int (32))
                                                     (Prims.of_int (15))
                                                     (Prims.of_int (32))
                                                     (Prims.of_int (78))))))
                                 with
                                 | FStar_Tactics_Result.Success (a2, ps'2) ->
                                     (match () with
                                      | () ->
                                          FStar_Tactics_Result.Success
                                            ((FStar_Reflection_Derived.mk_e_app
                                                a1 a2),
                                              (FStar_Tactics_Types.decr_depth
                                                 (FStar_Tactics_Types.set_proofstate_range
                                                    ps'2
                                                    (FStar_Range.prims_to_fstar_range
                                                       (Prims.mk_range
                                                          "Data.Serialize.Encode.fst"
                                                          (Prims.of_int (32))
                                                          (Prims.of_int (4))
                                                          (Prims.of_int (32))
                                                          (Prims.of_int (78))))))))
                                 | FStar_Tactics_Result.Failed (e, ps'2) ->
                                     FStar_Tactics_Result.Failed (e, ps'2)))
                       | FStar_Tactics_Result.Failed (e, ps'1) ->
                           FStar_Tactics_Result.Failed (e, ps'1)))
             | FStar_Tactics_Result.Failed (e, ps') ->
                 FStar_Tactics_Result.Failed (e, ps'))
let (generateEncodeSerialize_term_for_inductiveSumup :
  Data_Serialize_Types.inductiveSumup ->
    (FStar_Reflection_Types.term, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun s ->
    fun ps ->
      match () with
      | () ->
          ((match s with
            | { Data_Serialize_Types.iName = iName;
                Data_Serialize_Types.iVars = iVars;
                Data_Serialize_Types.iCons = iCons;_} ->
                (fun ps1 ->
                   match (FStar_Tactics_Util.map
                            (fun uu____28036 ->
                               fun ps2 ->
                                 match (FStar_Tactics_Derived.fresh_bv
                                          (FStar_Reflection_Basic.pack_ln
                                             FStar_Reflection_Data.Tv_Unknown))
                                         (FStar_Tactics_Types.incr_depth
                                            (FStar_Tactics_Types.set_proofstate_range
                                               ps2
                                               (FStar_Range.prims_to_fstar_range
                                                  (Prims.mk_range
                                                     "Data.Serialize.Encode.fst"
                                                     (Prims.of_int (39))
                                                     (Prims.of_int (44))
                                                     (Prims.of_int (39))
                                                     (Prims.of_int (59))))))
                                 with
                                 | FStar_Tactics_Result.Success (a, ps') ->
                                     (match () with
                                      | () ->
                                          FStar_Tactics_Result.Success
                                            ((FStar_Reflection_Basic.pack_binder
                                                a
                                                FStar_Reflection_Data.Q_Explicit),
                                              (FStar_Tactics_Types.decr_depth
                                                 (FStar_Tactics_Types.set_proofstate_range
                                                    ps'
                                                    (FStar_Range.prims_to_fstar_range
                                                       (Prims.mk_range
                                                          "Data.Serialize.Encode.fst"
                                                          (Prims.of_int (39))
                                                          (Prims.of_int (32))
                                                          (Prims.of_int (39))
                                                          (Prims.of_int (70))))))))
                                 | FStar_Tactics_Result.Failed (e, ps') ->
                                     FStar_Tactics_Result.Failed (e, ps'))
                            (Data_Serialize_Helpers.mkList Prims.int_zero
                               (s.Data_Serialize_Types.iVars - Prims.int_one)))
                           (FStar_Tactics_Types.incr_depth
                              (FStar_Tactics_Types.set_proofstate_range ps1
                                 (FStar_Range.prims_to_fstar_range
                                    (Prims.mk_range
                                       "Data.Serialize.Encode.fst"
                                       (Prims.of_int (39)) (Prims.of_int (8))
                                       (Prims.of_int (39))
                                       (Prims.of_int (94))))))
                   with
                   | FStar_Tactics_Result.Success (a, ps') ->
                       (match () with
                        | () ->
                            (match (FStar_Tactics_Derived.fresh_binder
                                      (FStar_Reflection_Basic.pack_ln
                                         FStar_Reflection_Data.Tv_Unknown))
                                     (FStar_Tactics_Types.incr_depth
                                        (FStar_Tactics_Types.set_proofstate_range
                                           (FStar_Tactics_Types.decr_depth
                                              (FStar_Tactics_Types.set_proofstate_range
                                                 ps'
                                                 (FStar_Range.prims_to_fstar_range
                                                    (Prims.mk_range
                                                       "Data.Serialize.Encode.fst"
                                                       (Prims.of_int (40))
                                                       (Prims.of_int (4))
                                                       (Prims.of_int (62))
                                                       (Prims.of_int (5))))))
                                           (FStar_Range.prims_to_fstar_range
                                              (Prims.mk_range
                                                 "Data.Serialize.Encode.fst"
                                                 (Prims.of_int (40))
                                                 (Prims.of_int (14))
                                                 (Prims.of_int (40))
                                                 (Prims.of_int (31))))))
                             with
                             | FStar_Tactics_Result.Success (a1, ps'1) ->
                                 (match () with
                                  | () ->
                                      (match (FStar_Tactics_Derived.fresh_binder
                                                (FStar_Reflection_Basic.pack_ln
                                                   (FStar_Reflection_Data.Tv_FVar
                                                      (FStar_Reflection_Basic.pack_fv
                                                         ["Data";
                                                         "Serialize";
                                                         "Types";
                                                         "serialized"]))))
                                               (FStar_Tactics_Types.incr_depth
                                                  (FStar_Tactics_Types.set_proofstate_range
                                                     (FStar_Tactics_Types.decr_depth
                                                        (FStar_Tactics_Types.set_proofstate_range
                                                           ps'1
                                                           (FStar_Range.prims_to_fstar_range
                                                              (Prims.mk_range
                                                                 "Data.Serialize.Encode.fst"
                                                                 (Prims.of_int (41))
                                                                 (Prims.of_int (4))
                                                                 (Prims.of_int (62))
                                                                 (Prims.of_int (5))))))
                                                     (FStar_Range.prims_to_fstar_range
                                                        (Prims.mk_range
                                                           "Data.Serialize.Encode.fst"
                                                           (Prims.of_int (41))
                                                           (Prims.of_int (28))
                                                           (Prims.of_int (41))
                                                           (Prims.of_int (54))))))
                                       with
                                       | FStar_Tactics_Result.Success
                                           (a2, ps'2) ->
                                           (match () with
                                            | () ->
                                                (match match (FStar_Tactics_Derived.binder_to_term
                                                                a1)
                                                               (FStar_Tactics_Types.incr_depth
                                                                  (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (42))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (5))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (42))
                                                                    (Prims.of_int (49))
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (5))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (43))
                                                                    (Prims.of_int (25))
                                                                    (Prims.of_int (43))
                                                                    (Prims.of_int (45))))))
                                                       with
                                                       | FStar_Tactics_Result.Success
                                                           (a3, ps'3) ->
                                                           (match () with
                                                            | () ->
                                                                (match 
                                                                   (FStar_Tactics_Util.map
                                                                    (fun
                                                                    uu____28186
                                                                    ->
                                                                    fun s1 ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((match uu____28186
                                                                    with
                                                                    | (i,
                                                                    (cName,
                                                                    cArgs))
                                                                    ->
                                                                    (fun bvs
                                                                    ->
                                                                    fun ps2
                                                                    ->
                                                                    match 
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_Const
                                                                    (FStar_Reflection_Data.C_Int
                                                                    i)))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (34))
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (61))))))
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
                                                                    match 
                                                                    (FStar_Tactics_Derived.binder_to_term
                                                                    a2)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (15))
                                                                    (Prims.of_int (57))
                                                                    (Prims.of_int (16))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (57))
                                                                    (Prims.of_int (16))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (55))
                                                                    (Prims.of_int (19))
                                                                    (Prims.of_int (55))
                                                                    (Prims.of_int (53))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a5,
                                                                    ps'5) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (FStar_Tactics_Util.fold_left
                                                                    (fun s2
                                                                    ->
                                                                    fun
                                                                    uu____28479
                                                                    ->
                                                                    match uu____28479
                                                                    with
                                                                    | 
                                                                    (j, arg)
                                                                    ->
                                                                    (fun ps3
                                                                    ->
                                                                    match 
                                                                    (generateDecodeSerialize_term_for_argSumup
                                                                    a arg)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (51))
                                                                    (Prims.of_int (23))
                                                                    (Prims.of_int (51))
                                                                    (Prims.of_int (79))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a6,
                                                                    ps'6) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    (FStar_Tactics_Derived.bv_to_term
                                                                    (FStar_List_Tot_Base.index
                                                                    bvs j))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'6
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (24))
                                                                    (Prims.of_int (53))
                                                                    (Prims.of_int (24))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (52))
                                                                    (Prims.of_int (23))
                                                                    (Prims.of_int (52))
                                                                    (Prims.of_int (51))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a7,
                                                                    ps'7) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    (a6,
                                                                    (a7,
                                                                    FStar_Reflection_Data.Q_Explicit))))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'7
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
                                                                    (a8,
                                                                    ps'8) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    (a8,
                                                                    (s2,
                                                                    FStar_Reflection_Data.Q_Explicit))))
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'8
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (37)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'8)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'8)))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'7)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'7)))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)))
                                                                    a5
                                                                    (Data_Serialize_Helpers.withIndex
                                                                    cArgs))
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (57))
                                                                    (Prims.of_int (16)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a5,
                                                                    ps'5) ->
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
                                                                    "Helpers";
                                                                    "Serialized";
                                                                    "appendInt"]))),
                                                                    (a4,
                                                                    FStar_Reflection_Data.Q_Explicit))))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
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
                                                                    (a6,
                                                                    ps'6) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    (a6,
                                                                    (a5,
                                                                    FStar_Reflection_Data.Q_Explicit))))
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'6
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (37)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)))
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
                                                                    (e, ps'4))),
                                                                    s1))
                                                                    (Data_Serialize_Helpers.withIndex
                                                                    iCons))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (42))
                                                                    (Prims.of_int (49))
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (5))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (44))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (7))))))
                                                                 with
                                                                 | FStar_Tactics_Result.Success
                                                                    (a4,
                                                                    ps'4) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (Data_Serialize_Helpers.mkMatchInductive
                                                                    s a3 a4)
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (42))
                                                                    (Prims.of_int (49))
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (5)))))))
                                                                 | FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                    ->
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
                                                          (Data_Serialize_Helpers.mk_abs
                                                             (FStar_List_Tot_Base.append
                                                                a [a1; a2])
                                                             a3)
                                                            (FStar_Tactics_Types.decr_depth
                                                               (FStar_Tactics_Types.set_proofstate_range
                                                                  ps'3
                                                                  (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (42))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (5)))))))
                                                 | FStar_Tactics_Result.Failed
                                                     (e, ps'3) ->
                                                     FStar_Tactics_Result.Failed
                                                       (e, ps'3)))
                                       | FStar_Tactics_Result.Failed
                                           (e, ps'2) ->
                                           FStar_Tactics_Result.Failed
                                             (e, ps'2)))
                             | FStar_Tactics_Result.Failed (e, ps'1) ->
                                 FStar_Tactics_Result.Failed (e, ps'1)))
                   | FStar_Tactics_Result.Failed (e, ps') ->
                       FStar_Tactics_Result.Failed (e, ps'))))
            (FStar_Tactics_Types.decr_depth
               (FStar_Tactics_Types.set_proofstate_range
                  (FStar_Tactics_Types.incr_depth
                     (FStar_Tactics_Types.set_proofstate_range ps
                        (FStar_Range.prims_to_fstar_range
                           (Prims.mk_range "Data.Serialize.Encode.fst"
                              (Prims.of_int (37)) (Prims.of_int (32))
                              (Prims.of_int (37)) (Prims.of_int (33))))))
                  (FStar_Range.prims_to_fstar_range
                     (Prims.mk_range "Data.Serialize.Encode.fst"
                        (Prims.of_int (37)) (Prims.of_int (4))
                        (Prims.of_int (62)) (Prims.of_int (5))))))
let (generateEncodeSerialize_for_inductiveSumup :
  Data_Serialize_Types.inductiveSumup ->
    (FStar_Reflection_Data.decls, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun s ->
    fun ps ->
      match () with
      | () ->
          ((match s with
            | { Data_Serialize_Types.iName = iName;
                Data_Serialize_Types.iVars = iVars;
                Data_Serialize_Types.iCons = iCons;_} ->
                (fun ps1 ->
                   match (FStar_Tactics_Util.map
                            (fun uu____30010 ->
                               fun ps2 ->
                                 match (FStar_Tactics_Derived.fresh_bv
                                          (FStar_Reflection_Basic.pack_ln
                                             FStar_Reflection_Data.Tv_Unknown))
                                         (FStar_Tactics_Types.incr_depth
                                            (FStar_Tactics_Types.set_proofstate_range
                                               ps2
                                               (FStar_Range.prims_to_fstar_range
                                                  (Prims.mk_range
                                                     "Data.Serialize.Encode.fst"
                                                     (Prims.of_int (69))
                                                     (Prims.of_int (34))
                                                     (Prims.of_int (69))
                                                     (Prims.of_int (49))))))
                                 with
                                 | FStar_Tactics_Result.Success (a, ps') ->
                                     (match () with
                                      | () ->
                                          FStar_Tactics_Result.Success
                                            ((FStar_Reflection_Basic.pack_binder
                                                a
                                                FStar_Reflection_Data.Q_Explicit),
                                              (FStar_Tactics_Types.decr_depth
                                                 (FStar_Tactics_Types.set_proofstate_range
                                                    ps'
                                                    (FStar_Range.prims_to_fstar_range
                                                       (Prims.mk_range
                                                          "Data.Serialize.Encode.fst"
                                                          (Prims.of_int (69))
                                                          (Prims.of_int (22))
                                                          (Prims.of_int (69))
                                                          (Prims.of_int (60))))))))
                                 | FStar_Tactics_Result.Failed (e, ps') ->
                                     FStar_Tactics_Result.Failed (e, ps'))
                            (Data_Serialize_Helpers.mkList Prims.int_zero
                               (s.Data_Serialize_Types.iVars - Prims.int_one)))
                           (FStar_Tactics_Types.incr_depth
                              (FStar_Tactics_Types.set_proofstate_range ps1
                                 (FStar_Range.prims_to_fstar_range
                                    (Prims.mk_range
                                       "Data.Serialize.Encode.fst"
                                       (Prims.of_int (69)) (Prims.of_int (8))
                                       (Prims.of_int (69))
                                       (Prims.of_int (84))))))
                   with
                   | FStar_Tactics_Result.Success (a, ps') ->
                       (match () with
                        | () ->
                            (match (FStar_Tactics_Derived.fresh_binder
                                      (FStar_Reflection_Basic.pack_ln
                                         FStar_Reflection_Data.Tv_Unknown))
                                     (FStar_Tactics_Types.incr_depth
                                        (FStar_Tactics_Types.set_proofstate_range
                                           (FStar_Tactics_Types.decr_depth
                                              (FStar_Tactics_Types.set_proofstate_range
                                                 ps'
                                                 (FStar_Range.prims_to_fstar_range
                                                    (Prims.mk_range
                                                       "Data.Serialize.Encode.fst"
                                                       (Prims.of_int (70))
                                                       (Prims.of_int (4))
                                                       (Prims.of_int (81))
                                                       (Prims.of_int (40))))))
                                           (FStar_Range.prims_to_fstar_range
                                              (Prims.mk_range
                                                 "Data.Serialize.Encode.fst"
                                                 (Prims.of_int (70))
                                                 (Prims.of_int (14))
                                                 (Prims.of_int (70))
                                                 (Prims.of_int (31))))))
                             with
                             | FStar_Tactics_Result.Success (a1, ps'1) ->
                                 (match () with
                                  | () ->
                                      (match (FStar_Tactics_Derived.fresh_binder
                                                (FStar_Reflection_Basic.pack_ln
                                                   (FStar_Reflection_Data.Tv_FVar
                                                      (FStar_Reflection_Basic.pack_fv
                                                         ["Data";
                                                         "Serialize";
                                                         "Types";
                                                         "serialized"]))))
                                               (FStar_Tactics_Types.incr_depth
                                                  (FStar_Tactics_Types.set_proofstate_range
                                                     (FStar_Tactics_Types.decr_depth
                                                        (FStar_Tactics_Types.set_proofstate_range
                                                           ps'1
                                                           (FStar_Range.prims_to_fstar_range
                                                              (Prims.mk_range
                                                                 "Data.Serialize.Encode.fst"
                                                                 (Prims.of_int (71))
                                                                 (Prims.of_int (4))
                                                                 (Prims.of_int (81))
                                                                 (Prims.of_int (40))))))
                                                     (FStar_Range.prims_to_fstar_range
                                                        (Prims.mk_range
                                                           "Data.Serialize.Encode.fst"
                                                           (Prims.of_int (71))
                                                           (Prims.of_int (28))
                                                           (Prims.of_int (71))
                                                           (Prims.of_int (54))))))
                                       with
                                       | FStar_Tactics_Result.Success
                                           (a2, ps'2) ->
                                           (match () with
                                            | () ->
                                                (match (generateEncodeSerialize_term_for_inductiveSumup
                                                          s)
                                                         (FStar_Tactics_Types.incr_depth
                                                            (FStar_Tactics_Types.set_proofstate_range
                                                               (FStar_Tactics_Types.decr_depth
                                                                  (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (73))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (81))
                                                                    (Prims.of_int (40))))))
                                                               (FStar_Range.prims_to_fstar_range
                                                                  (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (73))
                                                                    (Prims.of_int (15))
                                                                    (Prims.of_int (73))
                                                                    (Prims.of_int (64))))))
                                                 with
                                                 | FStar_Tactics_Result.Success
                                                     (a3, ps'3) ->
                                                     (match () with
                                                      | () ->
                                                          (match () with
                                                           | () ->
                                                               (match 
                                                                  match 
                                                                    match 
                                                                    match 
                                                                    match 
                                                                    (FStar_Tactics_Util.map
                                                                    FStar_Tactics_Derived.binder_to_term
                                                                    a)
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
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (81))
                                                                    (Prims.of_int (40))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (33))
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (78))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (75))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (81))
                                                                    (Prims.of_int (40))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (76))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (79))
                                                                    (Prims.of_int (7))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (76))
                                                                    (Prims.of_int (31))
                                                                    (Prims.of_int (79))
                                                                    (Prims.of_int (7))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (9))
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (95))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (24))
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (94))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (25))
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (52))))))
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
                                                                    match 
                                                                    (FStar_Tactics_Derived.binder_to_term
                                                                    a1)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (24))
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (94))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (55))
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (93))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (74))))))
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
                                                                    ([a5;
                                                                    FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Data";
                                                                    "Serialize";
                                                                    "Helpers";
                                                                    "Serialized";
                                                                    "emptySerialized"]))],
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (55))
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (93))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
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
                                                                    ((FStar_List_Tot_Base.append
                                                                    a4 a5),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (24))
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (94))))))))
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
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Derived.mk_e_app
                                                                    a3 a4),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (9))
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (95))))))))
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
                                                                    (Data_Serialize_Helpers.mk_abs
                                                                    (FStar_List_Tot_Base.append
                                                                    a [a1])
                                                                    a4)
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (76))
                                                                    (Prims.of_int (31))
                                                                    (Prims.of_int (79))
                                                                    (Prims.of_int (7)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                  with
                                                                  | FStar_Tactics_Result.Success
                                                                    (a4,
                                                                    ps'4) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    match 
                                                                    match 
                                                                    (transform_name_encode
                                                                    s.Data_Serialize_Types.iName)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (76))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (79))
                                                                    (Prims.of_int (7))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (76))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (79))
                                                                    (Prims.of_int (7))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (70))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (54))
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (69))))))
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
                                                                    ((FStar_Reflection_Basic.pack_fv
                                                                    a5),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (70))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
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
                                                                    (((
                                                                    fun
                                                                    _30221 ->
                                                                    FStar_Reflection_Data.Sg_Let
                                                                    (true,
                                                                    a5, [],
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    FStar_Reflection_Data.Tv_Unknown),
                                                                    _30221))),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (33))
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (78))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
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
                                                                    ((a5 a4),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (76))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (79))
                                                                    (Prims.of_int (7))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)))
                                                                  | FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                with
                                                                | FStar_Tactics_Result.Success
                                                                    (a4,
                                                                    ps'4) ->
                                                                    (
                                                                    match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    match 
                                                                    match 
                                                                    match 
                                                                    (transform_name_encode'
                                                                    s.Data_Serialize_Types.iName)
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
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (80))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (81))
                                                                    (Prims.of_int (40))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (80))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (80))
                                                                    (Prims.of_int (44))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (80))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (80))
                                                                    (Prims.of_int (44))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (70))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (54))
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (69))))))
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
                                                                    ((FStar_Reflection_Basic.pack_fv
                                                                    a5),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (70))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
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
                                                                    (((
                                                                    fun
                                                                    _30279 ->
                                                                    FStar_Reflection_Data.Sg_Let
                                                                    (true,
                                                                    a5, [],
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    FStar_Reflection_Data.Tv_Unknown),
                                                                    _30279))),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (33))
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (78))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
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
                                                                    ((a5 a3),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (80))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (80))
                                                                    (Prims.of_int (44))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
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
                                                                    ([
                                                                    FStar_Reflection_Basic.pack_sigelt
                                                                    a5;
                                                                    FStar_Reflection_Basic.pack_sigelt
                                                                    a4],
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Encode.fst"
                                                                    (Prims.of_int (81))
                                                                    (Prims.of_int (7))
                                                                    (Prims.of_int (81))
                                                                    (Prims.of_int (40))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)))
                                                                | FStar_Tactics_Result.Failed
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
                                 FStar_Tactics_Result.Failed (e, ps'1)))
                   | FStar_Tactics_Result.Failed (e, ps') ->
                       FStar_Tactics_Result.Failed (e, ps'))))
            (FStar_Tactics_Types.decr_depth
               (FStar_Tactics_Types.set_proofstate_range
                  (FStar_Tactics_Types.incr_depth
                     (FStar_Tactics_Types.set_proofstate_range ps
                        (FStar_Range.prims_to_fstar_range
                           (Prims.mk_range "Data.Serialize.Encode.fst"
                              (Prims.of_int (67)) (Prims.of_int (32))
                              (Prims.of_int (67)) (Prims.of_int (33))))))
                  (FStar_Range.prims_to_fstar_range
                     (Prims.mk_range "Data.Serialize.Encode.fst"
                        (Prims.of_int (67)) (Prims.of_int (4))
                        (Prims.of_int (81)) (Prims.of_int (40))))))
let (generateEncodeSerialize :
  FStar_Reflection_Types.fv ->
    (FStar_Reflection_Data.decls, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun name ->
    fun ps ->
      match (Data_Serialize_Rep.makeGenericRep name)
              (FStar_Tactics_Types.incr_depth
                 (FStar_Tactics_Types.set_proofstate_range ps
                    (FStar_Range.prims_to_fstar_range
                       (Prims.mk_range "Data.Serialize.Encode.fst"
                          (Prims.of_int (86)) (Prims.of_int (12))
                          (Prims.of_int (86)) (Prims.of_int (31))))))
      with
      | FStar_Tactics_Result.Success (a, ps') ->
          (match () with
           | () ->
               (generateEncodeSerialize_for_inductiveSumup a)
                 (FStar_Tactics_Types.decr_depth
                    (FStar_Tactics_Types.set_proofstate_range ps'
                       (FStar_Range.prims_to_fstar_range
                          (Prims.mk_range "Data.Serialize.Encode.fst"
                             (Prims.of_int (87)) (Prims.of_int (4))
                             (Prims.of_int (87)) (Prims.of_int (48)))))))
      | FStar_Tactics_Result.Failed (e, ps') ->
          FStar_Tactics_Result.Failed (e, ps')