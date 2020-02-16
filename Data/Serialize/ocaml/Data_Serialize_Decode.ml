open Prims
let (transform_name_decode' :
  FStar_Reflection_Types.name ->
    (FStar_Reflection_Types.name, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun n1 ->
    Data_Serialize_Helpers.nameCurMod' n1
      (fun x -> Prims.strcat x "_serialize_decode_chainable")
let (transform_name_decode :
  FStar_Reflection_Types.name ->
    (FStar_Reflection_Types.name, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun n1 ->
    Data_Serialize_Helpers.nameCurMod' n1
      (fun x -> Prims.strcat x "_serialize_decode")
let rec change_last : 'a . ('a -> 'a) -> 'a Prims.list -> 'a Prims.list =
  fun f ->
    fun l ->
      match l with
      | [] -> []
      | hd1::[] -> [f hd1]
      | hd1::tl1 -> hd1 :: (change_last f tl1)
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
                         "readInt"]))), s))
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
                         "readString"]))), s))
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
                         "readBool"]))), s))
      | Data_Serialize_Types.AS_List typ ->
          (fun ps ->
             match (generateDecodeSerialize_term_for_argSumup args_fun typ)
                     (FStar_Tactics_Types.incr_depth
                        (FStar_Tactics_Types.set_proofstate_range ps
                           (FStar_Range.prims_to_fstar_range
                              (Prims.mk_range "Data.Serialize.Decode.fst"
                                 (Prims.of_int (32)) (Prims.of_int (16))
                                 (Prims.of_int (32)) (Prims.of_int (72))))))
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
                                      "readList"]))),
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
             match match (transform_name_decode' tname)
                           (FStar_Tactics_Types.incr_depth
                              (FStar_Tactics_Types.set_proofstate_range
                                 (FStar_Tactics_Types.incr_depth
                                    (FStar_Tactics_Types.set_proofstate_range
                                       ps
                                       (FStar_Range.prims_to_fstar_range
                                          (Prims.mk_range
                                             "Data.Serialize.Decode.fst"
                                             (Prims.of_int (35))
                                             (Prims.of_int (12))
                                             (Prims.of_int (35))
                                             (Prims.of_int (55))))))
                                 (FStar_Range.prims_to_fstar_range
                                    (Prims.mk_range
                                       "Data.Serialize.Decode.fst"
                                       (Prims.of_int (35))
                                       (Prims.of_int (25))
                                       (Prims.of_int (35))
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
                                          "Data.Serialize.Decode.fst"
                                          (Prims.of_int (35))
                                          (Prims.of_int (12))
                                          (Prims.of_int (35))
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
                                                 "Data.Serialize.Decode.fst"
                                                 (Prims.of_int (36))
                                                 (Prims.of_int (4))
                                                 (Prims.of_int (37))
                                                 (Prims.of_int (80))))))
                                     (FStar_Range.prims_to_fstar_range
                                        (Prims.mk_range
                                           "Data.Serialize.Decode.fst"
                                           (Prims.of_int (36))
                                           (Prims.of_int (12))
                                           (Prims.of_int (36))
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
                                                           "Data.Serialize.Decode.fst"
                                                           (Prims.of_int (37))
                                                           (Prims.of_int (4))
                                                           (Prims.of_int (37))
                                                           (Prims.of_int (80))))))
                                               (FStar_Range.prims_to_fstar_range
                                                  (Prims.mk_range
                                                     "Data.Serialize.Decode.fst"
                                                     (Prims.of_int (37))
                                                     (Prims.of_int (16))
                                                     (Prims.of_int (37))
                                                     (Prims.of_int (79))))))
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
                                                          "Data.Serialize.Decode.fst"
                                                          (Prims.of_int (37))
                                                          (Prims.of_int (4))
                                                          (Prims.of_int (37))
                                                          (Prims.of_int (80))))))))
                                 | FStar_Tactics_Result.Failed (e, ps'2) ->
                                     FStar_Tactics_Result.Failed (e, ps'2)))
                       | FStar_Tactics_Result.Failed (e, ps'1) ->
                           FStar_Tactics_Result.Failed (e, ps'1)))
             | FStar_Tactics_Result.Failed (e, ps') ->
                 FStar_Tactics_Result.Failed (e, ps'))
let (id_tac_term :
  FStar_Reflection_Types.term ->
    (FStar_Reflection_Types.term, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  = fun t -> fun s -> FStar_Tactics_Result.Success (t, s)
let (generateDecodeSerialize_term_for_consSumup :
  Prims.nat ->
    FStar_Reflection_Types.binder Prims.list ->
      unit Data_Serialize_Types.consSumup ->
        FStar_Reflection_Types.bv ->
          (FStar_Reflection_Types.term, unit)
            FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun n1 ->
    fun encoders ->
      fun cons1 ->
        fun serialized_inp ->
          fun ps ->
            match () with
            | () ->
                ((match cons1 with
                  | (consName, args) ->
                      (fun ps1 ->
                         match (FStar_Tactics_Util.fold_right
                                  (fun arg ->
                                     fun uu____20818 ->
                                       match uu____20818 with
                                       | (bvs_args, bv_serialized, mkTerm) ->
                                           (fun ps2 ->
                                              match match (generateDecodeSerialize_term_for_argSumup
                                                             encoders arg)
                                                            (FStar_Tactics_Types.incr_depth
                                                               (FStar_Tactics_Types.set_proofstate_range
                                                                  (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (25))
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (88))))))
                                                                  (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (48))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (48))
                                                                    (Prims.of_int (70))))))
                                                    with
                                                    | FStar_Tactics_Result.Success
                                                        (a, ps') ->
                                                        (match () with
                                                         | () ->
                                                             (match match 
                                                                    (FStar_Tactics_Derived.bv_to_term
                                                                    bv_serialized)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (25))
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (88))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (88))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (34))
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (60))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a1,
                                                                    ps'1) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    (FStar_Tactics_Derived.bv_to_term
                                                                    bv_serialized)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (88))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (87))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a2,
                                                                    ps'2) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (Data_Serialize_Helpers.add_admit_decr_lex
                                                                    a1 a2)
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (88)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'2)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'2)))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'1)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'1)
                                                              with
                                                              | FStar_Tactics_Result.Success
                                                                  (a1, ps'1)
                                                                  ->
                                                                  (match ()
                                                                   with
                                                                   | 
                                                                   () ->
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    (a,
                                                                    (a1,
                                                                    FStar_Reflection_Data.Q_Explicit))))
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (37)))))))
                                                              | FStar_Tactics_Result.Failed
                                                                  (e, ps'1)
                                                                  ->
                                                                  FStar_Tactics_Result.Failed
                                                                    (e, ps'1)))
                                                    | FStar_Tactics_Result.Failed
                                                        (e, ps') ->
                                                        FStar_Tactics_Result.Failed
                                                          (e, ps')
                                              with
                                              | FStar_Tactics_Result.Success
                                                  (a, ps') ->
                                                  (match () with
                                                   | () ->
                                                       (match (Data_Serialize_Helpers.mkLet_tup'
                                                                 a)
                                                                (FStar_Tactics_Types.incr_depth
                                                                   (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (52))
                                                                    (Prims.of_int (12))
                                                                    (Prims.of_int (57))
                                                                    (Prims.of_int (26))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (52))
                                                                    (Prims.of_int (51))
                                                                    (Prims.of_int (52))
                                                                    (Prims.of_int (68))))))
                                                        with
                                                        | FStar_Tactics_Result.Success
                                                            (a1, ps'1) ->
                                                            (match () with
                                                             | () ->
                                                                 FStar_Tactics_Result.Success
                                                                   (((
                                                                    match a1
                                                                    with
                                                                    | 
                                                                    (mkTerm',
                                                                    (bv_arg,
                                                                    bv_serialized1))
                                                                    ->
                                                                    ((bv_arg
                                                                    ::
                                                                    bvs_args),
                                                                    bv_serialized1,
                                                                    ((fun
                                                                    inner ->
                                                                    fun ps3
                                                                    ->
                                                                    match 
                                                                    (mkTerm'
                                                                    inner)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (25))
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (40))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a2,
                                                                    ps'2) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (mkTerm
                                                                    a2)
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (18))
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (40)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'2)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'2)))))),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (52))
                                                                    (Prims.of_int (16))
                                                                    (Prims.of_int (52))
                                                                    (Prims.of_int (48))))))))
                                                        | FStar_Tactics_Result.Failed
                                                            (e, ps'1) ->
                                                            FStar_Tactics_Result.Failed
                                                              (e, ps'1)))
                                              | FStar_Tactics_Result.Failed
                                                  (e, ps') ->
                                                  FStar_Tactics_Result.Failed
                                                    (e, ps'))) args
                                  ([], serialized_inp, id_tac_term))
                                 (FStar_Tactics_Types.incr_depth
                                    (FStar_Tactics_Types.set_proofstate_range
                                       ps1
                                       (FStar_Range.prims_to_fstar_range
                                          (Prims.mk_range
                                             "Data.Serialize.Decode.fst"
                                             (Prims.of_int (45))
                                             (Prims.of_int (8))
                                             (Prims.of_int (58))
                                             (Prims.of_int (50))))))
                         with
                         | FStar_Tactics_Result.Success (a, ps') ->
                             (match () with
                              | () ->
                                  ((match a with
                                    | (bvs_args, bv_serialized, mkTerm) ->
                                        (fun ps2 ->
                                           match match match (Data_Serialize_Helpers.name_to_term
                                                                consName)
                                                               (FStar_Tactics_Types.incr_depth
                                                                  (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (60))
                                                                    (Prims.of_int (11))
                                                                    (Prims.of_int (63))
                                                                    (Prims.of_int (5))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (19))
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (77))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (28))
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (51))))))
                                                       with
                                                       | FStar_Tactics_Result.Success
                                                           (a1, ps'1) ->
                                                           (match () with
                                                            | () ->
                                                                (match 
                                                                   (FStar_Tactics_Util.map
                                                                    FStar_Tactics_Derived.bv_to_term
                                                                    bvs_args)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (19))
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (77))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (52))
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (77))))))
                                                                 with
                                                                 | FStar_Tactics_Result.Success
                                                                    (a2,
                                                                    ps'2) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Derived.mk_e_app
                                                                    a1 a2),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (19))
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (77))))))))
                                                                 | FStar_Tactics_Result.Failed
                                                                    (e, ps'2)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'2)))
                                                       | FStar_Tactics_Result.Failed
                                                           (e, ps'1) ->
                                                           FStar_Tactics_Result.Failed
                                                             (e, ps'1)
                                                 with
                                                 | FStar_Tactics_Result.Success
                                                     (a1, ps'1) ->
                                                     (match () with
                                                      | () ->
                                                          (match (FStar_Tactics_Derived.bv_to_term
                                                                    bv_serialized)
                                                                   (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (57))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (31))
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (57))))))
                                                           with
                                                           | FStar_Tactics_Result.Success
                                                               (a2, ps'2) ->
                                                               (match () with
                                                                | () ->
                                                                    (
                                                                    match 
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["FStar";
                                                                    "Pervasives";
                                                                    "Native";
                                                                    "Mktuple2"]))),
                                                                    (a1,
                                                                    FStar_Reflection_Data.Q_Explicit))))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
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
                                                                    (a3,
                                                                    ps'3) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    (a3,
                                                                    (a2,
                                                                    FStar_Reflection_Data.Q_Explicit))))
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (37)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'3)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'3)))
                                                           | FStar_Tactics_Result.Failed
                                                               (e, ps'2) ->
                                                               FStar_Tactics_Result.Failed
                                                                 (e, ps'2)))
                                                 | FStar_Tactics_Result.Failed
                                                     (e, ps'1) ->
                                                     FStar_Tactics_Result.Failed
                                                       (e, ps'1)
                                           with
                                           | FStar_Tactics_Result.Success
                                               (a1, ps'1) ->
                                               (match () with
                                                | () ->
                                                    (mkTerm a1)
                                                      (FStar_Tactics_Types.decr_depth
                                                         (FStar_Tactics_Types.set_proofstate_range
                                                            ps'1
                                                            (FStar_Range.prims_to_fstar_range
                                                               (Prims.mk_range
                                                                  "Data.Serialize.Decode.fst"
                                                                  (Prims.of_int (60))
                                                                  (Prims.of_int (4))
                                                                  (Prims.of_int (63))
                                                                  (Prims.of_int (5)))))))
                                           | FStar_Tactics_Result.Failed
                                               (e, ps'1) ->
                                               FStar_Tactics_Result.Failed
                                                 (e, ps'1))))
                                    (FStar_Tactics_Types.decr_depth
                                       (FStar_Tactics_Types.set_proofstate_range
                                          ps'
                                          (FStar_Range.prims_to_fstar_range
                                             (Prims.mk_range
                                                "Data.Serialize.Decode.fst"
                                                (Prims.of_int (44))
                                                (Prims.of_int (4))
                                                (Prims.of_int (63))
                                                (Prims.of_int (5)))))))
                         | FStar_Tactics_Result.Failed (e, ps') ->
                             FStar_Tactics_Result.Failed (e, ps'))))
                  (FStar_Tactics_Types.decr_depth
                     (FStar_Tactics_Types.set_proofstate_range
                        (FStar_Tactics_Types.incr_depth
                           (FStar_Tactics_Types.set_proofstate_range ps
                              (FStar_Range.prims_to_fstar_range
                                 (Prims.mk_range "Data.Serialize.Decode.fst"
                                    (Prims.of_int (43)) (Prims.of_int (25))
                                    (Prims.of_int (43)) (Prims.of_int (29))))))
                        (FStar_Range.prims_to_fstar_range
                           (Prims.mk_range "Data.Serialize.Decode.fst"
                              (Prims.of_int (43)) (Prims.of_int (4))
                              (Prims.of_int (63)) (Prims.of_int (5))))))
let (generateDecodeSerialize_term_for_inductiveSumup :
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
                            (fun uu____22080 ->
                               fun ps2 ->
                                 match (FStar_Tactics_Derived.fresh_bv
                                          (FStar_Reflection_Basic.pack_ln
                                             FStar_Reflection_Data.Tv_Unknown))
                                         (FStar_Tactics_Types.incr_depth
                                            (FStar_Tactics_Types.set_proofstate_range
                                               ps2
                                               (FStar_Range.prims_to_fstar_range
                                                  (Prims.mk_range
                                                     "Data.Serialize.Decode.fst"
                                                     (Prims.of_int (71))
                                                     (Prims.of_int (34))
                                                     (Prims.of_int (71))
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
                                                          "Data.Serialize.Decode.fst"
                                                          (Prims.of_int (71))
                                                          (Prims.of_int (22))
                                                          (Prims.of_int (71))
                                                          (Prims.of_int (60))))))))
                                 | FStar_Tactics_Result.Failed (e, ps') ->
                                     FStar_Tactics_Result.Failed (e, ps'))
                            (Data_Serialize_Helpers.mkList Prims.int_zero
                               (s.Data_Serialize_Types.iVars - Prims.int_one)))
                           (FStar_Tactics_Types.incr_depth
                              (FStar_Tactics_Types.set_proofstate_range ps1
                                 (FStar_Range.prims_to_fstar_range
                                    (Prims.mk_range
                                       "Data.Serialize.Decode.fst"
                                       (Prims.of_int (70)) (Prims.of_int (8))
                                       (Prims.of_int (72))
                                       (Prims.of_int (32))))))
                   with
                   | FStar_Tactics_Result.Success (a, ps') ->
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
                                                 ps'
                                                 (FStar_Range.prims_to_fstar_range
                                                    (Prims.mk_range
                                                       "Data.Serialize.Decode.fst"
                                                       (Prims.of_int (74))
                                                       (Prims.of_int (4))
                                                       (Prims.of_int (86))
                                                       (Prims.of_int (5))))))
                                           (FStar_Range.prims_to_fstar_range
                                              (Prims.mk_range
                                                 "Data.Serialize.Decode.fst"
                                                 (Prims.of_int (74))
                                                 (Prims.of_int (14))
                                                 (Prims.of_int (74))
                                                 (Prims.of_int (40))))))
                             with
                             | FStar_Tactics_Result.Success (a1, ps'1) ->
                                 (match () with
                                  | () ->
                                      (match match match (FStar_Tactics_Derived.binder_to_term
                                                            a1)
                                                           (FStar_Tactics_Types.incr_depth
                                                              (FStar_Tactics_Types.set_proofstate_range
                                                                 (FStar_Tactics_Types.incr_depth
                                                                    (
                                                                    FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (75))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (5))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (75))
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (5))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (77))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (77))
                                                                    (Prims.of_int (30))))))
                                                                 (FStar_Range.prims_to_fstar_range
                                                                    (
                                                                    Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (11))))))
                                                   with
                                                   | FStar_Tactics_Result.Success
                                                       (a2, ps'2) ->
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
                                                                    "readInt"]))),
                                                                    (a2,
                                                                    FStar_Reflection_Data.Q_Explicit))))
                                                              (FStar_Tactics_Types.decr_depth
                                                                 (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
                                                                    (
                                                                    FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (37)))))))
                                                   | FStar_Tactics_Result.Failed
                                                       (e, ps'2) ->
                                                       FStar_Tactics_Result.Failed
                                                         (e, ps'2)
                                             with
                                             | FStar_Tactics_Result.Success
                                                 (a2, ps'2) ->
                                                 (match () with
                                                  | () ->
                                                      (Data_Serialize_Helpers.mkLet_tup
                                                         a2
                                                         (fun consIndex ->
                                                            fun inp ->
                                                              fun ps2 ->
                                                                match 
                                                                  (FStar_Tactics_Util.map
                                                                    (fun
                                                                    cons1 ->
                                                                    generateDecodeSerialize_term_for_consSumup
                                                                    s.Data_Serialize_Types.iVars
                                                                    a cons1
                                                                    inp)
                                                                    iCons)
                                                                    (
                                                                    FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (80))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (82))
                                                                    (Prims.of_int (21))))))
                                                                with
                                                                | FStar_Tactics_Result.Success
                                                                    (a3,
                                                                    ps'3) ->
                                                                    (
                                                                    match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    (FStar_Tactics_Derived.bv_to_term
                                                                    consIndex)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (84))
                                                                    (Prims.of_int (10))
                                                                    (Prims.of_int (84))
                                                                    (Prims.of_int (52))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (84))
                                                                    (Prims.of_int (21))
                                                                    (Prims.of_int (84))
                                                                    (Prims.of_int (43))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a4,
                                                                    ps'4) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (Data_Serialize_Helpers.mkMatchInt
                                                                    a4 a3)
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (84))
                                                                    (Prims.of_int (10))
                                                                    (Prims.of_int (84))
                                                                    (Prims.of_int (52)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'4)))
                                                                | FStar_Tactics_Result.Failed
                                                                    (e, ps'3)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'3)))
                                                        (FStar_Tactics_Types.decr_depth
                                                           (FStar_Tactics_Types.set_proofstate_range
                                                              ps'2
                                                              (FStar_Range.prims_to_fstar_range
                                                                 (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (75))
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (5)))))))
                                             | FStar_Tactics_Result.Failed
                                                 (e, ps'2) ->
                                                 FStar_Tactics_Result.Failed
                                                   (e, ps'2)
                                       with
                                       | FStar_Tactics_Result.Success
                                           (a2, ps'2) ->
                                           (match () with
                                            | () ->
                                                (Data_Serialize_Helpers.mk_abs
                                                   (FStar_List_Tot_Base.append
                                                      a [a1]) a2)
                                                  (FStar_Tactics_Types.decr_depth
                                                     (FStar_Tactics_Types.set_proofstate_range
                                                        ps'2
                                                        (FStar_Range.prims_to_fstar_range
                                                           (Prims.mk_range
                                                              "Data.Serialize.Decode.fst"
                                                              (Prims.of_int (75))
                                                              (Prims.of_int (4))
                                                              (Prims.of_int (86))
                                                              (Prims.of_int (5)))))))
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
                           (Prims.mk_range "Data.Serialize.Decode.fst"
                              (Prims.of_int (68)) (Prims.of_int (32))
                              (Prims.of_int (68)) (Prims.of_int (33))))))
                  (FStar_Range.prims_to_fstar_range
                     (Prims.mk_range "Data.Serialize.Decode.fst"
                        (Prims.of_int (68)) (Prims.of_int (4))
                        (Prims.of_int (86)) (Prims.of_int (5))))))
let (generateDecodeSerialize_for_inductiveSumup :
  Data_Serialize_Types.inductiveSumup ->
    (FStar_Reflection_Data.decls, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun s ->
    fun ps ->
      match (FStar_Tactics_Util.map
               (fun uu____22547 ->
                  fun ps1 ->
                    match (FStar_Tactics_Derived.fresh_bv
                             (FStar_Reflection_Basic.pack_ln
                                FStar_Reflection_Data.Tv_Unknown))
                            (FStar_Tactics_Types.incr_depth
                               (FStar_Tactics_Types.set_proofstate_range ps1
                                  (FStar_Range.prims_to_fstar_range
                                     (Prims.mk_range
                                        "Data.Serialize.Decode.fst"
                                        (Prims.of_int (93))
                                        (Prims.of_int (34))
                                        (Prims.of_int (93))
                                        (Prims.of_int (49))))))
                    with
                    | FStar_Tactics_Result.Success (a, ps') ->
                        (match () with
                         | () ->
                             FStar_Tactics_Result.Success
                               ((FStar_Reflection_Basic.pack_binder a
                                   FStar_Reflection_Data.Q_Explicit),
                                 (FStar_Tactics_Types.decr_depth
                                    (FStar_Tactics_Types.set_proofstate_range
                                       ps'
                                       (FStar_Range.prims_to_fstar_range
                                          (Prims.mk_range
                                             "Data.Serialize.Decode.fst"
                                             (Prims.of_int (93))
                                             (Prims.of_int (22))
                                             (Prims.of_int (93))
                                             (Prims.of_int (60))))))))
                    | FStar_Tactics_Result.Failed (e, ps') ->
                        FStar_Tactics_Result.Failed (e, ps'))
               (Data_Serialize_Helpers.mkList Prims.int_zero
                  (s.Data_Serialize_Types.iVars - Prims.int_one)))
              (FStar_Tactics_Types.incr_depth
                 (FStar_Tactics_Types.set_proofstate_range ps
                    (FStar_Range.prims_to_fstar_range
                       (Prims.mk_range "Data.Serialize.Decode.fst"
                          (Prims.of_int (92)) (Prims.of_int (8))
                          (Prims.of_int (94)) (Prims.of_int (32))))))
      with
      | FStar_Tactics_Result.Success (a, ps') ->
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
                                    ps'
                                    (FStar_Range.prims_to_fstar_range
                                       (Prims.mk_range
                                          "Data.Serialize.Decode.fst"
                                          (Prims.of_int (96))
                                          (Prims.of_int (4))
                                          (Prims.of_int (109))
                                          (Prims.of_int (37))))))
                              (FStar_Range.prims_to_fstar_range
                                 (Prims.mk_range "Data.Serialize.Decode.fst"
                                    (Prims.of_int (96)) (Prims.of_int (14))
                                    (Prims.of_int (96)) (Prims.of_int (40))))))
                with
                | FStar_Tactics_Result.Success (a1, ps'1) ->
                    (match () with
                     | () ->
                         (match (generateDecodeSerialize_term_for_inductiveSumup
                                   s)
                                  (FStar_Tactics_Types.incr_depth
                                     (FStar_Tactics_Types.set_proofstate_range
                                        (FStar_Tactics_Types.decr_depth
                                           (FStar_Tactics_Types.set_proofstate_range
                                              ps'1
                                              (FStar_Range.prims_to_fstar_range
                                                 (Prims.mk_range
                                                    "Data.Serialize.Decode.fst"
                                                    (Prims.of_int (98))
                                                    (Prims.of_int (4))
                                                    (Prims.of_int (109))
                                                    (Prims.of_int (37))))))
                                        (FStar_Range.prims_to_fstar_range
                                           (Prims.mk_range
                                              "Data.Serialize.Decode.fst"
                                              (Prims.of_int (98))
                                              (Prims.of_int (15))
                                              (Prims.of_int (98))
                                              (Prims.of_int (64))))))
                          with
                          | FStar_Tactics_Result.Success (a2, ps'2) ->
                              (match () with
                               | () ->
                                   (match match match (transform_name_decode'
                                                         s.Data_Serialize_Types.iName)
                                                        (FStar_Tactics_Types.incr_depth
                                                           (FStar_Tactics_Types.set_proofstate_range
                                                              (FStar_Tactics_Types.incr_depth
                                                                 (FStar_Tactics_Types.set_proofstate_range
                                                                    (
                                                                    FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (99))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (109))
                                                                    (Prims.of_int (37))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (100))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (101))
                                                                    (Prims.of_int (17))))))
                                                                    (
                                                                    FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (100))
                                                                    (Prims.of_int (18))
                                                                    (Prims.of_int (100))
                                                                    (Prims.of_int (60))))))
                                                              (FStar_Range.prims_to_fstar_range
                                                                 (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (100))
                                                                    (Prims.of_int (27))
                                                                    (Prims.of_int (100))
                                                                    (Prims.of_int (59))))))
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
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (100))
                                                                    (Prims.of_int (18))
                                                                    (Prims.of_int (100))
                                                                    (Prims.of_int (60))))))))
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
                                                     ((FStar_Reflection_Data.Sg_Let
                                                         (true, a3, [],
                                                           (FStar_Reflection_Basic.pack_ln
                                                              FStar_Reflection_Data.Tv_Unknown),
                                                           a2)),
                                                       (FStar_Tactics_Types.decr_depth
                                                          (FStar_Tactics_Types.set_proofstate_range
                                                             ps'3
                                                             (FStar_Range.prims_to_fstar_range
                                                                (Prims.mk_range
                                                                   "Data.Serialize.Decode.fst"
                                                                   (Prims.of_int (100))
                                                                   (Prims.of_int (6))
                                                                   (Prims.of_int (101))
                                                                   (Prims.of_int (17))))))))
                                          | FStar_Tactics_Result.Failed
                                              (e, ps'3) ->
                                              FStar_Tactics_Result.Failed
                                                (e, ps'3)
                                    with
                                    | FStar_Tactics_Result.Success (a3, ps'3)
                                        ->
                                        (match () with
                                         | () ->
                                             (match match match (transform_name_decode
                                                                   s.Data_Serialize_Types.iName)
                                                                  (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (102))
                                                                    (Prims.of_int (7))
                                                                    (Prims.of_int (109))
                                                                    (Prims.of_int (37))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (103))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (106))
                                                                    (Prims.of_int (15))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (103))
                                                                    (Prims.of_int (18))
                                                                    (Prims.of_int (103))
                                                                    (Prims.of_int (59))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (103))
                                                                    (Prims.of_int (27))
                                                                    (Prims.of_int (103))
                                                                    (Prims.of_int (58))))))
                                                          with
                                                          | FStar_Tactics_Result.Success
                                                              (a4, ps'4) ->
                                                              (match () with
                                                               | () ->
                                                                   FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Basic.pack_fv
                                                                    a4),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (103))
                                                                    (Prims.of_int (18))
                                                                    (Prims.of_int (103))
                                                                    (Prims.of_int (59))))))))
                                                          | FStar_Tactics_Result.Failed
                                                              (e, ps'4) ->
                                                              FStar_Tactics_Result.Failed
                                                                (e, ps'4)
                                                    with
                                                    | FStar_Tactics_Result.Success
                                                        (a4, ps'4) ->
                                                        (match () with
                                                         | () ->
                                                             (match match 
                                                                    match 
                                                                    match 
                                                                    (FStar_Tactics_Util.map
                                                                    FStar_Tactics_Derived.binder_to_term
                                                                    (FStar_List_Tot_Base.append
                                                                    a [a1]))
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
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (103))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (106))
                                                                    (Prims.of_int (15))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (104))
                                                                    (Prims.of_int (13))
                                                                    (Prims.of_int (106))
                                                                    (Prims.of_int (15))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (104))
                                                                    (Prims.of_int (40))
                                                                    (Prims.of_int (106))
                                                                    (Prims.of_int (14))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (105))
                                                                    (Prims.of_int (28))
                                                                    (Prims.of_int (105))
                                                                    (Prims.of_int (83))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (105))
                                                                    (Prims.of_int (43))
                                                                    (Prims.of_int (105))
                                                                    (Prims.of_int (82))))))
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
                                                                    a2 a5),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (105))
                                                                    (Prims.of_int (28))
                                                                    (Prims.of_int (105))
                                                                    (Prims.of_int (83))))))))
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
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["FStar";
                                                                    "Pervasives";
                                                                    "Native";
                                                                    "fst"]))),
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
                                                                    (Data_Serialize_Helpers.mk_abs
                                                                    (FStar_List_Tot_Base.append
                                                                    a [a1])
                                                                    a5)
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (104))
                                                                    (Prims.of_int (13))
                                                                    (Prims.of_int (106))
                                                                    (Prims.of_int (15)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                              with
                                                              | FStar_Tactics_Result.Success
                                                                  (a5, ps'5)
                                                                  ->
                                                                  (match ()
                                                                   with
                                                                   | 
                                                                   () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Data.Sg_Let
                                                                    (true,
                                                                    a4, [],
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    FStar_Reflection_Data.Tv_Unknown),
                                                                    a5)),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (103))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (106))
                                                                    (Prims.of_int (15))))))))
                                                              | FStar_Tactics_Result.Failed
                                                                  (e, ps'5)
                                                                  ->
                                                                  FStar_Tactics_Result.Failed
                                                                    (e, ps'5)))
                                                    | FStar_Tactics_Result.Failed
                                                        (e, ps'4) ->
                                                        FStar_Tactics_Result.Failed
                                                          (e, ps'4)
                                              with
                                              | FStar_Tactics_Result.Success
                                                  (a4, ps'4) ->
                                                  (match () with
                                                   | () ->
                                                       FStar_Tactics_Result.Success
                                                         ([FStar_Reflection_Basic.pack_sigelt
                                                             a3;
                                                          FStar_Reflection_Basic.pack_sigelt
                                                            a4],
                                                           (FStar_Tactics_Types.decr_depth
                                                              (FStar_Tactics_Types.set_proofstate_range
                                                                 ps'4
                                                                 (FStar_Range.prims_to_fstar_range
                                                                    (
                                                                    Prims.mk_range
                                                                    "Data.Serialize.Decode.fst"
                                                                    (Prims.of_int (109))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (109))
                                                                    (Prims.of_int (37))))))))
                                              | FStar_Tactics_Result.Failed
                                                  (e, ps'4) ->
                                                  FStar_Tactics_Result.Failed
                                                    (e, ps'4)))
                                    | FStar_Tactics_Result.Failed (e, ps'3)
                                        ->
                                        FStar_Tactics_Result.Failed (e, ps'3)))
                          | FStar_Tactics_Result.Failed (e, ps'2) ->
                              FStar_Tactics_Result.Failed (e, ps'2)))
                | FStar_Tactics_Result.Failed (e, ps'1) ->
                    FStar_Tactics_Result.Failed (e, ps'1)))
      | FStar_Tactics_Result.Failed (e, ps') ->
          FStar_Tactics_Result.Failed (e, ps')
let (generateDecodeSerialize :
  FStar_Reflection_Types.fv ->
    (FStar_Reflection_Data.decls, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun name ->
    fun ps ->
      match (Data_Serialize_Rep.makeGenericRep name)
              (FStar_Tactics_Types.incr_depth
                 (FStar_Tactics_Types.set_proofstate_range ps
                    (FStar_Range.prims_to_fstar_range
                       (Prims.mk_range "Data.Serialize.Decode.fst"
                          (Prims.of_int (114)) (Prims.of_int (12))
                          (Prims.of_int (114)) (Prims.of_int (31))))))
      with
      | FStar_Tactics_Result.Success (a, ps') ->
          (match () with
           | () ->
               (generateDecodeSerialize_for_inductiveSumup a)
                 (FStar_Tactics_Types.decr_depth
                    (FStar_Tactics_Types.set_proofstate_range ps'
                       (FStar_Range.prims_to_fstar_range
                          (Prims.mk_range "Data.Serialize.Decode.fst"
                             (Prims.of_int (115)) (Prims.of_int (4))
                             (Prims.of_int (115)) (Prims.of_int (48)))))))
      | FStar_Tactics_Result.Failed (e, ps') ->
          FStar_Tactics_Result.Failed (e, ps')