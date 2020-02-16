open Prims

let (_UNSAFE_flag_is_compiled_rewriter_term : FStar_Reflection_Types.term) =
  FStar_Reflection_Basic.pack_ln
    (FStar_Reflection_Data.Tv_FVar
       (FStar_Reflection_Basic.pack_fv
          ["Data";
          "Serialize";
          "MakeNative";
          "_UNSAFE_flag_is_compiled_rewriter"]))
let (plugin_term : FStar_Reflection_Types.term) =
  FStar_Reflection_Basic.pack_ln
    (FStar_Reflection_Data.Tv_FVar
       (FStar_Reflection_Basic.pack_fv ["FStar"; "Pervasives"; "plugin"]))
let (mk_native_version :
  FStar_Reflection_Types.term ->
    Prims.string ->
      Prims.bool Prims.list ->
        Prims.bool ->
          (FStar_Reflection_Types.sigelt Prims.list, unit)
            FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun f ->
    fun nativeName ->
      fun args ->
        fun return ->
          fun ps ->
            match () with
            | () ->
                (match (Data_Serialize_Helpers.fvOf f)
                         (FStar_Tactics_Types.incr_depth
                            (FStar_Tactics_Types.set_proofstate_range
                               (FStar_Tactics_Types.decr_depth
                                  (FStar_Tactics_Types.set_proofstate_range
                                     (FStar_Tactics_Types.incr_depth
                                        (FStar_Tactics_Types.set_proofstate_range
                                           ps
                                           (FStar_Range.prims_to_fstar_range
                                              (Prims.mk_range
                                                 "Data.Serialize.MakeNative.fst"
                                                 (Prims.of_int (23))
                                                 (Prims.of_int (11))
                                                 (Prims.of_int (23))
                                                 (Prims.of_int (12))))))
                                     (FStar_Range.prims_to_fstar_range
                                        (Prims.mk_range
                                           "Data.Serialize.MakeNative.fst"
                                           (Prims.of_int (24))
                                           (Prims.of_int (2))
                                           (Prims.of_int (86))
                                           (Prims.of_int (3))))))
                               (FStar_Range.prims_to_fstar_range
                                  (Prims.mk_range
                                     "Data.Serialize.MakeNative.fst"
                                     (Prims.of_int (24)) (Prims.of_int (14))
                                     (Prims.of_int (24)) (Prims.of_int (20))))))
                 with
                 | FStar_Tactics_Result.Success (a, ps') ->
                     (match () with
                      | () ->
                          (match (Data_Serialize_Helpers.nameCurMod'
                                    (FStar_Reflection_Basic.inspect_fv a)
                                    (fun uu____57952 ->
                                       "flag_is_in_compiled_context"))
                                   (FStar_Tactics_Types.incr_depth
                                      (FStar_Tactics_Types.set_proofstate_range
                                         (FStar_Tactics_Types.decr_depth
                                            (FStar_Tactics_Types.set_proofstate_range
                                               ps'
                                               (FStar_Range.prims_to_fstar_range
                                                  (Prims.mk_range
                                                     "Data.Serialize.MakeNative.fst"
                                                     (Prims.of_int (25))
                                                     (Prims.of_int (2))
                                                     (Prims.of_int (86))
                                                     (Prims.of_int (3))))))
                                         (FStar_Range.prims_to_fstar_range
                                            (Prims.mk_range
                                               "Data.Serialize.MakeNative.fst"
                                               (Prims.of_int (25))
                                               (Prims.of_int (19))
                                               (Prims.of_int (25))
                                               (Prims.of_int (86))))))
                           with
                           | FStar_Tactics_Result.Success (a1, ps'1) ->
                               (match () with
                                | () ->
                                    (match (FStar_Tactics_Builtins.pack
                                              (FStar_Reflection_Data.Tv_FVar
                                                 (FStar_Reflection_Basic.pack_fv
                                                    a1)))
                                             (FStar_Tactics_Types.incr_depth
                                                (FStar_Tactics_Types.set_proofstate_range
                                                   (FStar_Tactics_Types.decr_depth
                                                      (FStar_Tactics_Types.set_proofstate_range
                                                         ps'1
                                                         (FStar_Range.prims_to_fstar_range
                                                            (Prims.mk_range
                                                               "Data.Serialize.MakeNative.fst"
                                                               (Prims.of_int (26))
                                                               (Prims.of_int (2))
                                                               (Prims.of_int (86))
                                                               (Prims.of_int (3))))))
                                                   (FStar_Range.prims_to_fstar_range
                                                      (Prims.mk_range
                                                         "Data.Serialize.MakeNative.fst"
                                                         (Prims.of_int (26))
                                                         (Prims.of_int (19))
                                                         (Prims.of_int (26))
                                                         (Prims.of_int (48))))))
                                     with
                                     | FStar_Tactics_Result.Success
                                         (a2, ps'2) ->
                                         (match () with
                                          | () ->
                                              (match (Data_Serialize_Helpers.nameCurMod'
                                                        (FStar_Reflection_Basic.inspect_fv
                                                           a)
                                                        (fun uu____57979 ->
                                                           "is_this_module_compiled"))
                                                       (FStar_Tactics_Types.incr_depth
                                                          (FStar_Tactics_Types.set_proofstate_range
                                                             (FStar_Tactics_Types.decr_depth
                                                                (FStar_Tactics_Types.set_proofstate_range
                                                                   ps'2
                                                                   (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (27))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                             (FStar_Range.prims_to_fstar_range
                                                                (Prims.mk_range
                                                                   "Data.Serialize.MakeNative.fst"
                                                                   (Prims.of_int (27))
                                                                   (Prims.of_int (38))
                                                                   (Prims.of_int (27))
                                                                   (Prims.of_int (101))))))
                                               with
                                               | FStar_Tactics_Result.Success
                                                   (a3, ps'3) ->
                                                   (match () with
                                                    | () ->
                                                        (match (FStar_Tactics_Builtins.pack
                                                                  (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    a3)))
                                                                 (FStar_Tactics_Types.incr_depth
                                                                    (
                                                                    FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (28))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (28))
                                                                    (Prims.of_int (38))
                                                                    (Prims.of_int (28))
                                                                    (Prims.of_int (86))))))
                                                         with
                                                         | FStar_Tactics_Result.Success
                                                             (a4, ps'4) ->
                                                             (match () with
                                                              | () ->
                                                                  (match 
                                                                    match 
                                                                    match 
                                                                    (FStar_Tactics_Builtins.top_env
                                                                    ())
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
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (22))
                                                                    (Prims.of_int (42))
                                                                    (Prims.of_int (5))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (28))
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (56))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (39))
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (51))))))
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
                                                                    ((FStar_Reflection_Basic.lookup_typ
                                                                    a5 a1),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (28))
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (56))))))))
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
                                                                    ((match a5
                                                                    with
                                                                    | FStar_Pervasives_Native.Some
                                                                    uu____58032
                                                                    ->
                                                                    (fun s ->
                                                                    FStar_Tactics_Result.Success
                                                                    ([], s))
                                                                    | FStar_Pervasives_Native.None
                                                                    ->
                                                                    (fun ps1
                                                                    ->
                                                                    match 
                                                                    match 
                                                                    match 
                                                                    match 
                                                                    match 
                                                                    match 
                                                                    match 
                                                                    (FStar_Tactics_Derived.fresh_binder
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Prims";
                                                                    "unit"]))))
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
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (32))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (11))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (37))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (10))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (38))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (10))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (38))
                                                                    (Prims.of_int (21))
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (9))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (40))
                                                                    (Prims.of_int (12))
                                                                    (Prims.of_int (40))
                                                                    (Prims.of_int (49))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (40))
                                                                    (Prims.of_int (20))
                                                                    (Prims.of_int (40))
                                                                    (Prims.of_int (42))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (40))
                                                                    (Prims.of_int (21))
                                                                    (Prims.of_int (40))
                                                                    (Prims.of_int (41))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a6,
                                                                    ps'6) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ([a6],
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'6
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (40))
                                                                    (Prims.of_int (20))
                                                                    (Prims.of_int (40))
                                                                    (Prims.of_int (42))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a6,
                                                                    ps'6) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (Data_Serialize_Helpers.mk_abs
                                                                    a6 a2)
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'6
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (40))
                                                                    (Prims.of_int (12))
                                                                    (Prims.of_int (40))
                                                                    (Prims.of_int (49)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a6,
                                                                    ps'6) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Data.Sg_Let
                                                                    (false,
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    a3), [],
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_Arrow
                                                                    ((FStar_Reflection_Basic.pack_binder
                                                                    (FStar_Reflection_Basic.pack_bv
                                                                    {
                                                                    FStar_Reflection_Data.bv_ppname
                                                                    = "_";
                                                                    FStar_Reflection_Data.bv_index
                                                                    =
                                                                    Prims.int_zero;
                                                                    FStar_Reflection_Data.bv_sort
                                                                    =
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Prims";
                                                                    "unit"])))
                                                                    })
                                                                    FStar_Reflection_Data.Q_Explicit),
                                                                    (FStar_Reflection_Basic.pack_comp
                                                                    (FStar_Reflection_Data.C_Total
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Prims";
                                                                    "bool"]))),
                                                                    FStar_Pervasives_Native.None)))))),
                                                                    a6)),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'6
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (38))
                                                                    (Prims.of_int (21))
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (9))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a6,
                                                                    ps'6) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Basic.pack_sigelt
                                                                    a6),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'6
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (38))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (10))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a6,
                                                                    ps'6) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Basic.set_sigelt_attrs
                                                                    [
                                                                    FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["FStar";
                                                                    "Pervasives";
                                                                    "plugin"]))]
                                                                    a6),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'6
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (37))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (10))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a6,
                                                                    ps'6) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ([a6],
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'6
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (32))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (11))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a6,
                                                                    ps'6) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    (((
                                                                    FStar_Reflection_Basic.set_sigelt_attrs
                                                                    [
                                                                    FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["FStar";
                                                                    "Tactics";
                                                                    "Effect";
                                                                    "postprocess_for_extraction_with"]))),
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_Abs
                                                                    ((FStar_Reflection_Basic.pack_binder
                                                                    (FStar_Reflection_Basic.pack_bv
                                                                    {
                                                                    FStar_Reflection_Data.bv_ppname
                                                                    = "uu___";
                                                                    FStar_Reflection_Data.bv_index
                                                                    =
                                                                    (Prims.of_int (17));
                                                                    FStar_Reflection_Data.bv_sort
                                                                    =
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    FStar_Reflection_Data.Tv_Unknown)
                                                                    })
                                                                    FStar_Reflection_Data.Q_Explicit),
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["FStar";
                                                                    "Tactics";
                                                                    "Builtins";
                                                                    "apply_lemma"]))),
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Data";
                                                                    "Serialize";
                                                                    "MakeNative";
                                                                    "_UNSAFE_flag_is_compiled_rewriter_term"]))),
                                                                    FStar_Reflection_Data.Q_Explicit))))))),
                                                                    FStar_Reflection_Data.Q_Explicit)))]
                                                                    (FStar_Reflection_Basic.pack_sigelt
                                                                    (FStar_Reflection_Data.Sg_Let
                                                                    (false,
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    a1), [],
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Prims";
                                                                    "bool"]))),
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_Const
                                                                    FStar_Reflection_Data.C_False))))))
                                                                    :: a6),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'6
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (31))
                                                                    (Prims.of_int (16))
                                                                    (Prims.of_int (42))
                                                                    (Prims.of_int (5))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6))))
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (22))
                                                                    (Prims.of_int (42))
                                                                    (Prims.of_int (5)))))))
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
                                                                    match 
                                                                    (FStar_Tactics_Builtins.top_env
                                                                    ())
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'5
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (44))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (44))
                                                                    (Prims.of_int (11))
                                                                    (Prims.of_int (44))
                                                                    (Prims.of_int (39))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (44))
                                                                    (Prims.of_int (22))
                                                                    (Prims.of_int (44))
                                                                    (Prims.of_int (34))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a6,
                                                                    ps'6) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Basic.lookup_typ
                                                                    a6 a1),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'6
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (44))
                                                                    (Prims.of_int (11))
                                                                    (Prims.of_int (44))
                                                                    (Prims.of_int (39))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'6)
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
                                                                    (FStar_Tactics_Util.map
                                                                    (fun t ->
                                                                    fun ps1
                                                                    ->
                                                                    match 
                                                                    (FStar_Tactics_Derived.fresh_binder
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    FStar_Reflection_Data.Tv_Unknown))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (26))
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (43))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a7,
                                                                    ps'7) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((a7, t),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'7
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (26))
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (46))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'7)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'7))
                                                                    args)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'6
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (12))
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (52))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a7,
                                                                    ps'7) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    match 
                                                                    (FStar_Tactics_Util.map
                                                                    (fun
                                                                    uu____58398
                                                                    ->
                                                                    match uu____58398
                                                                    with
                                                                    | 
                                                                    (b, isS)
                                                                    ->
                                                                    if isS
                                                                    then
                                                                    (fun ps1
                                                                    ->
                                                                    match 
                                                                    (FStar_Tactics_Derived.binder_to_term
                                                                    b)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (32))
                                                                    (Prims.of_int (50))
                                                                    (Prims.of_int (50))))))
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
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Data";
                                                                    "Serialize";
                                                                    "Typeclasses";
                                                                    "deserialize"]))),
                                                                    (a8,
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
                                                                    (e, ps'8))
                                                                    else
                                                                    FStar_Tactics_Derived.binder_to_term
                                                                    b) a7)
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
                                                                    ps'7
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (46))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (46))
                                                                    (Prims.of_int (16))
                                                                    (Prims.of_int (46))
                                                                    (Prims.of_int (29))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (13))
                                                                    (Prims.of_int (53))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (25))
                                                                    (Prims.of_int (53))
                                                                    (Prims.of_int (3))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a8,
                                                                    ps'8) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Derived.mk_e_app
                                                                    f a8),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'8
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (13))
                                                                    (Prims.of_int (53))
                                                                    (Prims.of_int (3))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'8)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'8)
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a8,
                                                                    ps'8) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    (if
                                                                    return
                                                                    then
                                                                    FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Data";
                                                                    "Serialize";
                                                                    "Typeclasses";
                                                                    "serialize"]))),
                                                                    (a8,
                                                                    FStar_Reflection_Data.Q_Explicit)))
                                                                    else
                                                                    (fun s ->
                                                                    FStar_Tactics_Result.Success
                                                                    (a8, s)))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'8
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (54))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (54))
                                                                    (Prims.of_int (13))
                                                                    (Prims.of_int (54))
                                                                    (Prims.of_int (61))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a9,
                                                                    ps'9) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    (Data_Serialize_Helpers.nameCurMod'
                                                                    (FStar_Reflection_Basic.inspect_fv
                                                                    a)
                                                                    (fun s ->
                                                                    Prims.strcat
                                                                    s
                                                                    (Prims.strcat
                                                                    "_"
                                                                    (Prims.strcat
                                                                    nativeName
                                                                    "_native_helper"))))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'9
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (55))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (55))
                                                                    (Prims.of_int (17))
                                                                    (Prims.of_int (55))
                                                                    (Prims.of_int (94))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a10,
                                                                    ps'10) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    match 
                                                                    (FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    a10)))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'10
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (38))
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (67))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a11,
                                                                    ps'11) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    (FStar_Tactics_Util.map
                                                                    (fun
                                                                    uu____58558
                                                                    ->
                                                                    match uu____58558
                                                                    with
                                                                    | 
                                                                    (b, isS)
                                                                    ->
                                                                    if isS
                                                                    then
                                                                    (fun ps1
                                                                    ->
                                                                    match 
                                                                    (FStar_Tactics_Derived.binder_to_term
                                                                    b)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (59))
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (59))
                                                                    (Prims.of_int (48))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a12,
                                                                    ps'12) ->
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
                                                                    "serialize"]))),
                                                                    (a12,
                                                                    FStar_Reflection_Data.Q_Explicit))))
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'12
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (89))
                                                                    (Prims.of_int (37)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'12) ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'12))
                                                                    else
                                                                    FStar_Tactics_Derived.binder_to_term
                                                                    b) a7)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'11
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (68))
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (3))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a12,
                                                                    ps'12) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Derived.mk_e_app
                                                                    a11 a12),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'12
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (56))
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (62))
                                                                    (Prims.of_int (3))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'12) ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'12)))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'11) ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'11)
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a11,
                                                                    ps'11) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    (if
                                                                    return
                                                                    then
                                                                    FStar_Tactics_Builtins.pack
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Data";
                                                                    "Serialize";
                                                                    "Typeclasses";
                                                                    "deserialize"]))),
                                                                    (a11,
                                                                    FStar_Reflection_Data.Q_Explicit)))
                                                                    else
                                                                    (fun s ->
                                                                    FStar_Tactics_Result.Success
                                                                    (a11, s)))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'11
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (63))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (63))
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (63))
                                                                    (Prims.of_int (99))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a12,
                                                                    ps'12) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    (Data_Serialize_Helpers.nameCurMod'
                                                                    (FStar_Reflection_Basic.inspect_fv
                                                                    a)
                                                                    (fun s ->
                                                                    nativeName))
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'12
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (64))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (69))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (69))
                                                                    (Prims.of_int (22))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (73))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (73))
                                                                    (Prims.of_int (18))
                                                                    (Prims.of_int (73))
                                                                    (Prims.of_int (66))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a13,
                                                                    ps'13) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    match 
                                                                    (Data_Serialize_Helpers.mk_abs
                                                                    (FStar_List_Tot_Base.map
                                                                    FStar_Pervasives_Native.fst
                                                                    a7) a9)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'13
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (74))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (75))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (76))
                                                                    (Prims.of_int (37))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (76))
                                                                    (Prims.of_int (16))
                                                                    (Prims.of_int (76))
                                                                    (Prims.of_int (37))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a14,
                                                                    ps'14) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Data.Sg_Let
                                                                    (false,
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    a10), [],
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    FStar_Reflection_Data.Tv_Unknown),
                                                                    a14)),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'14
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (75))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (76))
                                                                    (Prims.of_int (37))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'14) ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'14)
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a14,
                                                                    ps'14) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    match 
                                                                    match 
                                                                    (FStar_Tactics_Builtins.top_env
                                                                    ())
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'14
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (77))
                                                                    (Prims.of_int (5))
                                                                    (Prims.of_int (86))
                                                                    (Prims.of_int (3))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (80))
                                                                    (Prims.of_int (33))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (79))
                                                                    (Prims.of_int (11))
                                                                    (Prims.of_int (79))
                                                                    (Prims.of_int (31))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (79))
                                                                    (Prims.of_int (15))
                                                                    (Prims.of_int (79))
                                                                    (Prims.of_int (27))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a15,
                                                                    ps'15) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (FStar_Tactics_Builtins.tc
                                                                    a15 f)
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'15
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (79))
                                                                    (Prims.of_int (11))
                                                                    (Prims.of_int (79))
                                                                    (Prims.of_int (31)))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'15) ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'15)
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a15,
                                                                    ps'15) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    (Data_Serialize_Helpers.mk_abs
                                                                    (FStar_List_Tot_Base.map
                                                                    FStar_Pervasives_Native.fst
                                                                    a7) a12)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'15
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (80))
                                                                    (Prims.of_int (33))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (80))
                                                                    (Prims.of_int (11))
                                                                    (Prims.of_int (80))
                                                                    (Prims.of_int (33))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a16,
                                                                    ps'16) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Data.Sg_Let
                                                                    (false,
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    a13), [],
                                                                    a15, a16)),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'16
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (78))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (80))
                                                                    (Prims.of_int (33))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'16) ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'16)))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'15) ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'15)
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a15,
                                                                    ps'15) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_List_Tot_Base.append
                                                                    a5
                                                                    [
                                                                    FStar_Reflection_Basic.set_sigelt_attrs
                                                                    [plugin_term]
                                                                    (FStar_Reflection_Basic.pack_sigelt
                                                                    a14);
                                                                    FStar_Reflection_Basic.pack_sigelt
                                                                    a15]),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'15
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.MakeNative.fst"
                                                                    (Prims.of_int (81))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (84))
                                                                    (Prims.of_int (2))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'15) ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'15)))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'14) ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'14)))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'13) ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'13))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'12) ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'12)))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'11) ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'11)))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'10) ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e,
                                                                    ps'10)))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'9)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'9)))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'8)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'8))))
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
                                                                   | 
                                                                   FStar_Tactics_Result.Failed
                                                                    (e, ps'5)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'5)))
                                                         | FStar_Tactics_Result.Failed
                                                             (e, ps'4) ->
                                                             FStar_Tactics_Result.Failed
                                                               (e, ps'4)))
                                               | FStar_Tactics_Result.Failed
                                                   (e, ps'3) ->
                                                   FStar_Tactics_Result.Failed
                                                     (e, ps'3)))
                                     | FStar_Tactics_Result.Failed (e, ps'2)
                                         ->
                                         FStar_Tactics_Result.Failed
                                           (e, ps'2)))
                           | FStar_Tactics_Result.Failed (e, ps'1) ->
                               FStar_Tactics_Result.Failed (e, ps'1)))
                 | FStar_Tactics_Result.Failed (e, ps') ->
                     FStar_Tactics_Result.Failed (e, ps'))