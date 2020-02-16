open Prims
let rec (makeGenericRep'Cons'Arg :
  Prims.nat ->
    FStar_Reflection_Types.bv Prims.list ->
      FStar_Reflection_Types.term ->
        (unit Data_Serialize_Types.argSumup, unit)
          FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun iVars ->
    fun bvs ->
      fun t ->
        fun ps ->
          match () with
          | () ->
              ((match FStar_Reflection_Derived.collect_app t with
                | (f, args) ->
                    (fun ps1 ->
                       match () with
                       | () ->
                           (match (FStar_Tactics_Builtins.inspect f)
                                    (FStar_Tactics_Types.incr_depth
                                       (FStar_Tactics_Types.set_proofstate_range
                                          (FStar_Tactics_Types.decr_depth
                                             (FStar_Tactics_Types.set_proofstate_range
                                                (FStar_Tactics_Types.incr_depth
                                                   (FStar_Tactics_Types.set_proofstate_range
                                                      ps1
                                                      (FStar_Range.prims_to_fstar_range
                                                         (Prims.mk_range
                                                            "Data.Serialize.Rep.fst"
                                                            (Prims.of_int (18))
                                                            (Prims.of_int (10))
                                                            (Prims.of_int (18))
                                                            (Prims.of_int (43))))))
                                                (FStar_Range.prims_to_fstar_range
                                                   (Prims.mk_range
                                                      "Data.Serialize.Rep.fst"
                                                      (Prims.of_int (19))
                                                      (Prims.of_int (2))
                                                      (Prims.of_int (30))
                                                      (Prims.of_int (103))))))
                                          (FStar_Range.prims_to_fstar_range
                                             (Prims.mk_range
                                                "Data.Serialize.Rep.fst"
                                                (Prims.of_int (17))
                                                (Prims.of_int (6))
                                                (Prims.of_int (17))
                                                (Prims.of_int (7))))))
                            with
                            | FStar_Tactics_Result.Success (a, ps') ->
                                (match () with
                                 | () ->
                                     ((match a with
                                       | FStar_Reflection_Data.Tv_FVar x ->
                                           (match ((FStar_Reflection_Basic.inspect_fv
                                                      x), args)
                                            with
                                            | ("Prims"::"list"::[],
                                               (arg, uu____18962)::[]) ->
                                                (fun ps2 ->
                                                   match (makeGenericRep'Cons'Arg
                                                            iVars bvs arg)
                                                           (FStar_Tactics_Types.incr_depth
                                                              (FStar_Tactics_Types.set_proofstate_range
                                                                 ps2
                                                                 (FStar_Range.prims_to_fstar_range
                                                                    (
                                                                    Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (22))
                                                                    (Prims.of_int (44))
                                                                    (Prims.of_int (22))
                                                                    (Prims.of_int (51))))))
                                                   with
                                                   | FStar_Tactics_Result.Success
                                                       (a1, ps'1) ->
                                                       (match () with
                                                        | () ->
                                                            FStar_Tactics_Result.Success
                                                              ((Data_Serialize_Types.AS_List
                                                                  a1),
                                                                (FStar_Tactics_Types.decr_depth
                                                                   (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (22))
                                                                    (Prims.of_int (36))
                                                                    (Prims.of_int (22))
                                                                    (Prims.of_int (51))))))))
                                                   | FStar_Tactics_Result.Failed
                                                       (e, ps'1) ->
                                                       FStar_Tactics_Result.Failed
                                                         (e, ps'1))
                                            | ("Prims"::"int"::[],
                                               uu____19044) ->
                                                (fun s ->
                                                   FStar_Tactics_Result.Success
                                                     (Data_Serialize_Types.AS_Int,
                                                       s))
                                            | ("Prims"::"bool"::[],
                                               uu____19076) ->
                                                (fun s ->
                                                   FStar_Tactics_Result.Success
                                                     (Data_Serialize_Types.AS_Bool,
                                                       s))
                                            | ("Prims"::"string"::[],
                                               uu____19108) ->
                                                (fun s ->
                                                   FStar_Tactics_Result.Success
                                                     (Data_Serialize_Types.AS_String,
                                                       s))
                                            | (n1, uu____19141) ->
                                                (fun ps2 ->
                                                   match (FStar_Tactics_Util.map
                                                            (fun uu____19197
                                                               ->
                                                               match uu____19197
                                                               with
                                                               | (x1,
                                                                  uu____19207)
                                                                   ->
                                                                   makeGenericRep'Cons'Arg
                                                                    iVars bvs
                                                                    x1) args)
                                                           (FStar_Tactics_Types.incr_depth
                                                              (FStar_Tactics_Types.set_proofstate_range
                                                                 ps2
                                                                 (FStar_Range.prims_to_fstar_range
                                                                    (
                                                                    Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (26))
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (26))
                                                                    (Prims.of_int (59))))))
                                                   with
                                                   | FStar_Tactics_Result.Success
                                                       (a1, ps'1) ->
                                                       (match () with
                                                        | () ->
                                                            FStar_Tactics_Result.Success
                                                              ((Data_Serialize_Types.AS_Inductive
                                                                  (n1, a1)),
                                                                (FStar_Tactics_Types.decr_depth
                                                                   (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (26))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (26))
                                                                    (Prims.of_int (59))))))))
                                                   | FStar_Tactics_Result.Failed
                                                       (e, ps'1) ->
                                                       FStar_Tactics_Result.Failed
                                                         (e, ps'1)))
                                       | FStar_Reflection_Data.Tv_Refine
                                           (bv, ref) ->
                                           makeGenericRep'Cons'Arg iVars bvs
                                             (FStar_Reflection_Basic.inspect_bv
                                                bv).FStar_Reflection_Data.bv_sort
                                       | FStar_Reflection_Data.Tv_Var x ->
                                           (fun ps2 ->
                                              match (if
                                                       iVars = Prims.int_zero
                                                     then
                                                       FStar_Tactics_Derived.fail
                                                         "found Tv_BVar while no type variable are bound"
                                                     else
                                                       (fun ps3 ->
                                                          match (Data_Serialize_Helpers.findIndex
                                                                   x bvs)
                                                                  (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (115))
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (130))))))
                                                          with
                                                          | FStar_Tactics_Result.Success
                                                              (a1, ps'1) ->
                                                              (match () with
                                                               | () ->
                                                                   FStar_Tactics_Result.Success
                                                                    ((a1 mod
                                                                    iVars),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (114))
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (139))))))))
                                                          | FStar_Tactics_Result.Failed
                                                              (e, ps'1) ->
                                                              FStar_Tactics_Result.Failed
                                                                (e, ps'1)))
                                                      (FStar_Tactics_Types.incr_depth
                                                         (FStar_Tactics_Types.set_proofstate_range
                                                            ps2
                                                            (FStar_Range.prims_to_fstar_range
                                                               (Prims.mk_range
                                                                  "Data.Serialize.Rep.fst"
                                                                  (Prims.of_int (29))
                                                                  (Prims.of_int (36))
                                                                  (Prims.of_int (29))
                                                                  (Prims.of_int (140))))))
                                              with
                                              | FStar_Tactics_Result.Success
                                                  (a1, ps'1) ->
                                                  (match () with
                                                   | () ->
                                                       FStar_Tactics_Result.Success
                                                         ((Data_Serialize_Types.AS_TVar
                                                             a1),
                                                           (FStar_Tactics_Types.decr_depth
                                                              (FStar_Tactics_Types.set_proofstate_range
                                                                 ps'1
                                                                 (FStar_Range.prims_to_fstar_range
                                                                    (
                                                                    Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (28))
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (140))))))))
                                              | FStar_Tactics_Result.Failed
                                                  (e, ps'1) ->
                                                  FStar_Tactics_Result.Failed
                                                    (e, ps'1))
                                       | FStar_Reflection_Data.Tv_BVar x ->
                                           (fun ps2 ->
                                              match (if
                                                       iVars = Prims.int_zero
                                                     then
                                                       FStar_Tactics_Derived.fail
                                                         "found Tv_BVar while no type variable are bound"
                                                     else
                                                       (fun ps3 ->
                                                          match (Data_Serialize_Helpers.findIndex
                                                                   x bvs)
                                                                  (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (115))
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (130))))))
                                                          with
                                                          | FStar_Tactics_Result.Success
                                                              (a1, ps'1) ->
                                                              (match () with
                                                               | () ->
                                                                   FStar_Tactics_Result.Success
                                                                    ((a1 mod
                                                                    iVars),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (114))
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (139))))))))
                                                          | FStar_Tactics_Result.Failed
                                                              (e, ps'1) ->
                                                              FStar_Tactics_Result.Failed
                                                                (e, ps'1)))
                                                      (FStar_Tactics_Types.incr_depth
                                                         (FStar_Tactics_Types.set_proofstate_range
                                                            ps2
                                                            (FStar_Range.prims_to_fstar_range
                                                               (Prims.mk_range
                                                                  "Data.Serialize.Rep.fst"
                                                                  (Prims.of_int (29))
                                                                  (Prims.of_int (36))
                                                                  (Prims.of_int (29))
                                                                  (Prims.of_int (140))))))
                                              with
                                              | FStar_Tactics_Result.Success
                                                  (a1, ps'1) ->
                                                  (match () with
                                                   | () ->
                                                       FStar_Tactics_Result.Success
                                                         ((Data_Serialize_Types.AS_TVar
                                                             a1),
                                                           (FStar_Tactics_Types.decr_depth
                                                              (FStar_Tactics_Types.set_proofstate_range
                                                                 ps'1
                                                                 (FStar_Range.prims_to_fstar_range
                                                                    (
                                                                    Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (28))
                                                                    (Prims.of_int (29))
                                                                    (Prims.of_int (140))))))))
                                              | FStar_Tactics_Result.Failed
                                                  (e, ps'1) ->
                                                  FStar_Tactics_Result.Failed
                                                    (e, ps'1))
                                       | tv ->
                                           (fun ps2 ->
                                              match match match match 
                                                                  match ()
                                                                  with
                                                                  | () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((FStar_Reflection_Basic.term_to_string
                                                                    (Obj.magic
                                                                    (failwith
                                                                    "Cannot evaluate open quotation at runtime"))),
                                                                    (FStar_Tactics_Types.decr_depth
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
                                                                    ps2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (15))
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (103))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (43))
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (102))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (73))
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (102))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (77))
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (102))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (92))
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (102))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (77))
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (102)))))))
                                                                with
                                                                | FStar_Tactics_Result.Success
                                                                    (a1,
                                                                    ps'1) ->
                                                                    (
                                                                    match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((Prims.strcat
                                                                    ", that is "
                                                                    a1),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "/nix/store/pwh4c0rf5hg43dhc796z15gjk6dxrx6c-home-lucas-fstar/ulib/prims.fst"
                                                                    (Prims.of_int (352))
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (352))
                                                                    (Prims.of_int (57))))))))
                                                                | FStar_Tactics_Result.Failed
                                                                    (e, ps'1)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'1)
                                                          with
                                                          | FStar_Tactics_Result.Success
                                                              (a1, ps'1) ->
                                                              (match () with
                                                               | () ->
                                                                   FStar_Tactics_Result.Success
                                                                    ((Prims.strcat
                                                                    (FStar_Reflection_Basic.term_to_string
                                                                    f) a1),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "/nix/store/pwh4c0rf5hg43dhc796z15gjk6dxrx6c-home-lucas-fstar/ulib/prims.fst"
                                                                    (Prims.of_int (352))
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (352))
                                                                    (Prims.of_int (57))))))))
                                                          | FStar_Tactics_Result.Failed
                                                              (e, ps'1) ->
                                                              FStar_Tactics_Result.Failed
                                                                (e, ps'1)
                                                    with
                                                    | FStar_Tactics_Result.Success
                                                        (a1, ps'1) ->
                                                        (match () with
                                                         | () ->
                                                             FStar_Tactics_Result.Success
                                                               ((Prims.strcat
                                                                   "DUNNO HOW TO ACT WITH "
                                                                   a1),
                                                                 (FStar_Tactics_Types.decr_depth
                                                                    (
                                                                    FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "/nix/store/pwh4c0rf5hg43dhc796z15gjk6dxrx6c-home-lucas-fstar/ulib/prims.fst"
                                                                    (Prims.of_int (352))
                                                                    (Prims.of_int (45))
                                                                    (Prims.of_int (352))
                                                                    (Prims.of_int (57))))))))
                                                    | FStar_Tactics_Result.Failed
                                                        (e, ps'1) ->
                                                        FStar_Tactics_Result.Failed
                                                          (e, ps'1)
                                              with
                                              | FStar_Tactics_Result.Success
                                                  (a1, ps'1) ->
                                                  (match () with
                                                   | () ->
                                                       (FStar_Tactics_Derived.fail
                                                          a1)
                                                         (FStar_Tactics_Types.decr_depth
                                                            (FStar_Tactics_Types.set_proofstate_range
                                                               ps'1
                                                               (FStar_Range.prims_to_fstar_range
                                                                  (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (10))
                                                                    (Prims.of_int (30))
                                                                    (Prims.of_int (103)))))))
                                              | FStar_Tactics_Result.Failed
                                                  (e, ps'1) ->
                                                  FStar_Tactics_Result.Failed
                                                    (e, ps'1))))
                                       (FStar_Tactics_Types.decr_depth
                                          (FStar_Tactics_Types.set_proofstate_range
                                             ps'
                                             (FStar_Range.prims_to_fstar_range
                                                (Prims.mk_range
                                                   "Data.Serialize.Rep.fst"
                                                   (Prims.of_int (19))
                                                   (Prims.of_int (2))
                                                   (Prims.of_int (30))
                                                   (Prims.of_int (103)))))))
                            | FStar_Tactics_Result.Failed (e, ps') ->
                                FStar_Tactics_Result.Failed (e, ps')))))
                (FStar_Tactics_Types.decr_depth
                   (FStar_Tactics_Types.set_proofstate_range
                      (FStar_Tactics_Types.incr_depth
                         (FStar_Tactics_Types.set_proofstate_range ps
                            (FStar_Range.prims_to_fstar_range
                               (Prims.mk_range "Data.Serialize.Rep.fst"
                                  (Prims.of_int (17)) (Prims.of_int (16))
                                  (Prims.of_int (17)) (Prims.of_int (29))))))
                      (FStar_Range.prims_to_fstar_range
                         (Prims.mk_range "Data.Serialize.Rep.fst"
                            (Prims.of_int (17)) (Prims.of_int (2))
                            (Prims.of_int (30)) (Prims.of_int (103))))))
let (makeGenericRep'Cons :
  Prims.nat ->
    FStar_Reflection_Types.name ->
      (unit Data_Serialize_Types.consSumup, unit)
        FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun iVars ->
    fun name ->
      fun ps ->
        match match (FStar_Tactics_Builtins.top_env ())
                      (FStar_Tactics_Types.incr_depth
                         (FStar_Tactics_Types.set_proofstate_range
                            (FStar_Tactics_Types.incr_depth
                               (FStar_Tactics_Types.set_proofstate_range ps
                                  (FStar_Range.prims_to_fstar_range
                                     (Prims.mk_range "Data.Serialize.Rep.fst"
                                        (Prims.of_int (33))
                                        (Prims.of_int (10))
                                        (Prims.of_int (33))
                                        (Prims.of_int (39))))))
                            (FStar_Range.prims_to_fstar_range
                               (Prims.mk_range "Data.Serialize.Rep.fst"
                                  (Prims.of_int (33)) (Prims.of_int (22))
                                  (Prims.of_int (33)) (Prims.of_int (34))))))
              with
              | FStar_Tactics_Result.Success (a, ps') ->
                  (match () with
                   | () ->
                       (Data_Serialize_Helpers.lookup_typ' a name)
                         (FStar_Tactics_Types.decr_depth
                            (FStar_Tactics_Types.set_proofstate_range ps'
                               (FStar_Range.prims_to_fstar_range
                                  (Prims.mk_range "Data.Serialize.Rep.fst"
                                     (Prims.of_int (33)) (Prims.of_int (10))
                                     (Prims.of_int (33)) (Prims.of_int (39)))))))
              | FStar_Tactics_Result.Failed (e, ps') ->
                  FStar_Tactics_Result.Failed (e, ps')
        with
        | FStar_Tactics_Result.Success (a, ps') ->
            (match () with
             | () ->
                 ((match FStar_Reflection_Basic.inspect_sigelt a with
                   | FStar_Reflection_Data.Sg_Constructor (uu____19845, typ)
                       ->
                       (fun ps1 ->
                          match (FStar_Tactics_SyntaxHelpers.collect_arr_bs
                                   typ)
                                  (FStar_Tactics_Types.incr_depth
                                     (FStar_Tactics_Types.set_proofstate_range
                                        ps1
                                        (FStar_Range.prims_to_fstar_range
                                           (Prims.mk_range
                                              "Data.Serialize.Rep.fst"
                                              (Prims.of_int (36))
                                              (Prims.of_int (27))
                                              (Prims.of_int (36))
                                              (Prims.of_int (45))))))
                          with
                          | FStar_Tactics_Result.Success (a1, ps'1) ->
                              (match () with
                               | () ->
                                   ((match a1 with
                                     | (args, comp') ->
                                         (fun ps2 ->
                                            match (FStar_Tactics_Derived.guard
                                                     (FStar_Reflection_Data.uu___is_C_Total
                                                        (FStar_Reflection_Basic.inspect_comp
                                                           comp')))
                                                    (FStar_Tactics_Types.incr_depth
                                                       (FStar_Tactics_Types.set_proofstate_range
                                                          ps2
                                                          (FStar_Range.prims_to_fstar_range
                                                             (Prims.mk_range
                                                                "Data.Serialize.Rep.fst"
                                                                (Prims.of_int (37))
                                                                (Prims.of_int (8))
                                                                (Prims.of_int (37))
                                                                (Prims.of_int (45))))))
                                            with
                                            | FStar_Tactics_Result.Success
                                                (a2, ps'2) ->
                                                (match () with
                                                 | () ->
                                                     (match () with
                                                      | () ->
                                                          ((match FStar_List_Tot_Base.splitAt
                                                                    iVars
                                                                    args
                                                            with
                                                            | (tvars, args1)
                                                                ->
                                                                (fun ps3 ->
                                                                   match ()
                                                                   with
                                                                   | 
                                                                   () ->
                                                                    (match 
                                                                    (FStar_Tactics_Util.map
                                                                    (fun x ->
                                                                    makeGenericRep'Cons'Arg
                                                                    iVars
                                                                    (FStar_List_Tot_Base.map
                                                                    FStar_Reflection_Derived.bv_of_binder
                                                                    tvars)
                                                                    (FStar_Reflection_Derived.type_of_binder
                                                                    x)) args1)
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (39))
                                                                    (Prims.of_int (24))
                                                                    (Prims.of_int (39))
                                                                    (Prims.of_int (48))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (92))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (92))))))
                                                                    with
                                                                    | 
                                                                    FStar_Tactics_Result.Success
                                                                    (a3,
                                                                    ps'3) ->
                                                                    (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    FStar_Tactics_Result.Success
                                                                    ((name,
                                                                    a3),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (92))))))))
                                                                    | 
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'3)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'3)))))
                                                            (FStar_Tactics_Types.decr_depth
                                                               (FStar_Tactics_Types.set_proofstate_range
                                                                  (FStar_Tactics_Types.incr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (38))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (92))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (38))
                                                                    (Prims.of_int (26))
                                                                    (Prims.of_int (38))
                                                                    (Prims.of_int (46))))))
                                                                  (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Rep.fst"
                                                                    (Prims.of_int (38))
                                                                    (Prims.of_int (8))
                                                                    (Prims.of_int (41))
                                                                    (Prims.of_int (92))))))))
                                            | FStar_Tactics_Result.Failed
                                                (e, ps'2) ->
                                                FStar_Tactics_Result.Failed
                                                  (e, ps'2))))
                                     (FStar_Tactics_Types.decr_depth
                                        (FStar_Tactics_Types.set_proofstate_range
                                           ps'1
                                           (FStar_Range.prims_to_fstar_range
                                              (Prims.mk_range
                                                 "Data.Serialize.Rep.fst"
                                                 (Prims.of_int (36))
                                                 (Prims.of_int (9))
                                                 (Prims.of_int (41))
                                                 (Prims.of_int (92)))))))
                          | FStar_Tactics_Result.Failed (e, ps'1) ->
                              FStar_Tactics_Result.Failed (e, ps'1))
                   | uu____20062 ->
                       FStar_Tactics_Derived.fail
                         (Prims.strcat (FStar_String.concat "." name)
                            " is not a constructor")))
                   (FStar_Tactics_Types.decr_depth
                      (FStar_Tactics_Types.set_proofstate_range ps'
                         (FStar_Range.prims_to_fstar_range
                            (Prims.mk_range "Data.Serialize.Rep.fst"
                               (Prims.of_int (34)) (Prims.of_int (2))
                               (Prims.of_int (42)) (Prims.of_int (64)))))))
        | FStar_Tactics_Result.Failed (e, ps') ->
            FStar_Tactics_Result.Failed (e, ps')
let (makeGenericRep :
  FStar_Reflection_Types.fv ->
    (Data_Serialize_Types.inductiveSumup, unit)
      FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun name ->
    fun ps ->
      match match (FStar_Tactics_Builtins.top_env ())
                    (FStar_Tactics_Types.incr_depth
                       (FStar_Tactics_Types.set_proofstate_range
                          (FStar_Tactics_Types.incr_depth
                             (FStar_Tactics_Types.set_proofstate_range ps
                                (FStar_Range.prims_to_fstar_range
                                   (Prims.mk_range "Data.Serialize.Rep.fst"
                                      (Prims.of_int (45)) (Prims.of_int (10))
                                      (Prims.of_int (45)) (Prims.of_int (52))))))
                          (FStar_Range.prims_to_fstar_range
                             (Prims.mk_range "Data.Serialize.Rep.fst"
                                (Prims.of_int (45)) (Prims.of_int (22))
                                (Prims.of_int (45)) (Prims.of_int (34))))))
            with
            | FStar_Tactics_Result.Success (a, ps') ->
                (match () with
                 | () ->
                     (Data_Serialize_Helpers.lookup_typ' a
                        (FStar_Reflection_Basic.inspect_fv name))
                       (FStar_Tactics_Types.decr_depth
                          (FStar_Tactics_Types.set_proofstate_range ps'
                             (FStar_Range.prims_to_fstar_range
                                (Prims.mk_range "Data.Serialize.Rep.fst"
                                   (Prims.of_int (45)) (Prims.of_int (10))
                                   (Prims.of_int (45)) (Prims.of_int (52)))))))
            | FStar_Tactics_Result.Failed (e, ps') ->
                FStar_Tactics_Result.Failed (e, ps')
      with
      | FStar_Tactics_Result.Success (a, ps') ->
          (match () with
           | () ->
               ((match FStar_Reflection_Basic.inspect_sigelt a with
                 | FStar_Reflection_Data.Sg_Inductive
                     (name1, univs, params, typ', constructors) ->
                     (fun ps1 ->
                        match () with
                        | () ->
                            (match (FStar_Tactics_Util.map
                                      (makeGenericRep'Cons
                                         (FStar_List_Tot_Base.length params))
                                      constructors)
                                     (FStar_Tactics_Types.incr_depth
                                        (FStar_Tactics_Types.set_proofstate_range
                                           (FStar_Tactics_Types.decr_depth
                                              (FStar_Tactics_Types.set_proofstate_range
                                                 (FStar_Tactics_Types.incr_depth
                                                    (FStar_Tactics_Types.set_proofstate_range
                                                       ps1
                                                       (FStar_Range.prims_to_fstar_range
                                                          (Prims.mk_range
                                                             "Data.Serialize.Rep.fst"
                                                             (Prims.of_int (48))
                                                             (Prims.of_int (22))
                                                             (Prims.of_int (48))
                                                             (Prims.of_int (37))))))
                                                 (FStar_Range.prims_to_fstar_range
                                                    (Prims.mk_range
                                                       "Data.Serialize.Rep.fst"
                                                       (Prims.of_int (49))
                                                       (Prims.of_int (9))
                                                       (Prims.of_int (50))
                                                       (Prims.of_int (53))))))
                                           (FStar_Range.prims_to_fstar_range
                                              (Prims.mk_range
                                                 "Data.Serialize.Rep.fst"
                                                 (Prims.of_int (49))
                                                 (Prims.of_int (21))
                                                 (Prims.of_int (49))
                                                 (Prims.of_int (65))))))
                             with
                             | FStar_Tactics_Result.Success (a1, ps'1) ->
                                 (match () with
                                  | () ->
                                      FStar_Tactics_Result.Success
                                        ({
                                           Data_Serialize_Types.iName = name1;
                                           Data_Serialize_Types.iVars =
                                             (FStar_List_Tot_Base.length
                                                params);
                                           Data_Serialize_Types.iCons = a1
                                         },
                                          (FStar_Tactics_Types.decr_depth
                                             (FStar_Tactics_Types.set_proofstate_range
                                                ps'1
                                                (FStar_Range.prims_to_fstar_range
                                                   (Prims.mk_range
                                                      "Data.Serialize.Rep.fst"
                                                      (Prims.of_int (50))
                                                      (Prims.of_int (10))
                                                      (Prims.of_int (50))
                                                      (Prims.of_int (52))))))))
                             | FStar_Tactics_Result.Failed (e, ps'1) ->
                                 FStar_Tactics_Result.Failed (e, ps'1)))
                 | uu____20286 ->
                     FStar_Tactics_Derived.fail
                       (Prims.strcat
                          (FStar_Reflection_Derived.fv_to_string name)
                          " is not an inductive")))
                 (FStar_Tactics_Types.decr_depth
                    (FStar_Tactics_Types.set_proofstate_range ps'
                       (FStar_Range.prims_to_fstar_range
                          (Prims.mk_range "Data.Serialize.Rep.fst"
                             (Prims.of_int (46)) (Prims.of_int (2))
                             (Prims.of_int (51)) (Prims.of_int (58)))))))
      | FStar_Tactics_Result.Failed (e, ps') ->
          FStar_Tactics_Result.Failed (e, ps')