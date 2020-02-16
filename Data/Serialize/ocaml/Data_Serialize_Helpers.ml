open Prims
let mkerror : 'At . Prims.string -> 'At =
  fun uu____40797 -> failwith "Not yet implemented:mkerror"
type ('Aa, 'Ab) mkTupleType = ('Aa * 'Ab)
let (mkTupleTypeTac :
  FStar_Reflection_Types.typ ->
    FStar_Reflection_Types.typ ->
      (FStar_Reflection_Types.typ, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun a ->
    fun b ->
      fun ps ->
        match match (FStar_Tactics_Builtins.pack
                       (FStar_Reflection_Data.Tv_App
                          ((FStar_Reflection_Basic.pack_ln
                              (FStar_Reflection_Data.Tv_FVar
                                 (FStar_Reflection_Basic.pack_fv
                                    ["Data";
                                    "Serialize";
                                    "Helpers";
                                    "mkTupleType"]))),
                            (a, FStar_Reflection_Data.Q_Explicit))))
                      (FStar_Tactics_Types.incr_depth
                         (FStar_Tactics_Types.set_proofstate_range
                            (FStar_Tactics_Types.incr_depth
                               (FStar_Tactics_Types.set_proofstate_range ps
                                  (FStar_Range.prims_to_fstar_range
                                     (Prims.mk_range
                                        "Data.Serialize.Helpers.fst"
                                        (Prims.of_int (11))
                                        (Prims.of_int (9))
                                        (Prims.of_int (11))
                                        (Prims.of_int (80))))))
                            (FStar_Range.prims_to_fstar_range
                               (Prims.mk_range "Data.Serialize.Helpers.fst"
                                  (Prims.of_int (11)) (Prims.of_int (17))
                                  (Prims.of_int (11)) (Prims.of_int (63))))))
              with
              | FStar_Tactics_Result.Success (a1, ps') ->
                  (match () with
                   | () ->
                       FStar_Tactics_Result.Success
                         ((FStar_Reflection_Data.Tv_App
                             (a1, (b, FStar_Reflection_Data.Q_Explicit))),
                           (FStar_Tactics_Types.decr_depth
                              (FStar_Tactics_Types.set_proofstate_range ps'
                                 (FStar_Range.prims_to_fstar_range
                                    (Prims.mk_range
                                       "Data.Serialize.Helpers.fst"
                                       (Prims.of_int (11)) (Prims.of_int (9))
                                       (Prims.of_int (11))
                                       (Prims.of_int (80))))))))
              | FStar_Tactics_Result.Failed (e, ps') ->
                  FStar_Tactics_Result.Failed (e, ps')
        with
        | FStar_Tactics_Result.Success (a1, ps') ->
            (match () with
             | () ->
                 (FStar_Tactics_Builtins.pack a1)
                   (FStar_Tactics_Types.decr_depth
                      (FStar_Tactics_Types.set_proofstate_range ps'
                         (FStar_Range.prims_to_fstar_range
                            (Prims.mk_range "Data.Serialize.Helpers.fst"
                               (Prims.of_int (11)) (Prims.of_int (4))
                               (Prims.of_int (11)) (Prims.of_int (80)))))))
        | FStar_Tactics_Result.Failed (e, ps') ->
            FStar_Tactics_Result.Failed (e, ps')
let (makeOptionType :
  FStar_Reflection_Types.term ->
    (FStar_Reflection_Types.typ, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun t ->
    FStar_Tactics_Builtins.pack
      (FStar_Reflection_Data.Tv_App
         ((FStar_Reflection_Basic.pack_ln
             (FStar_Reflection_Data.Tv_FVar
                (FStar_Reflection_Basic.pack_fv
                   ["FStar"; "Pervasives"; "Native"; "option"]))),
           (t, FStar_Reflection_Data.Q_Explicit)))
let rec (makeTupleType :
  FStar_Reflection_Types.typ Prims.list ->
    (FStar_Reflection_Types.typ, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun types ->
    match types with
    | [] ->
        (fun s ->
           FStar_Tactics_Result.Success
             ((FStar_Reflection_Basic.pack_ln
                 (FStar_Reflection_Data.Tv_FVar
                    (FStar_Reflection_Basic.pack_fv ["Prims"; "unit"]))), s))
    | t::[] -> (fun s -> FStar_Tactics_Result.Success (t, s))
    | t::tl1 ->
        (fun ps ->
           match (makeTupleType tl1)
                   (FStar_Tactics_Types.incr_depth
                      (FStar_Tactics_Types.set_proofstate_range ps
                         (FStar_Range.prims_to_fstar_range
                            (Prims.mk_range "Data.Serialize.Helpers.fst"
                               (Prims.of_int (20)) (Prims.of_int (30))
                               (Prims.of_int (20)) (Prims.of_int (48))))))
           with
           | FStar_Tactics_Result.Success (a, ps') ->
               (match () with
                | () ->
                    (match match (FStar_Tactics_Builtins.pack
                                    (FStar_Reflection_Data.Tv_App
                                       ((FStar_Reflection_Basic.pack_ln
                                           (FStar_Reflection_Data.Tv_FVar
                                              (FStar_Reflection_Basic.pack_fv
                                                 ["Data";
                                                 "Serialize";
                                                 "Helpers";
                                                 "mkTupleType"]))),
                                         (t,
                                           FStar_Reflection_Data.Q_Explicit))))
                                   (FStar_Tactics_Types.incr_depth
                                      (FStar_Tactics_Types.set_proofstate_range
                                         (FStar_Tactics_Types.incr_depth
                                            (FStar_Tactics_Types.set_proofstate_range
                                               (FStar_Tactics_Types.decr_depth
                                                  (FStar_Tactics_Types.set_proofstate_range
                                                     ps'
                                                     (FStar_Range.prims_to_fstar_range
                                                        (Prims.mk_range
                                                           "Data.Serialize.Helpers.fst"
                                                           (Prims.of_int (11))
                                                           (Prims.of_int (4))
                                                           (Prims.of_int (11))
                                                           (Prims.of_int (80))))))
                                               (FStar_Range.prims_to_fstar_range
                                                  (Prims.mk_range
                                                     "Data.Serialize.Helpers.fst"
                                                     (Prims.of_int (11))
                                                     (Prims.of_int (9))
                                                     (Prims.of_int (11))
                                                     (Prims.of_int (80))))))
                                         (FStar_Range.prims_to_fstar_range
                                            (Prims.mk_range
                                               "Data.Serialize.Helpers.fst"
                                               (Prims.of_int (11))
                                               (Prims.of_int (17))
                                               (Prims.of_int (11))
                                               (Prims.of_int (63))))))
                           with
                           | FStar_Tactics_Result.Success (a1, ps'1) ->
                               (match () with
                                | () ->
                                    FStar_Tactics_Result.Success
                                      ((FStar_Reflection_Data.Tv_App
                                          (a1,
                                            (a,
                                              FStar_Reflection_Data.Q_Explicit))),
                                        (FStar_Tactics_Types.decr_depth
                                           (FStar_Tactics_Types.set_proofstate_range
                                              ps'1
                                              (FStar_Range.prims_to_fstar_range
                                                 (Prims.mk_range
                                                    "Data.Serialize.Helpers.fst"
                                                    (Prims.of_int (11))
                                                    (Prims.of_int (9))
                                                    (Prims.of_int (11))
                                                    (Prims.of_int (80))))))))
                           | FStar_Tactics_Result.Failed (e, ps'1) ->
                               FStar_Tactics_Result.Failed (e, ps'1)
                     with
                     | FStar_Tactics_Result.Success (a1, ps'1) ->
                         (match () with
                          | () ->
                              (FStar_Tactics_Builtins.pack a1)
                                (FStar_Tactics_Types.decr_depth
                                   (FStar_Tactics_Types.set_proofstate_range
                                      ps'1
                                      (FStar_Range.prims_to_fstar_range
                                         (Prims.mk_range
                                            "Data.Serialize.Helpers.fst"
                                            (Prims.of_int (11))
                                            (Prims.of_int (4))
                                            (Prims.of_int (11))
                                            (Prims.of_int (80)))))))
                     | FStar_Tactics_Result.Failed (e, ps'1) ->
                         FStar_Tactics_Result.Failed (e, ps'1)))
           | FStar_Tactics_Result.Failed (e, ps') ->
               FStar_Tactics_Result.Failed (e, ps'))
let (makeEitherType :
  FStar_Reflection_Types.typ Prims.list ->
    (FStar_Reflection_Types.typ, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun types ->
    fun ps ->
      match (FStar_Tactics_Util.map makeOptionType types)
              (FStar_Tactics_Types.incr_depth
                 (FStar_Tactics_Types.set_proofstate_range ps
                    (FStar_Range.prims_to_fstar_range
                       (Prims.mk_range "Data.Serialize.Helpers.fst"
                          (Prims.of_int (23)) (Prims.of_int (18))
                          (Prims.of_int (23)) (Prims.of_int (44))))))
      with
      | FStar_Tactics_Result.Success (a, ps') ->
          (match () with
           | () ->
               (makeTupleType a)
                 (FStar_Tactics_Types.decr_depth
                    (FStar_Tactics_Types.set_proofstate_range ps'
                       (FStar_Range.prims_to_fstar_range
                          (Prims.mk_range "Data.Serialize.Helpers.fst"
                             (Prims.of_int (23)) (Prims.of_int (4))
                             (Prims.of_int (23)) (Prims.of_int (44)))))))
      | FStar_Tactics_Result.Failed (e, ps') ->
          FStar_Tactics_Result.Failed (e, ps')
let (lookup_typ' :
  FStar_Reflection_Types.env ->
    FStar_Reflection_Types.name ->
      (FStar_Reflection_Types.sigelt, unit)
        FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun env ->
    fun name ->
      match FStar_Reflection_Basic.lookup_typ env name with
      | FStar_Pervasives_Native.Some x ->
          (fun s -> FStar_Tactics_Result.Success (x, s))
      | FStar_Pervasives_Native.None ->
          FStar_Tactics_Derived.fail
            (Prims.strcat "No \""
               (Prims.strcat (FStar_String.concat "." name)
                  "\" found in the given enviroment"))
let rec (mk_abs :
  FStar_Reflection_Types.binder Prims.list ->
    FStar_Reflection_Types.term ->
      (FStar_Reflection_Types.term, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun bs ->
    fun body ->
      match bs with
      | [] -> (fun s -> FStar_Tactics_Result.Success (body, s))
      | b::bs1 ->
          (fun ps ->
             match match (mk_abs bs1 body)
                           (FStar_Tactics_Types.incr_depth
                              (FStar_Tactics_Types.set_proofstate_range
                                 (FStar_Tactics_Types.incr_depth
                                    (FStar_Tactics_Types.set_proofstate_range
                                       ps
                                       (FStar_Range.prims_to_fstar_range
                                          (Prims.mk_range
                                             "Data.Serialize.Helpers.fst"
                                             (Prims.of_int (33))
                                             (Prims.of_int (20))
                                             (Prims.of_int (33))
                                             (Prims.of_int (47))))))
                                 (FStar_Range.prims_to_fstar_range
                                    (Prims.mk_range
                                       "Data.Serialize.Helpers.fst"
                                       (Prims.of_int (33))
                                       (Prims.of_int (30))
                                       (Prims.of_int (33))
                                       (Prims.of_int (46))))))
                   with
                   | FStar_Tactics_Result.Success (a, ps') ->
                       (match () with
                        | () ->
                            FStar_Tactics_Result.Success
                              ((FStar_Reflection_Data.Tv_Abs (b, a)),
                                (FStar_Tactics_Types.decr_depth
                                   (FStar_Tactics_Types.set_proofstate_range
                                      ps'
                                      (FStar_Range.prims_to_fstar_range
                                         (Prims.mk_range
                                            "Data.Serialize.Helpers.fst"
                                            (Prims.of_int (33))
                                            (Prims.of_int (20))
                                            (Prims.of_int (33))
                                            (Prims.of_int (47))))))))
                   | FStar_Tactics_Result.Failed (e, ps') ->
                       FStar_Tactics_Result.Failed (e, ps')
             with
             | FStar_Tactics_Result.Success (a, ps') ->
                 (match () with
                  | () ->
                      (FStar_Tactics_Builtins.pack a)
                        (FStar_Tactics_Types.decr_depth
                           (FStar_Tactics_Types.set_proofstate_range ps'
                              (FStar_Range.prims_to_fstar_range
                                 (Prims.mk_range "Data.Serialize.Helpers.fst"
                                    (Prims.of_int (33)) (Prims.of_int (15))
                                    (Prims.of_int (33)) (Prims.of_int (47)))))))
             | FStar_Tactics_Result.Failed (e, ps') ->
                 FStar_Tactics_Result.Failed (e, ps'))
let rec last :
  'a . 'a Prims.list -> ('a, unit) FStar_Tactics_Effect._dm4f_TAC_repr =
  fun l ->
    match l with
    | [] -> FStar_Tactics_Derived.fail "last: empty list"
    | x::[] -> (fun s -> FStar_Tactics_Result.Success (x, s))
    | uu____41388::xs -> last xs
let (argvToBinder :
  FStar_Reflection_Data.argv ->
    (FStar_Reflection_Types.binder, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun x ->
    fun ps ->
      match () with
      | () ->
          ((match x with
            | (v1, q) ->
                (fun ps1 ->
                   match match (FStar_Tactics_Builtins.inspect v1)
                                 (FStar_Tactics_Types.incr_depth
                                    (FStar_Tactics_Types.set_proofstate_range
                                       (FStar_Tactics_Types.incr_depth
                                          (FStar_Tactics_Types.set_proofstate_range
                                             ps1
                                             (FStar_Range.prims_to_fstar_range
                                                (Prims.mk_range
                                                   "Data.Serialize.Helpers.fst"
                                                   (Prims.of_int (44))
                                                   (Prims.of_int (2))
                                                   (Prims.of_int (48))
                                                   (Prims.of_int (3))))))
                                       (FStar_Range.prims_to_fstar_range
                                          (Prims.mk_range
                                             "Data.Serialize.Helpers.fst"
                                             (Prims.of_int (42))
                                             (Prims.of_int (6))
                                             (Prims.of_int (42))
                                             (Prims.of_int (7))))))
                         with
                         | FStar_Tactics_Result.Success (a, ps') ->
                             (match () with
                              | () ->
                                  ((match a with
                                    | FStar_Reflection_Data.Tv_BVar b ->
                                        (fun s ->
                                           FStar_Tactics_Result.Success
                                             (b, s))
                                    | FStar_Reflection_Data.Tv_Var b ->
                                        (fun s ->
                                           FStar_Tactics_Result.Success
                                             (b, s))
                                    | x1 ->
                                        (fun ps2 ->
                                           match match match match () with
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
                                                                    ps2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (22))
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (87))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (43))
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (86))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (43))
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (69))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (59))
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (68))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (43))
                                                                    (Prims.of_int (47))
                                                                    (Prims.of_int (69)))))))
                                                       with
                                                       | FStar_Tactics_Result.Success
                                                           (a1, ps'1) ->
                                                           (match () with
                                                            | () ->
                                                                FStar_Tactics_Result.Success
                                                                  ((Prims.strcat
                                                                    a1
                                                                    "\": not a BV"),
                                                                    (
                                                                    FStar_Tactics_Types.decr_depth
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
                                                                "argvToBinder \""
                                                                a1),
                                                              (FStar_Tactics_Types.decr_depth
                                                                 (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (
                                                                    FStar_Range.prims_to_fstar_range
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
                                                                  "Data.Serialize.Helpers.fst"
                                                                  (Prims.of_int (47))
                                                                  (Prims.of_int (17))
                                                                  (Prims.of_int (47))
                                                                  (Prims.of_int (87)))))))
                                           | FStar_Tactics_Result.Failed
                                               (e, ps'1) ->
                                               FStar_Tactics_Result.Failed
                                                 (e, ps'1))))
                                    (FStar_Tactics_Types.decr_depth
                                       (FStar_Tactics_Types.set_proofstate_range
                                          ps'
                                          (FStar_Range.prims_to_fstar_range
                                             (Prims.mk_range
                                                "Data.Serialize.Helpers.fst"
                                                (Prims.of_int (44))
                                                (Prims.of_int (2))
                                                (Prims.of_int (48))
                                                (Prims.of_int (3)))))))
                         | FStar_Tactics_Result.Failed (e, ps') ->
                             FStar_Tactics_Result.Failed (e, ps')
                   with
                   | FStar_Tactics_Result.Success (a, ps') ->
                       (match () with
                        | () ->
                            FStar_Tactics_Result.Success
                              ((FStar_Reflection_Basic.pack_binder a q),
                                (FStar_Tactics_Types.decr_depth
                                   (FStar_Tactics_Types.set_proofstate_range
                                      ps'
                                      (FStar_Range.prims_to_fstar_range
                                         (Prims.mk_range
                                            "Data.Serialize.Helpers.fst"
                                            (Prims.of_int (43))
                                            (Prims.of_int (2))
                                            (Prims.of_int (48))
                                            (Prims.of_int (5))))))))
                   | FStar_Tactics_Result.Failed (e, ps') ->
                       FStar_Tactics_Result.Failed (e, ps'))))
            (FStar_Tactics_Types.decr_depth
               (FStar_Tactics_Types.set_proofstate_range
                  (FStar_Tactics_Types.incr_depth
                     (FStar_Tactics_Types.set_proofstate_range ps
                        (FStar_Range.prims_to_fstar_range
                           (Prims.mk_range "Data.Serialize.Helpers.fst"
                              (Prims.of_int (42)) (Prims.of_int (13))
                              (Prims.of_int (42)) (Prims.of_int (14))))))
                  (FStar_Range.prims_to_fstar_range
                     (Prims.mk_range "Data.Serialize.Helpers.fst"
                        (Prims.of_int (42)) (Prims.of_int (2))
                        (Prims.of_int (48)) (Prims.of_int (5))))))
let (binderToArgv :
  FStar_Reflection_Types.binder ->
    ((FStar_Reflection_Types.term * FStar_Reflection_Data.aqualv), unit)
      FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun b ->
    fun ps ->
      match () with
      | () ->
          ((match FStar_Reflection_Basic.inspect_binder b with
            | (bv, q) ->
                (fun ps1 ->
                   match (FStar_Tactics_Derived.bv_to_term bv)
                           (FStar_Tactics_Types.incr_depth
                              (FStar_Tactics_Types.set_proofstate_range ps1
                                 (FStar_Range.prims_to_fstar_range
                                    (Prims.mk_range
                                       "Data.Serialize.Helpers.fst"
                                       (Prims.of_int (51)) (Prims.of_int (2))
                                       (Prims.of_int (51))
                                       (Prims.of_int (15))))))
                   with
                   | FStar_Tactics_Result.Success (a, ps') ->
                       (match () with
                        | () ->
                            FStar_Tactics_Result.Success
                              ((a, q),
                                (FStar_Tactics_Types.decr_depth
                                   (FStar_Tactics_Types.set_proofstate_range
                                      ps'
                                      (FStar_Range.prims_to_fstar_range
                                         (Prims.mk_range
                                            "Data.Serialize.Helpers.fst"
                                            (Prims.of_int (51))
                                            (Prims.of_int (2))
                                            (Prims.of_int (51))
                                            (Prims.of_int (18))))))))
                   | FStar_Tactics_Result.Failed (e, ps') ->
                       FStar_Tactics_Result.Failed (e, ps'))))
            (FStar_Tactics_Types.decr_depth
               (FStar_Tactics_Types.set_proofstate_range
                  (FStar_Tactics_Types.incr_depth
                     (FStar_Tactics_Types.set_proofstate_range ps
                        (FStar_Range.prims_to_fstar_range
                           (Prims.mk_range "Data.Serialize.Helpers.fst"
                              (Prims.of_int (50)) (Prims.of_int (14))
                              (Prims.of_int (50)) (Prims.of_int (30))))))
                  (FStar_Range.prims_to_fstar_range
                     (Prims.mk_range "Data.Serialize.Helpers.fst"
                        (Prims.of_int (50)) (Prims.of_int (2))
                        (Prims.of_int (51)) (Prims.of_int (18))))))
let (norm_term' :
  FStar_Reflection_Types.term ->
    (FStar_Reflection_Types.term, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  = fun t -> FStar_Tactics_Derived.norm_term [FStar_Pervasives.zeta] t
let (fvOf :
  FStar_Reflection_Types.term ->
    (FStar_Reflection_Types.fv, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun t ->
    fun ps ->
      match (FStar_Tactics_Builtins.inspect t)
              (FStar_Tactics_Types.incr_depth
                 (FStar_Tactics_Types.set_proofstate_range ps
                    (FStar_Range.prims_to_fstar_range
                       (Prims.mk_range "Data.Serialize.Helpers.fst"
                          (Prims.of_int (56)) (Prims.of_int (27))
                          (Prims.of_int (56)) (Prims.of_int (36))))))
      with
      | FStar_Tactics_Result.Success (a, ps') ->
          (match () with
           | () ->
               ((match a with
                 | FStar_Reflection_Data.Tv_FVar fv ->
                     (fun s -> FStar_Tactics_Result.Success (fv, s))
                 | uu____42072 -> FStar_Tactics_Derived.fail "not a fv"))
                 (FStar_Tactics_Types.decr_depth
                    (FStar_Tactics_Types.set_proofstate_range ps'
                       (FStar_Range.prims_to_fstar_range
                          (Prims.mk_range "Data.Serialize.Helpers.fst"
                             (Prims.of_int (56)) (Prims.of_int (21))
                             (Prims.of_int (58)) (Prims.of_int (24)))))))
      | FStar_Tactics_Result.Failed (e, ps') ->
          FStar_Tactics_Result.Failed (e, ps')
let (nameCurMod' :
  FStar_Reflection_Types.name ->
    (Prims.string -> Prims.string) ->
      (Prims.string Prims.list, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun n1 ->
    fun f ->
      fun ps ->
        match (FStar_Tactics_Derived.cur_module ())
                (FStar_Tactics_Types.incr_depth
                   (FStar_Tactics_Types.set_proofstate_range ps
                      (FStar_Range.prims_to_fstar_range
                         (Prims.mk_range "Data.Serialize.Helpers.fst"
                            (Prims.of_int (61)) (Prims.of_int (2))
                            (Prims.of_int (61)) (Prims.of_int (15))))))
        with
        | FStar_Tactics_Result.Success (a, ps') ->
            (match () with
             | () ->
                 (match match match (last n1)
                                      (FStar_Tactics_Types.incr_depth
                                         (FStar_Tactics_Types.set_proofstate_range
                                            (FStar_Tactics_Types.incr_depth
                                               (FStar_Tactics_Types.set_proofstate_range
                                                  (FStar_Tactics_Types.incr_depth
                                                     (FStar_Tactics_Types.set_proofstate_range
                                                        (FStar_Tactics_Types.decr_depth
                                                           (FStar_Tactics_Types.set_proofstate_range
                                                              ps'
                                                              (FStar_Range.prims_to_fstar_range
                                                                 (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (2))
                                                                    (Prims.of_int (61))
                                                                    (Prims.of_int (30))))))
                                                        (FStar_Range.prims_to_fstar_range
                                                           (Prims.mk_range
                                                              "Data.Serialize.Helpers.fst"
                                                              (Prims.of_int (61))
                                                              (Prims.of_int (18))
                                                              (Prims.of_int (61))
                                                              (Prims.of_int (30))))))
                                                  (FStar_Range.prims_to_fstar_range
                                                     (Prims.mk_range
                                                        "Data.Serialize.Helpers.fst"
                                                        (Prims.of_int (61))
                                                        (Prims.of_int (19))
                                                        (Prims.of_int (61))
                                                        (Prims.of_int (29))))))
                                            (FStar_Range.prims_to_fstar_range
                                               (Prims.mk_range
                                                  "Data.Serialize.Helpers.fst"
                                                  (Prims.of_int (61))
                                                  (Prims.of_int (21))
                                                  (Prims.of_int (61))
                                                  (Prims.of_int (29))))))
                              with
                              | FStar_Tactics_Result.Success (a1, ps'1) ->
                                  (match () with
                                   | () ->
                                       FStar_Tactics_Result.Success
                                         ((f a1),
                                           (FStar_Tactics_Types.decr_depth
                                              (FStar_Tactics_Types.set_proofstate_range
                                                 ps'1
                                                 (FStar_Range.prims_to_fstar_range
                                                    (Prims.mk_range
                                                       "Data.Serialize.Helpers.fst"
                                                       (Prims.of_int (61))
                                                       (Prims.of_int (19))
                                                       (Prims.of_int (61))
                                                       (Prims.of_int (29))))))))
                              | FStar_Tactics_Result.Failed (e, ps'1) ->
                                  FStar_Tactics_Result.Failed (e, ps'1)
                        with
                        | FStar_Tactics_Result.Success (a1, ps'1) ->
                            (match () with
                             | () ->
                                 FStar_Tactics_Result.Success
                                   ([a1],
                                     (FStar_Tactics_Types.decr_depth
                                        (FStar_Tactics_Types.set_proofstate_range
                                           ps'1
                                           (FStar_Range.prims_to_fstar_range
                                              (Prims.mk_range
                                                 "Data.Serialize.Helpers.fst"
                                                 (Prims.of_int (61))
                                                 (Prims.of_int (18))
                                                 (Prims.of_int (61))
                                                 (Prims.of_int (30))))))))
                        | FStar_Tactics_Result.Failed (e, ps'1) ->
                            FStar_Tactics_Result.Failed (e, ps'1)
                  with
                  | FStar_Tactics_Result.Success (a1, ps'1) ->
                      (match () with
                       | () ->
                           FStar_Tactics_Result.Success
                             ((FStar_List_Tot_Base.append a a1),
                               (FStar_Tactics_Types.decr_depth
                                  (FStar_Tactics_Types.set_proofstate_range
                                     ps'1
                                     (FStar_Range.prims_to_fstar_range
                                        (Prims.mk_range
                                           "Data.Serialize.Helpers.fst"
                                           (Prims.of_int (61))
                                           (Prims.of_int (2))
                                           (Prims.of_int (61))
                                           (Prims.of_int (30))))))))
                  | FStar_Tactics_Result.Failed (e, ps'1) ->
                      FStar_Tactics_Result.Failed (e, ps'1)))
        | FStar_Tactics_Result.Failed (e, ps') ->
            FStar_Tactics_Result.Failed (e, ps')
let rec (findIndex' :
  FStar_Reflection_Types.bv ->
    Prims.nat ->
      FStar_Reflection_Types.bv Prims.list ->
        (Prims.nat, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun x ->
    fun n1 ->
      fun l ->
        match l with
        | [] -> FStar_Tactics_Derived.fail "findIndex failed"
        | hd1::tl1 ->
            if (FStar_Reflection_Basic.compare_bv x hd1) = FStar_Order.Eq
            then (fun s -> FStar_Tactics_Result.Success (n1, s))
            else findIndex' x (n1 + Prims.int_one) tl1
let (findIndex :
  FStar_Reflection_Types.bv ->
    FStar_Reflection_Types.bv Prims.list ->
      (Prims.nat, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  = fun x -> fun l -> findIndex' x Prims.int_zero l
let (name_to_term :
  FStar_Reflection_Types.name ->
    (FStar_Reflection_Types.term, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun n1 ->
    FStar_Tactics_Builtins.pack
      (FStar_Reflection_Data.Tv_FVar (FStar_Reflection_Basic.pack_fv n1))
let (bvName : Prims.string -> FStar_Reflection_Types.bv) =
  fun n1 ->
    FStar_Reflection_Basic.pack_bv
      {
        FStar_Reflection_Data.bv_ppname = (Prims.strcat "v" n1);
        FStar_Reflection_Data.bv_index = Prims.int_zero;
        FStar_Reflection_Data.bv_sort =
          (FStar_Reflection_Basic.pack_ln FStar_Reflection_Data.Tv_Unknown)
      }
let (binderName : Prims.string -> FStar_Reflection_Types.binder) =
  fun n1 ->
    FStar_Reflection_Basic.pack_binder (bvName n1)
      FStar_Reflection_Data.Q_Explicit
let (bvNth : Prims.int -> FStar_Reflection_Types.bv) =
  fun n1 ->
    FStar_Reflection_Basic.pack_bv
      {
        FStar_Reflection_Data.bv_ppname =
          (Prims.strcat "typvar" (Prims.string_of_int n1));
        FStar_Reflection_Data.bv_index = Prims.int_zero;
        FStar_Reflection_Data.bv_sort =
          (FStar_Reflection_Basic.pack_ln FStar_Reflection_Data.Tv_Unknown)
      }
let (binderNth : Prims.int -> FStar_Reflection_Types.binder) =
  fun n1 ->
    FStar_Reflection_Basic.pack_binder (bvNth n1)
      FStar_Reflection_Data.Q_Explicit
let rec (mkList : Prims.int -> Prims.int -> Prims.int Prims.list) =
  fun min1 ->
    fun max1 ->
      if min1 > max1
      then []
      else min1 :: (mkList (min1 + Prims.int_one) max1)
let (call1 :
  FStar_Reflection_Types.term ->
    FStar_Reflection_Types.term ->
      (FStar_Reflection_Types.term, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun f ->
    fun arg ->
      FStar_Tactics_Builtins.pack
        (FStar_Reflection_Data.Tv_App
           (f, (arg, FStar_Reflection_Data.Q_Explicit)))
let (call2 :
  FStar_Reflection_Types.term ->
    FStar_Reflection_Types.term ->
      FStar_Reflection_Types.term ->
        (FStar_Reflection_Types.term, unit)
          FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun f ->
    fun arg1 ->
      fun arg2 ->
        fun ps ->
          match (FStar_Tactics_Builtins.pack
                   (FStar_Reflection_Data.Tv_App
                      (f, (arg1, FStar_Reflection_Data.Q_Explicit))))
                  (FStar_Tactics_Types.incr_depth
                     (FStar_Tactics_Types.set_proofstate_range ps
                        (FStar_Range.prims_to_fstar_range
                           (Prims.mk_range "Data.Serialize.Helpers.fst"
                              (Prims.of_int (91)) (Prims.of_int (10))
                              (Prims.of_int (91)) (Prims.of_int (24))))))
          with
          | FStar_Tactics_Result.Success (a, ps') ->
              (match () with
               | () ->
                   (FStar_Tactics_Builtins.pack
                      (FStar_Reflection_Data.Tv_App
                         (a, (arg2, FStar_Reflection_Data.Q_Explicit))))
                     (FStar_Tactics_Types.decr_depth
                        (FStar_Tactics_Types.set_proofstate_range ps'
                           (FStar_Range.prims_to_fstar_range
                              (Prims.mk_range "Data.Serialize.Helpers.fst"
                                 (Prims.of_int (89)) (Prims.of_int (4))
                                 (Prims.of_int (89)) (Prims.of_int (37)))))))
          | FStar_Tactics_Result.Failed (e, ps') ->
              FStar_Tactics_Result.Failed (e, ps')
let (call3 :
  FStar_Reflection_Types.term ->
    FStar_Reflection_Types.term ->
      FStar_Reflection_Types.term ->
        FStar_Reflection_Types.term ->
          (FStar_Reflection_Types.term, unit)
            FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun f ->
    fun arg1 ->
      fun arg2 ->
        fun arg3 ->
          fun ps ->
            match match (FStar_Tactics_Builtins.pack
                           (FStar_Reflection_Data.Tv_App
                              (f, (arg1, FStar_Reflection_Data.Q_Explicit))))
                          (FStar_Tactics_Types.incr_depth
                             (FStar_Tactics_Types.set_proofstate_range
                                (FStar_Tactics_Types.incr_depth
                                   (FStar_Tactics_Types.set_proofstate_range
                                      ps
                                      (FStar_Range.prims_to_fstar_range
                                         (Prims.mk_range
                                            "Data.Serialize.Helpers.fst"
                                            (Prims.of_int (93))
                                            (Prims.of_int (10))
                                            (Prims.of_int (93))
                                            (Prims.of_int (29))))))
                                (FStar_Range.prims_to_fstar_range
                                   (Prims.mk_range
                                      "Data.Serialize.Helpers.fst"
                                      (Prims.of_int (91)) (Prims.of_int (10))
                                      (Prims.of_int (91)) (Prims.of_int (24))))))
                  with
                  | FStar_Tactics_Result.Success (a, ps') ->
                      (match () with
                       | () ->
                           (FStar_Tactics_Builtins.pack
                              (FStar_Reflection_Data.Tv_App
                                 (a,
                                   (arg2, FStar_Reflection_Data.Q_Explicit))))
                             (FStar_Tactics_Types.decr_depth
                                (FStar_Tactics_Types.set_proofstate_range ps'
                                   (FStar_Range.prims_to_fstar_range
                                      (Prims.mk_range
                                         "Data.Serialize.Helpers.fst"
                                         (Prims.of_int (89))
                                         (Prims.of_int (4))
                                         (Prims.of_int (89))
                                         (Prims.of_int (37)))))))
                  | FStar_Tactics_Result.Failed (e, ps') ->
                      FStar_Tactics_Result.Failed (e, ps')
            with
            | FStar_Tactics_Result.Success (a, ps') ->
                (match () with
                 | () ->
                     (FStar_Tactics_Builtins.pack
                        (FStar_Reflection_Data.Tv_App
                           (a, (arg3, FStar_Reflection_Data.Q_Explicit))))
                       (FStar_Tactics_Types.decr_depth
                          (FStar_Tactics_Types.set_proofstate_range ps'
                             (FStar_Range.prims_to_fstar_range
                                (Prims.mk_range "Data.Serialize.Helpers.fst"
                                   (Prims.of_int (89)) (Prims.of_int (4))
                                   (Prims.of_int (89)) (Prims.of_int (37)))))))
            | FStar_Tactics_Result.Failed (e, ps') ->
                FStar_Tactics_Result.Failed (e, ps')
let (lex :
  FStar_Reflection_Types.term ->
    FStar_Reflection_Types.term ->
      (FStar_Reflection_Types.term, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun l ->
    fun r ->
      fun ps ->
        match () with
        | () ->
            (match match (FStar_Tactics_Builtins.pack
                            (FStar_Reflection_Data.Tv_App
                               ((FStar_Reflection_Basic.pack_ln
                                   (FStar_Reflection_Data.Tv_FVar
                                      (FStar_Reflection_Basic.pack_fv
                                         ["Prims"; "LexCons"]))),
                                 (l, FStar_Reflection_Data.Q_Explicit))))
                           (FStar_Tactics_Types.incr_depth
                              (FStar_Tactics_Types.set_proofstate_range
                                 (FStar_Tactics_Types.incr_depth
                                    (FStar_Tactics_Types.set_proofstate_range
                                       (FStar_Tactics_Types.decr_depth
                                          (FStar_Tactics_Types.set_proofstate_range
                                             (FStar_Tactics_Types.incr_depth
                                                (FStar_Tactics_Types.set_proofstate_range
                                                   ps
                                                   (FStar_Range.prims_to_fstar_range
                                                      (Prims.mk_range
                                                         "Data.Serialize.Helpers.fst"
                                                         (Prims.of_int (99))
                                                         (Prims.of_int (20))
                                                         (Prims.of_int (99))
                                                         (Prims.of_int (48))))))
                                             (FStar_Range.prims_to_fstar_range
                                                (Prims.mk_range
                                                   "Data.Serialize.Helpers.fst"
                                                   (Prims.of_int (100))
                                                   (Prims.of_int (2))
                                                   (Prims.of_int (100))
                                                   (Prims.of_int (27))))))
                                       (FStar_Range.prims_to_fstar_range
                                          (Prims.mk_range
                                             "Data.Serialize.Helpers.fst"
                                             (Prims.of_int (100))
                                             (Prims.of_int (16))
                                             (Prims.of_int (100))
                                             (Prims.of_int (21))))))
                                 (FStar_Range.prims_to_fstar_range
                                    (Prims.mk_range
                                       "Data.Serialize.Helpers.fst"
                                       (Prims.of_int (91))
                                       (Prims.of_int (10))
                                       (Prims.of_int (91))
                                       (Prims.of_int (24))))))
                   with
                   | FStar_Tactics_Result.Success (a, ps') ->
                       (match () with
                        | () ->
                            (FStar_Tactics_Builtins.pack
                               (FStar_Reflection_Data.Tv_App
                                  (a,
                                    ((FStar_Reflection_Basic.pack_ln
                                        (FStar_Reflection_Data.Tv_FVar
                                           (FStar_Reflection_Basic.pack_fv
                                              ["Prims"; "LexTop"]))),
                                      FStar_Reflection_Data.Q_Explicit))))
                              (FStar_Tactics_Types.decr_depth
                                 (FStar_Tactics_Types.set_proofstate_range
                                    ps'
                                    (FStar_Range.prims_to_fstar_range
                                       (Prims.mk_range
                                          "Data.Serialize.Helpers.fst"
                                          (Prims.of_int (89))
                                          (Prims.of_int (4))
                                          (Prims.of_int (89))
                                          (Prims.of_int (37)))))))
                   | FStar_Tactics_Result.Failed (e, ps') ->
                       FStar_Tactics_Result.Failed (e, ps')
             with
             | FStar_Tactics_Result.Success (a, ps') ->
                 (match () with
                  | () ->
                      (match match (FStar_Tactics_Builtins.pack
                                      (FStar_Reflection_Data.Tv_App
                                         ((FStar_Reflection_Basic.pack_ln
                                             (FStar_Reflection_Data.Tv_FVar
                                                (FStar_Reflection_Basic.pack_fv
                                                   ["Prims"; "LexCons"]))),
                                           (r,
                                             FStar_Reflection_Data.Q_Explicit))))
                                     (FStar_Tactics_Types.incr_depth
                                        (FStar_Tactics_Types.set_proofstate_range
                                           (FStar_Tactics_Types.incr_depth
                                              (FStar_Tactics_Types.set_proofstate_range
                                                 (FStar_Tactics_Types.decr_depth
                                                    (FStar_Tactics_Types.set_proofstate_range
                                                       ps'
                                                       (FStar_Range.prims_to_fstar_range
                                                          (Prims.mk_range
                                                             "Data.Serialize.Helpers.fst"
                                                             (Prims.of_int (100))
                                                             (Prims.of_int (2))
                                                             (Prims.of_int (100))
                                                             (Prims.of_int (27))))))
                                                 (FStar_Range.prims_to_fstar_range
                                                    (Prims.mk_range
                                                       "Data.Serialize.Helpers.fst"
                                                       (Prims.of_int (100))
                                                       (Prims.of_int (22))
                                                       (Prims.of_int (100))
                                                       (Prims.of_int (27))))))
                                           (FStar_Range.prims_to_fstar_range
                                              (Prims.mk_range
                                                 "Data.Serialize.Helpers.fst"
                                                 (Prims.of_int (91))
                                                 (Prims.of_int (10))
                                                 (Prims.of_int (91))
                                                 (Prims.of_int (24))))))
                             with
                             | FStar_Tactics_Result.Success (a1, ps'1) ->
                                 (match () with
                                  | () ->
                                      (FStar_Tactics_Builtins.pack
                                         (FStar_Reflection_Data.Tv_App
                                            (a1,
                                              ((FStar_Reflection_Basic.pack_ln
                                                  (FStar_Reflection_Data.Tv_FVar
                                                     (FStar_Reflection_Basic.pack_fv
                                                        ["Prims"; "LexTop"]))),
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
                             | FStar_Tactics_Result.Failed (e, ps'1) ->
                                 FStar_Tactics_Result.Failed (e, ps'1)
                       with
                       | FStar_Tactics_Result.Success (a1, ps'1) ->
                           (match () with
                            | () ->
                                (match (FStar_Tactics_Builtins.pack
                                          (FStar_Reflection_Data.Tv_App
                                             ((FStar_Reflection_Basic.pack_ln
                                                 (FStar_Reflection_Data.Tv_FVar
                                                    (FStar_Reflection_Basic.pack_fv
                                                       ["Prims"; "precedes"]))),
                                               (a,
                                                 FStar_Reflection_Data.Q_Explicit))))
                                         (FStar_Tactics_Types.incr_depth
                                            (FStar_Tactics_Types.set_proofstate_range
                                               (FStar_Tactics_Types.decr_depth
                                                  (FStar_Tactics_Types.set_proofstate_range
                                                     ps'1
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
                                 | FStar_Tactics_Result.Success (a2, ps'2) ->
                                     (match () with
                                      | () ->
                                          (FStar_Tactics_Builtins.pack
                                             (FStar_Reflection_Data.Tv_App
                                                (a2,
                                                  (a1,
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
                                 | FStar_Tactics_Result.Failed (e, ps'2) ->
                                     FStar_Tactics_Result.Failed (e, ps'2)))
                       | FStar_Tactics_Result.Failed (e, ps'1) ->
                           FStar_Tactics_Result.Failed (e, ps'1)))
             | FStar_Tactics_Result.Failed (e, ps') ->
                 FStar_Tactics_Result.Failed (e, ps'))
let (add_admit :
  FStar_Reflection_Types.term ->
    (FStar_Reflection_Types.term, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun body ->
    fun ps ->
      match (FStar_Tactics_Derived.fresh_bv
               (FStar_Reflection_Basic.pack_ln
                  FStar_Reflection_Data.Tv_Unknown))
              (FStar_Tactics_Types.incr_depth
                 (FStar_Tactics_Types.set_proofstate_range ps
                    (FStar_Range.prims_to_fstar_range
                       (Prims.mk_range "Data.Serialize.Helpers.fst"
                          (Prims.of_int (104)) (Prims.of_int (16))
                          (Prims.of_int (104)) (Prims.of_int (29))))))
      with
      | FStar_Tactics_Result.Success (a, ps') ->
          (match () with
           | () ->
               (FStar_Tactics_Builtins.pack
                  (FStar_Reflection_Data.Tv_Let
                     (false, [], a,
                       (FStar_Reflection_Basic.pack_ln
                          (FStar_Reflection_Data.Tv_App
                             ((FStar_Reflection_Basic.pack_ln
                                 (FStar_Reflection_Data.Tv_FVar
                                    (FStar_Reflection_Basic.pack_fv
                                       ["Prims"; "admitP"]))),
                               ((FStar_Reflection_Basic.pack_ln
                                   (FStar_Reflection_Data.Tv_App
                                      ((FStar_Reflection_Basic.pack_ln
                                          (FStar_Reflection_Data.Tv_FVar
                                             (FStar_Reflection_Basic.pack_fv
                                                ["Prims"; "l_Forall"]))),
                                        ((FStar_Reflection_Basic.pack_ln
                                            (FStar_Reflection_Data.Tv_Abs
                                               ((FStar_Reflection_Basic.pack_binder
                                                   (FStar_Reflection_Basic.pack_bv
                                                      {
                                                        FStar_Reflection_Data.bv_ppname
                                                          = "t";
                                                        FStar_Reflection_Data.bv_index
                                                          =
                                                          (Prims.of_int (87));
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
                                                                 ["Prims";
                                                                 "l_Forall"]))),
                                                         ((FStar_Reflection_Basic.pack_ln
                                                             (FStar_Reflection_Data.Tv_Abs
                                                                ((FStar_Reflection_Basic.pack_binder
                                                                    (
                                                                    FStar_Reflection_Basic.pack_bv
                                                                    {
                                                                    FStar_Reflection_Data.bv_ppname
                                                                    = "x";
                                                                    FStar_Reflection_Data.bv_index
                                                                    =
                                                                    (Prims.of_int (88));
                                                                    FStar_Reflection_Data.bv_sort
                                                                    =
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_BVar
                                                                    (FStar_Reflection_Basic.pack_bv
                                                                    {
                                                                    FStar_Reflection_Data.bv_ppname
                                                                    = "t";
                                                                    FStar_Reflection_Data.bv_index
                                                                    =
                                                                    Prims.int_zero;
                                                                    FStar_Reflection_Data.bv_sort
                                                                    =
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    FStar_Reflection_Data.Tv_Unknown)
                                                                    })))
                                                                    })
                                                                    FStar_Reflection_Data.Q_Explicit),
                                                                  (FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Prims";
                                                                    "precedes"]))),
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Prims";
                                                                    "LexCons"]))),
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_BVar
                                                                    (FStar_Reflection_Basic.pack_bv
                                                                    {
                                                                    FStar_Reflection_Data.bv_ppname
                                                                    = "x";
                                                                    FStar_Reflection_Data.bv_index
                                                                    =
                                                                    Prims.int_zero;
                                                                    FStar_Reflection_Data.bv_sort
                                                                    =
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    FStar_Reflection_Data.Tv_Unknown)
                                                                    }))),
                                                                    FStar_Reflection_Data.Q_Explicit)))),
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Prims";
                                                                    "LexTop"]))),
                                                                    FStar_Reflection_Data.Q_Explicit)))),
                                                                    FStar_Reflection_Data.Q_Explicit)))),
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_App
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Prims";
                                                                    "LexCons"]))),
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_BVar
                                                                    (FStar_Reflection_Basic.pack_bv
                                                                    {
                                                                    FStar_Reflection_Data.bv_ppname
                                                                    = "x";
                                                                    FStar_Reflection_Data.bv_index
                                                                    =
                                                                    Prims.int_zero;
                                                                    FStar_Reflection_Data.bv_sort
                                                                    =
                                                                    (FStar_Reflection_Basic.pack_ln
                                                                    FStar_Reflection_Data.Tv_Unknown)
                                                                    }))),
                                                                    FStar_Reflection_Data.Q_Explicit)))),
                                                                    ((FStar_Reflection_Basic.pack_ln
                                                                    (FStar_Reflection_Data.Tv_FVar
                                                                    (FStar_Reflection_Basic.pack_fv
                                                                    ["Prims";
                                                                    "LexTop"]))),
                                                                    FStar_Reflection_Data.Q_Explicit)))),
                                                                    FStar_Reflection_Data.Q_Explicit))))))),
                                                           FStar_Reflection_Data.Q_Explicit))))))),
                                          FStar_Reflection_Data.Q_Explicit)))),
                                 FStar_Reflection_Data.Q_Explicit)))), body)))
                 (FStar_Tactics_Types.decr_depth
                    (FStar_Tactics_Types.set_proofstate_range ps'
                       (FStar_Range.prims_to_fstar_range
                          (Prims.mk_range "Data.Serialize.Helpers.fst"
                             (Prims.of_int (105)) (Prims.of_int (4))
                             (Prims.of_int (111)) (Prims.of_int (5)))))))
      | FStar_Tactics_Result.Failed (e, ps') ->
          FStar_Tactics_Result.Failed (e, ps')
let (add_admit_decr_lex :
  FStar_Reflection_Types.term ->
    FStar_Reflection_Types.term ->
      (FStar_Reflection_Types.term, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun v1 ->
    fun body ->
      fun ps ->
        match (FStar_Tactics_Derived.fresh_bv
                 (FStar_Reflection_Basic.pack_ln
                    FStar_Reflection_Data.Tv_Unknown))
                (FStar_Tactics_Types.incr_depth
                   (FStar_Tactics_Types.set_proofstate_range ps
                      (FStar_Range.prims_to_fstar_range
                         (Prims.mk_range "Data.Serialize.Helpers.fst"
                            (Prims.of_int (115)) (Prims.of_int (16))
                            (Prims.of_int (115)) (Prims.of_int (29))))))
        with
        | FStar_Tactics_Result.Success (a, ps') ->
            (match () with
             | () ->
                 (match match match (lex v1 v1)
                                      (FStar_Tactics_Types.incr_depth
                                         (FStar_Tactics_Types.set_proofstate_range
                                            (FStar_Tactics_Types.incr_depth
                                               (FStar_Tactics_Types.set_proofstate_range
                                                  (FStar_Tactics_Types.incr_depth
                                                     (FStar_Tactics_Types.set_proofstate_range
                                                        (FStar_Tactics_Types.decr_depth
                                                           (FStar_Tactics_Types.set_proofstate_range
                                                              ps'
                                                              (FStar_Range.prims_to_fstar_range
                                                                 (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (116))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (121))
                                                                    (Prims.of_int (5))))))
                                                        (FStar_Range.prims_to_fstar_range
                                                           (Prims.mk_range
                                                              "Data.Serialize.Helpers.fst"
                                                              (Prims.of_int (117))
                                                              (Prims.of_int (4))
                                                              (Prims.of_int (121))
                                                              (Prims.of_int (5))))))
                                                  (FStar_Range.prims_to_fstar_range
                                                     (Prims.mk_range
                                                        "Data.Serialize.Helpers.fst"
                                                        (Prims.of_int (119))
                                                        (Prims.of_int (6))
                                                        (Prims.of_int (119))
                                                        (Prims.of_int (33))))))
                                            (FStar_Range.prims_to_fstar_range
                                               (Prims.mk_range
                                                  "Data.Serialize.Helpers.fst"
                                                  (Prims.of_int (119))
                                                  (Prims.of_int (23))
                                                  (Prims.of_int (119))
                                                  (Prims.of_int (32))))))
                              with
                              | FStar_Tactics_Result.Success (a1, ps'1) ->
                                  (match () with
                                   | () ->
                                       (FStar_Tactics_Builtins.pack
                                          (FStar_Reflection_Data.Tv_App
                                             ((FStar_Reflection_Basic.pack_ln
                                                 (FStar_Reflection_Data.Tv_FVar
                                                    (FStar_Reflection_Basic.pack_fv
                                                       ["Prims"; "admitP"]))),
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
                              | FStar_Tactics_Result.Failed (e, ps'1) ->
                                  FStar_Tactics_Result.Failed (e, ps'1)
                        with
                        | FStar_Tactics_Result.Success (a1, ps'1) ->
                            (match () with
                             | () ->
                                 FStar_Tactics_Result.Success
                                   ((FStar_Reflection_Data.Tv_Let
                                       (false, [], a, a1, body)),
                                     (FStar_Tactics_Types.decr_depth
                                        (FStar_Tactics_Types.set_proofstate_range
                                           ps'1
                                           (FStar_Range.prims_to_fstar_range
                                              (Prims.mk_range
                                                 "Data.Serialize.Helpers.fst"
                                                 (Prims.of_int (117))
                                                 (Prims.of_int (4))
                                                 (Prims.of_int (121))
                                                 (Prims.of_int (5))))))))
                        | FStar_Tactics_Result.Failed (e, ps'1) ->
                            FStar_Tactics_Result.Failed (e, ps'1)
                  with
                  | FStar_Tactics_Result.Success (a1, ps'1) ->
                      (match () with
                       | () ->
                           (FStar_Tactics_Builtins.pack a1)
                             (FStar_Tactics_Types.decr_depth
                                (FStar_Tactics_Types.set_proofstate_range
                                   ps'1
                                   (FStar_Range.prims_to_fstar_range
                                      (Prims.mk_range
                                         "Data.Serialize.Helpers.fst"
                                         (Prims.of_int (116))
                                         (Prims.of_int (4))
                                         (Prims.of_int (121))
                                         (Prims.of_int (5)))))))
                  | FStar_Tactics_Result.Failed (e, ps'1) ->
                      FStar_Tactics_Result.Failed (e, ps'1)))
        | FStar_Tactics_Result.Failed (e, ps') ->
            FStar_Tactics_Result.Failed (e, ps')
let (mkLet_tup' :
  FStar_Reflection_Types.term ->
    (((FStar_Reflection_Types.term ->
         (FStar_Reflection_Types.term, unit)
           FStar_Tactics_Effect._dm4f_TAC_repr)
       * (FStar_Reflection_Types.bv * FStar_Reflection_Types.bv)),
      unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun def ->
    fun ps ->
      match (FStar_Tactics_Derived.fresh_bv
               (FStar_Reflection_Basic.pack_ln
                  (FStar_Reflection_Data.Tv_App
                     ((FStar_Reflection_Basic.pack_ln
                         (FStar_Reflection_Data.Tv_App
                            ((FStar_Reflection_Basic.pack_ln
                                (FStar_Reflection_Data.Tv_FVar
                                   (FStar_Reflection_Basic.pack_fv
                                      ["FStar";
                                      "Pervasives";
                                      "Native";
                                      "tuple2"]))),
                              ((FStar_Reflection_Basic.pack_ln
                                  FStar_Reflection_Data.Tv_Unknown),
                                FStar_Reflection_Data.Q_Explicit)))),
                       ((FStar_Reflection_Basic.pack_ln
                           FStar_Reflection_Data.Tv_Unknown),
                         FStar_Reflection_Data.Q_Explicit)))))
              (FStar_Tactics_Types.incr_depth
                 (FStar_Tactics_Types.set_proofstate_range ps
                    (FStar_Range.prims_to_fstar_range
                       (Prims.mk_range "Data.Serialize.Helpers.fst"
                          (Prims.of_int (125)) (Prims.of_int (14))
                          (Prims.of_int (125)) (Prims.of_int (33))))))
      with
      | FStar_Tactics_Result.Success (a, ps') ->
          (match () with
           | () ->
               (match (FStar_Tactics_Derived.fresh_bv
                         (FStar_Reflection_Basic.pack_ln
                            FStar_Reflection_Data.Tv_Unknown))
                        (FStar_Tactics_Types.incr_depth
                           (FStar_Tactics_Types.set_proofstate_range
                              (FStar_Tactics_Types.decr_depth
                                 (FStar_Tactics_Types.set_proofstate_range
                                    ps'
                                    (FStar_Range.prims_to_fstar_range
                                       (Prims.mk_range
                                          "Data.Serialize.Helpers.fst"
                                          (Prims.of_int (126))
                                          (Prims.of_int (4))
                                          (Prims.of_int (143))
                                          (Prims.of_int (21))))))
                              (FStar_Range.prims_to_fstar_range
                                 (Prims.mk_range "Data.Serialize.Helpers.fst"
                                    (Prims.of_int (126)) (Prims.of_int (16))
                                    (Prims.of_int (126)) (Prims.of_int (29))))))
                with
                | FStar_Tactics_Result.Success (a1, ps'1) ->
                    (match () with
                     | () ->
                         (match (FStar_Tactics_Derived.fresh_bv
                                   (FStar_Reflection_Basic.pack_ln
                                      FStar_Reflection_Data.Tv_Unknown))
                                  (FStar_Tactics_Types.incr_depth
                                     (FStar_Tactics_Types.set_proofstate_range
                                        (FStar_Tactics_Types.decr_depth
                                           (FStar_Tactics_Types.set_proofstate_range
                                              ps'1
                                              (FStar_Range.prims_to_fstar_range
                                                 (Prims.mk_range
                                                    "Data.Serialize.Helpers.fst"
                                                    (Prims.of_int (127))
                                                    (Prims.of_int (4))
                                                    (Prims.of_int (143))
                                                    (Prims.of_int (21))))))
                                        (FStar_Range.prims_to_fstar_range
                                           (Prims.mk_range
                                              "Data.Serialize.Helpers.fst"
                                              (Prims.of_int (127))
                                              (Prims.of_int (16))
                                              (Prims.of_int (127))
                                              (Prims.of_int (29))))))
                          with
                          | FStar_Tactics_Result.Success (a2, ps'2) ->
                              (match () with
                               | () ->
                                   FStar_Tactics_Result.Success
                                     ((((fun body ->
                                           fun ps1 ->
                                             match match match match 
                                                                 match 
                                                                   (FStar_Tactics_Derived.bv_to_term
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
                                                                    ps1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (130))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (142))
                                                                    (Prims.of_int (7))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (133))
                                                                    (Prims.of_int (7))
                                                                    (Prims.of_int (141))
                                                                    (Prims.of_int (9))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (133))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (141))
                                                                    (Prims.of_int (8))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (135))
                                                                    (Prims.of_int (9))
                                                                    (Prims.of_int (135))
                                                                    (Prims.of_int (40))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (135))
                                                                    (Prims.of_int (23))
                                                                    (Prims.of_int (135))
                                                                    (Prims.of_int (39))))))
                                                                 with
                                                                 | FStar_Tactics_Result.Success
                                                                    (a3,
                                                                    ps'3) ->
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
                                                                    (a3,
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
                                                                 | FStar_Tactics_Result.Failed
                                                                    (e, ps'3)
                                                                    ->
                                                                    FStar_Tactics_Result.Failed
                                                                    (e, ps'3)
                                                               with
                                                               | FStar_Tactics_Result.Success
                                                                   (a3, ps'3)
                                                                   ->
                                                                   (match ()
                                                                    with
                                                                    | 
                                                                    () ->
                                                                    (match 
                                                                    match 
                                                                    match 
                                                                    match 
                                                                    (FStar_Tactics_Derived.bv_to_term
                                                                    a)
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
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (133))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (141))
                                                                    (Prims.of_int (8))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (136))
                                                                    (Prims.of_int (9))
                                                                    (Prims.of_int (140))
                                                                    (Prims.of_int (11))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (136))
                                                                    (Prims.of_int (15))
                                                                    (Prims.of_int (140))
                                                                    (Prims.of_int (10))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (138))
                                                                    (Prims.of_int (11))
                                                                    (Prims.of_int (138))
                                                                    (Prims.of_int (42))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (138))
                                                                    (Prims.of_int (25))
                                                                    (Prims.of_int (138))
                                                                    (Prims.of_int (41))))))
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
                                                                    ["FStar";
                                                                    "Pervasives";
                                                                    "Native";
                                                                    "snd"]))),
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
                                                                    ((FStar_Reflection_Data.Tv_Let
                                                                    (false,
                                                                    [], a2,
                                                                    a4, body)),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (136))
                                                                    (Prims.of_int (15))
                                                                    (Prims.of_int (140))
                                                                    (Prims.of_int (10))))))))
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
                                                                    (FStar_Tactics_Builtins.pack
                                                                    a4)
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (136))
                                                                    (Prims.of_int (9))
                                                                    (Prims.of_int (140))
                                                                    (Prims.of_int (11)))))))
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
                                                                    ((FStar_Reflection_Data.Tv_Let
                                                                    (false,
                                                                    [], a1,
                                                                    a3, a4)),
                                                                    (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'4
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (133))
                                                                    (Prims.of_int (14))
                                                                    (Prims.of_int (141))
                                                                    (Prims.of_int (8))))))))
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
                                                                    (e, ps'3)
                                                         with
                                                         | FStar_Tactics_Result.Success
                                                             (a3, ps'3) ->
                                                             (match () with
                                                              | () ->
                                                                  (FStar_Tactics_Builtins.pack
                                                                    a3)
                                                                    (
                                                                    FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (133))
                                                                    (Prims.of_int (7))
                                                                    (Prims.of_int (141))
                                                                    (Prims.of_int (9)))))))
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
                                                              ((FStar_Reflection_Data.Tv_Let
                                                                  (false, [],
                                                                    a, def,
                                                                    a3)),
                                                                (FStar_Tactics_Types.decr_depth
                                                                   (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'3
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (130))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (142))
                                                                    (Prims.of_int (7))))))))
                                                   | FStar_Tactics_Result.Failed
                                                       (e, ps'3) ->
                                                       FStar_Tactics_Result.Failed
                                                         (e, ps'3)
                                             with
                                             | FStar_Tactics_Result.Success
                                                 (a3, ps'3) ->
                                                 (match () with
                                                  | () ->
                                                      (FStar_Tactics_Builtins.pack
                                                         a3)
                                                        (FStar_Tactics_Types.decr_depth
                                                           (FStar_Tactics_Types.set_proofstate_range
                                                              ps'3
                                                              (FStar_Range.prims_to_fstar_range
                                                                 (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (129))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (142))
                                                                    (Prims.of_int (7)))))))
                                             | FStar_Tactics_Result.Failed
                                                 (e, ps'3) ->
                                                 FStar_Tactics_Result.Failed
                                                   (e, ps'3))), (a1, a2)),
                                       (FStar_Tactics_Types.decr_depth
                                          (FStar_Tactics_Types.set_proofstate_range
                                             ps'2
                                             (FStar_Range.prims_to_fstar_range
                                                (Prims.mk_range
                                                   "Data.Serialize.Helpers.fst"
                                                   (Prims.of_int (128))
                                                   (Prims.of_int (4))
                                                   (Prims.of_int (143))
                                                   (Prims.of_int (21))))))))
                          | FStar_Tactics_Result.Failed (e, ps'2) ->
                              FStar_Tactics_Result.Failed (e, ps'2)))
                | FStar_Tactics_Result.Failed (e, ps'1) ->
                    FStar_Tactics_Result.Failed (e, ps'1)))
      | FStar_Tactics_Result.Failed (e, ps') ->
          FStar_Tactics_Result.Failed (e, ps')
let (mkLet_tup :
  FStar_Reflection_Types.term ->
    (FStar_Reflection_Types.bv ->
       FStar_Reflection_Types.bv ->
         (FStar_Reflection_Types.term, unit)
           FStar_Tactics_Effect._dm4f_TAC_repr)
      ->
      (FStar_Reflection_Types.term, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun def ->
    fun body ->
      fun ps ->
        match (mkLet_tup' def)
                (FStar_Tactics_Types.incr_depth
                   (FStar_Tactics_Types.set_proofstate_range ps
                      (FStar_Range.prims_to_fstar_range
                         (Prims.mk_range "Data.Serialize.Helpers.fst"
                            (Prims.of_int (148)) (Prims.of_int (20))
                            (Prims.of_int (148)) (Prims.of_int (34))))))
        with
        | FStar_Tactics_Result.Success (a, ps') ->
            (match () with
             | () ->
                 ((match a with
                   | (f, (a1, b)) ->
                       (fun ps1 ->
                          match (body a1 b)
                                  (FStar_Tactics_Types.incr_depth
                                     (FStar_Tactics_Types.set_proofstate_range
                                        ps1
                                        (FStar_Range.prims_to_fstar_range
                                           (Prims.mk_range
                                              "Data.Serialize.Helpers.fst"
                                              (Prims.of_int (149))
                                              (Prims.of_int (6))
                                              (Prims.of_int (149))
                                              (Prims.of_int (16))))))
                          with
                          | FStar_Tactics_Result.Success (a2, ps'1) ->
                              (match () with
                               | () ->
                                   (f a2)
                                     (FStar_Tactics_Types.decr_depth
                                        (FStar_Tactics_Types.set_proofstate_range
                                           ps'1
                                           (FStar_Range.prims_to_fstar_range
                                              (Prims.mk_range
                                                 "Data.Serialize.Helpers.fst"
                                                 (Prims.of_int (149))
                                                 (Prims.of_int (4))
                                                 (Prims.of_int (149))
                                                 (Prims.of_int (16)))))))
                          | FStar_Tactics_Result.Failed (e, ps'1) ->
                              FStar_Tactics_Result.Failed (e, ps'1))))
                   (FStar_Tactics_Types.decr_depth
                      (FStar_Tactics_Types.set_proofstate_range ps'
                         (FStar_Range.prims_to_fstar_range
                            (Prims.mk_range "Data.Serialize.Helpers.fst"
                               (Prims.of_int (148)) (Prims.of_int (4))
                               (Prims.of_int (149)) (Prims.of_int (16)))))))
        | FStar_Tactics_Result.Failed (e, ps') ->
            FStar_Tactics_Result.Failed (e, ps')
let admitMe : 'Auu____44405 . 'Auu____44405 -> 'Auu____44405 = fun n1 -> n1
let (admitTerm :
  FStar_Reflection_Types.term ->
    (FStar_Reflection_Types.term, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun t ->
    FStar_Tactics_Builtins.pack
      (FStar_Reflection_Data.Tv_App
         ((FStar_Reflection_Basic.pack_ln
             (FStar_Reflection_Data.Tv_FVar
                (FStar_Reflection_Basic.pack_fv
                   ["Data"; "Serialize"; "Helpers"; "admitMe"]))),
           (t, FStar_Reflection_Data.Q_Explicit)))
let rec withIndex_helper :
  'a . 'a Prims.list -> Prims.int -> (Prims.int * 'a) Prims.list =
  fun l ->
    fun n1 ->
      match l with
      | [] -> []
      | hd1::tl1 -> (n1, hd1) :: (withIndex_helper tl1 (n1 + Prims.int_one))
let withIndex : 'a . 'a Prims.list -> (Prims.int * 'a) Prims.list =
  fun l -> withIndex_helper l Prims.int_zero
let (mkMatchInt :
  FStar_Reflection_Types.term ->
    FStar_Reflection_Types.term Prims.list ->
      (FStar_Reflection_Types.term, unit) FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun n1 ->
    fun bodies ->
      fun ps ->
        match match match match match match (FStar_Tactics_Derived.fresh_bv
                                               (FStar_Reflection_Basic.pack_ln
                                                  (FStar_Reflection_Data.Tv_FVar
                                                     (FStar_Reflection_Basic.pack_fv
                                                        ["Prims"; "int"]))))
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
                                                                    ps
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (169))
                                                                    (Prims.of_int (4))
                                                                    (Prims.of_int (180))
                                                                    (Prims.of_int (5))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (170))
                                                                    (Prims.of_int (6))
                                                                    (Prims.of_int (179))
                                                                    (Prims.of_int (7))))))
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (173))
                                                                    (Prims.of_int (10))
                                                                    (Prims.of_int (178))
                                                                    (Prims.of_int (11))))))
                                                                (FStar_Range.prims_to_fstar_range
                                                                   (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (173))
                                                                    (Prims.of_int (11))
                                                                    (Prims.of_int (175))
                                                                    (Prims.of_int (33))))))
                                                          (FStar_Range.prims_to_fstar_range
                                                             (Prims.mk_range
                                                                "Data.Serialize.Helpers.fst"
                                                                (Prims.of_int (173))
                                                                (Prims.of_int (11))
                                                                (Prims.of_int (174))
                                                                (Prims.of_int (28))))))
                                                    (FStar_Range.prims_to_fstar_range
                                                       (Prims.mk_range
                                                          "Data.Serialize.Helpers.fst"
                                                          (Prims.of_int (173))
                                                          (Prims.of_int (20))
                                                          (Prims.of_int (174))
                                                          (Prims.of_int (28))))))
                                      with
                                      | FStar_Tactics_Result.Success 
                                          (a, ps') ->
                                          (match () with
                                           | () ->
                                               FStar_Tactics_Result.Success
                                                 ((FStar_Reflection_Data.Pat_Wild
                                                     a),
                                                   (FStar_Tactics_Types.decr_depth
                                                      (FStar_Tactics_Types.set_proofstate_range
                                                         ps'
                                                         (FStar_Range.prims_to_fstar_range
                                                            (Prims.mk_range
                                                               "Data.Serialize.Helpers.fst"
                                                               (Prims.of_int (173))
                                                               (Prims.of_int (11))
                                                               (Prims.of_int (174))
                                                               (Prims.of_int (28))))))))
                                      | FStar_Tactics_Result.Failed (e, ps')
                                          ->
                                          FStar_Tactics_Result.Failed
                                            (e, ps')
                                with
                                | FStar_Tactics_Result.Success (a, ps') ->
                                    (match () with
                                     | () ->
                                         FStar_Tactics_Result.Success
                                           ((a,
                                              (FStar_Reflection_Basic.pack_ln
                                                 (FStar_Reflection_Data.Tv_App
                                                    ((FStar_Reflection_Basic.pack_ln
                                                        (FStar_Reflection_Data.Tv_FVar
                                                           (FStar_Reflection_Basic.pack_fv
                                                              ["Data";
                                                              "Serialize";
                                                              "Helpers";
                                                              "mkerror"]))),
                                                      ((FStar_Reflection_Basic.pack_ln
                                                          (FStar_Reflection_Data.Tv_Const
                                                             (FStar_Reflection_Data.C_String
                                                                "mkMatchInt"))),
                                                        FStar_Reflection_Data.Q_Explicit))))),
                                             (FStar_Tactics_Types.decr_depth
                                                (FStar_Tactics_Types.set_proofstate_range
                                                   ps'
                                                   (FStar_Range.prims_to_fstar_range
                                                      (Prims.mk_range
                                                         "Data.Serialize.Helpers.fst"
                                                         (Prims.of_int (173))
                                                         (Prims.of_int (11))
                                                         (Prims.of_int (175))
                                                         (Prims.of_int (33))))))))
                                | FStar_Tactics_Result.Failed (e, ps') ->
                                    FStar_Tactics_Result.Failed (e, ps')
                          with
                          | FStar_Tactics_Result.Success (a, ps') ->
                              (match () with
                               | () ->
                                   FStar_Tactics_Result.Success
                                     ([a],
                                       (FStar_Tactics_Types.decr_depth
                                          (FStar_Tactics_Types.set_proofstate_range
                                             ps'
                                             (FStar_Range.prims_to_fstar_range
                                                (Prims.mk_range
                                                   "Data.Serialize.Helpers.fst"
                                                   (Prims.of_int (173))
                                                   (Prims.of_int (10))
                                                   (Prims.of_int (178))
                                                   (Prims.of_int (11))))))))
                          | FStar_Tactics_Result.Failed (e, ps') ->
                              FStar_Tactics_Result.Failed (e, ps')
                    with
                    | FStar_Tactics_Result.Success (a, ps') ->
                        (match () with
                         | () ->
                             FStar_Tactics_Result.Success
                               ((FStar_List_Tot_Base.append
                                   (FStar_List_Tot_Base.map
                                      (fun uu____44829 ->
                                         match uu____44829 with
                                         | (i, body) ->
                                             ((FStar_Reflection_Data.Pat_Constant
                                                 (FStar_Reflection_Data.C_Int
                                                    i)), body))
                                      (withIndex bodies)) a),
                                 (FStar_Tactics_Types.decr_depth
                                    (FStar_Tactics_Types.set_proofstate_range
                                       ps'
                                       (FStar_Range.prims_to_fstar_range
                                          (Prims.mk_range
                                             "Data.Serialize.Helpers.fst"
                                             (Prims.of_int (170))
                                             (Prims.of_int (6))
                                             (Prims.of_int (179))
                                             (Prims.of_int (7))))))))
                    | FStar_Tactics_Result.Failed (e, ps') ->
                        FStar_Tactics_Result.Failed (e, ps')
              with
              | FStar_Tactics_Result.Success (a, ps') ->
                  (match () with
                   | () ->
                       FStar_Tactics_Result.Success
                         ((FStar_Reflection_Data.Tv_Match (n1, a)),
                           (FStar_Tactics_Types.decr_depth
                              (FStar_Tactics_Types.set_proofstate_range ps'
                                 (FStar_Range.prims_to_fstar_range
                                    (Prims.mk_range
                                       "Data.Serialize.Helpers.fst"
                                       (Prims.of_int (169))
                                       (Prims.of_int (4))
                                       (Prims.of_int (180))
                                       (Prims.of_int (5))))))))
              | FStar_Tactics_Result.Failed (e, ps') ->
                  FStar_Tactics_Result.Failed (e, ps')
        with
        | FStar_Tactics_Result.Success (a, ps') ->
            (match () with
             | () ->
                 (FStar_Tactics_Builtins.pack a)
                   (FStar_Tactics_Types.decr_depth
                      (FStar_Tactics_Types.set_proofstate_range ps'
                         (FStar_Range.prims_to_fstar_range
                            (Prims.mk_range "Data.Serialize.Helpers.fst"
                               (Prims.of_int (168)) (Prims.of_int (4))
                               (Prims.of_int (180)) (Prims.of_int (5)))))))
        | FStar_Tactics_Result.Failed (e, ps') ->
            FStar_Tactics_Result.Failed (e, ps')
let (mkIfThenElse :
  FStar_Reflection_Types.term ->
    FStar_Reflection_Types.term ->
      FStar_Reflection_Types.term ->
        (FStar_Reflection_Types.term, unit)
          FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun condition ->
    fun bodyTrue ->
      fun bodyFalse ->
        FStar_Tactics_Builtins.pack
          (FStar_Reflection_Data.Tv_Match
             (condition,
               [((FStar_Reflection_Data.Pat_Constant
                    FStar_Reflection_Data.C_True), bodyTrue);
               ((FStar_Reflection_Data.Pat_Constant
                   FStar_Reflection_Data.C_False), bodyFalse)]))
let (mkMatchInductive :
  Data_Serialize_Types.inductiveSumup ->
    FStar_Reflection_Types.term ->
      (FStar_Reflection_Types.bv Prims.list ->
         (FStar_Reflection_Types.term, unit)
           FStar_Tactics_Effect._dm4f_TAC_repr)
        Prims.list ->
        (FStar_Reflection_Types.term, unit)
          FStar_Tactics_Effect._dm4f_TAC_repr)
  =
  fun s ->
    fun head1 ->
      fun bodies ->
        fun ps ->
          match match match match (FStar_Tactics_Util.zip
                                     s.Data_Serialize_Types.iCons bodies)
                                    (FStar_Tactics_Types.incr_depth
                                       (FStar_Tactics_Types.set_proofstate_range
                                          (FStar_Tactics_Types.incr_depth
                                             (FStar_Tactics_Types.set_proofstate_range
                                                (FStar_Tactics_Types.incr_depth
                                                   (FStar_Tactics_Types.set_proofstate_range
                                                      (FStar_Tactics_Types.incr_depth
                                                         (FStar_Tactics_Types.set_proofstate_range
                                                            ps
                                                            (FStar_Range.prims_to_fstar_range
                                                               (Prims.mk_range
                                                                  "Data.Serialize.Helpers.fst"
                                                                  (Prims.of_int (197))
                                                                  (Prims.of_int (4))
                                                                  (Prims.of_int (208))
                                                                  (Prims.of_int (5))))))
                                                      (FStar_Range.prims_to_fstar_range
                                                         (Prims.mk_range
                                                            "Data.Serialize.Helpers.fst"
                                                            (Prims.of_int (198))
                                                            (Prims.of_int (6))
                                                            (Prims.of_int (207))
                                                            (Prims.of_int (7))))))
                                                (FStar_Range.prims_to_fstar_range
                                                   (Prims.mk_range
                                                      "Data.Serialize.Helpers.fst"
                                                      (Prims.of_int (198))
                                                      (Prims.of_int (22))
                                                      (Prims.of_int (198))
                                                      (Prims.of_int (56))))))
                                          (FStar_Range.prims_to_fstar_range
                                             (Prims.mk_range
                                                "Data.Serialize.Helpers.fst"
                                                (Prims.of_int (198))
                                                (Prims.of_int (32))
                                                (Prims.of_int (198))
                                                (Prims.of_int (56))))))
                            with
                            | FStar_Tactics_Result.Success (a, ps') ->
                                (match () with
                                 | () ->
                                     FStar_Tactics_Result.Success
                                       ((withIndex a),
                                         (FStar_Tactics_Types.decr_depth
                                            (FStar_Tactics_Types.set_proofstate_range
                                               ps'
                                               (FStar_Range.prims_to_fstar_range
                                                  (Prims.mk_range
                                                     "Data.Serialize.Helpers.fst"
                                                     (Prims.of_int (198))
                                                     (Prims.of_int (22))
                                                     (Prims.of_int (198))
                                                     (Prims.of_int (56))))))))
                            | FStar_Tactics_Result.Failed (e, ps') ->
                                FStar_Tactics_Result.Failed (e, ps')
                      with
                      | FStar_Tactics_Result.Success (a, ps') ->
                          (match () with
                           | () ->
                               (FStar_Tactics_Util.map
                                  (fun uu____45599 ->
                                     match uu____45599 with
                                     | (i, ((name, args), body)) ->
                                         (fun ps1 ->
                                            match (FStar_Tactics_Util.map
                                                     (fun uu____45853 ->
                                                        FStar_Tactics_Derived.fresh_bv
                                                          (FStar_Reflection_Basic.pack_ln
                                                             FStar_Reflection_Data.Tv_Unknown))
                                                     args)
                                                    (FStar_Tactics_Types.incr_depth
                                                       (FStar_Tactics_Types.set_proofstate_range
                                                          ps1
                                                          (FStar_Range.prims_to_fstar_range
                                                             (Prims.mk_range
                                                                "Data.Serialize.Helpers.fst"
                                                                (Prims.of_int (201))
                                                                (Prims.of_int (21))
                                                                (Prims.of_int (201))
                                                                (Prims.of_int (54))))))
                                            with
                                            | FStar_Tactics_Result.Success
                                                (a1, ps'1) ->
                                                (match () with
                                                 | () ->
                                                     (match (body a1)
                                                              (FStar_Tactics_Types.incr_depth
                                                                 (FStar_Tactics_Types.set_proofstate_range
                                                                    (
                                                                    FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'1
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (202))
                                                                    (Prims.of_int (12))
                                                                    (Prims.of_int (204))
                                                                    (Prims.of_int (21))))))
                                                                    (
                                                                    FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (204))
                                                                    (Prims.of_int (12))
                                                                    (Prims.of_int (204))
                                                                    (Prims.of_int (21))))))
                                                      with
                                                      | FStar_Tactics_Result.Success
                                                          (a2, ps'2) ->
                                                          (match () with
                                                           | () ->
                                                               FStar_Tactics_Result.Success
                                                                 (((FStar_Reflection_Data.Pat_Cons
                                                                    ((FStar_Reflection_Basic.pack_fv
                                                                    name),
                                                                    (FStar_List_Tot_Base.map
                                                                    (fun x ->
                                                                    ((FStar_Reflection_Data.Pat_Wild
                                                                    x),
                                                                    false))
                                                                    a1))),
                                                                    a2),
                                                                   (FStar_Tactics_Types.decr_depth
                                                                    (FStar_Tactics_Types.set_proofstate_range
                                                                    ps'2
                                                                    (FStar_Range.prims_to_fstar_range
                                                                    (Prims.mk_range
                                                                    "Data.Serialize.Helpers.fst"
                                                                    (Prims.of_int (202))
                                                                    (Prims.of_int (12))
                                                                    (Prims.of_int (204))
                                                                    (Prims.of_int (21))))))))
                                                      | FStar_Tactics_Result.Failed
                                                          (e, ps'2) ->
                                                          FStar_Tactics_Result.Failed
                                                            (e, ps'2)))
                                            | FStar_Tactics_Result.Failed
                                                (e, ps'1) ->
                                                FStar_Tactics_Result.Failed
                                                  (e, ps'1))) a)
                                 (FStar_Tactics_Types.decr_depth
                                    (FStar_Tactics_Types.set_proofstate_range
                                       ps'
                                       (FStar_Range.prims_to_fstar_range
                                          (Prims.mk_range
                                             "Data.Serialize.Helpers.fst"
                                             (Prims.of_int (199))
                                             (Prims.of_int (8))
                                             (Prims.of_int (206))
                                             (Prims.of_int (15)))))))
                      | FStar_Tactics_Result.Failed (e, ps') ->
                          FStar_Tactics_Result.Failed (e, ps')
                with
                | FStar_Tactics_Result.Success (a, ps') ->
                    (match () with
                     | () ->
                         FStar_Tactics_Result.Success
                           ((FStar_Reflection_Data.Tv_Match (head1, a)),
                             (FStar_Tactics_Types.decr_depth
                                (FStar_Tactics_Types.set_proofstate_range ps'
                                   (FStar_Range.prims_to_fstar_range
                                      (Prims.mk_range
                                         "Data.Serialize.Helpers.fst"
                                         (Prims.of_int (197))
                                         (Prims.of_int (4))
                                         (Prims.of_int (208))
                                         (Prims.of_int (5))))))))
                | FStar_Tactics_Result.Failed (e, ps') ->
                    FStar_Tactics_Result.Failed (e, ps')
          with
          | FStar_Tactics_Result.Success (a, ps') ->
              (match () with
               | () ->
                   (FStar_Tactics_Builtins.pack a)
                     (FStar_Tactics_Types.decr_depth
                        (FStar_Tactics_Types.set_proofstate_range ps'
                           (FStar_Range.prims_to_fstar_range
                              (Prims.mk_range "Data.Serialize.Helpers.fst"
                                 (Prims.of_int (196)) (Prims.of_int (4))
                                 (Prims.of_int (208)) (Prims.of_int (5)))))))
          | FStar_Tactics_Result.Failed (e, ps') ->
              FStar_Tactics_Result.Failed (e, ps')