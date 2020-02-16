open Prims
let (call1_test : Prims.int) = (Prims.of_int (42))

let (call2_test : Prims.int) = (Prims.of_int (42))

let (mkLet_tup'_test : Prims.int) =
  (FStar_Pervasives_Native.fst ((Prims.of_int (12)), (Prims.of_int (42)))) +
    (FStar_Pervasives_Native.snd ((Prims.of_int (12)), (Prims.of_int (42))))

let (mkMatchInt_test : Prims.int) = (Prims.of_int (333))
let (mkMatchInt_test2 : Prims.int -> Prims.int) =
  fun x0 ->
    match x0 with
    | _5311 when _5311 = Prims.int_zero -> (Prims.of_int (111))
    | _5313 when _5313 = Prims.int_one -> (Prims.of_int (222))
    | _5315 when _5315 = (Prims.of_int (2)) -> (Prims.of_int (333))
    | _5317 when _5317 = (Prims.of_int (3)) -> (Prims.of_int (444))
    | _5319 when _5319 = (Prims.of_int (4)) -> (Prims.of_int (555))
    | x1 -> Data_Serialize_Helpers.mkerror "mkMatchInt"
type mkMatchInductive_test_typ =
  | MkMatchInductive_testA of Prims.int 
  | MkMatchInductive_testB of Prims.string 
let (uu___is_MkMatchInductive_testA :
  mkMatchInductive_test_typ -> Prims.bool) =
  fun projectee ->
    match projectee with
    | MkMatchInductive_testA _0 -> true
    | uu____5350 -> false
let (__proj__MkMatchInductive_testA__item___0 :
  mkMatchInductive_test_typ -> Prims.int) =
  fun projectee -> match projectee with | MkMatchInductive_testA _0 -> _0
let (uu___is_MkMatchInductive_testB :
  mkMatchInductive_test_typ -> Prims.bool) =
  fun projectee ->
    match projectee with
    | MkMatchInductive_testB _0 -> true
    | uu____5376 -> false
let (__proj__MkMatchInductive_testB__item___0 :
  mkMatchInductive_test_typ -> Prims.string) =
  fun projectee -> match projectee with | MkMatchInductive_testB _0 -> _0