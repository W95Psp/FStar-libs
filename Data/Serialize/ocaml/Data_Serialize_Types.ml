open Prims
type 'Aargs argSumup =
  | AS_Int 
  | AS_String 
  | AS_Bool 
  | AS_List of unit argSumup 
  | AS_Inductive of FStar_Reflection_Types.name * unit argSumup Prims.list 
  | AS_TVar of Prims.nat 
let (uu___is_AS_Int : Prims.nat -> unit argSumup -> Prims.bool) =
  fun args ->
    fun projectee ->
      match projectee with | AS_Int -> true | uu____5891 -> false
let (uu___is_AS_String : Prims.nat -> unit argSumup -> Prims.bool) =
  fun args ->
    fun projectee ->
      match projectee with | AS_String -> true | uu____5921 -> false
let (uu___is_AS_Bool : Prims.nat -> unit argSumup -> Prims.bool) =
  fun args ->
    fun projectee ->
      match projectee with | AS_Bool -> true | uu____5951 -> false
let (uu___is_AS_List : Prims.nat -> unit argSumup -> Prims.bool) =
  fun args ->
    fun projectee ->
      match projectee with | AS_List _0 -> true | uu____5987 -> false
let (__proj__AS_List__item___0 : Prims.nat -> unit argSumup -> unit argSumup)
  = fun args -> fun projectee -> match projectee with | AS_List _0 -> _0
let (uu___is_AS_Inductive : Prims.nat -> unit argSumup -> Prims.bool) =
  fun args ->
    fun projectee ->
      match projectee with
      | AS_Inductive (_0, _1) -> true
      | uu____6059 -> false
let (__proj__AS_Inductive__item___0 :
  Prims.nat -> unit argSumup -> FStar_Reflection_Types.name) =
  fun args ->
    fun projectee -> match projectee with | AS_Inductive (_0, _1) -> _0
let (__proj__AS_Inductive__item___1 :
  Prims.nat -> unit argSumup -> unit argSumup Prims.list) =
  fun args ->
    fun projectee -> match projectee with | AS_Inductive (_0, _1) -> _1
let (uu___is_AS_TVar : Prims.nat -> unit argSumup -> Prims.bool) =
  fun args ->
    fun projectee ->
      match projectee with | AS_TVar argIndex -> true | uu____6162 -> false
let (__proj__AS_TVar__item__argIndex :
  Prims.nat -> unit argSumup -> Prims.nat) =
  fun args ->
    fun projectee -> match projectee with | AS_TVar argIndex -> argIndex
type 'AiVars consSumup =
  (FStar_Reflection_Types.name * unit argSumup Prims.list)
type inductiveSumup =
  {
  iName: FStar_Reflection_Types.name ;
  iVars: Prims.nat ;
  iCons: unit consSumup Prims.list }
let (__proj__MkinductiveSumup__item__iName :
  inductiveSumup -> FStar_Reflection_Types.name) =
  fun projectee -> match projectee with | { iName; iVars; iCons;_} -> iName
let (__proj__MkinductiveSumup__item__iVars : inductiveSumup -> Prims.nat) =
  fun projectee -> match projectee with | { iName; iVars; iCons;_} -> iVars
let (__proj__MkinductiveSumup__item__iCons :
  inductiveSumup -> unit consSumup Prims.list) =
  fun projectee -> match projectee with | { iName; iVars; iCons;_} -> iCons
type serialized =
  (FStar_Reflection_Types.name Prims.list * (Prims.int Prims.list *
    (Prims.string Prims.list * Prims.bool Prims.list)))
type ('Auu____6330, 'Aa) tserialized = serialized
let (serialize_size : serialized -> Prims.nat) =
  fun uu____6341 ->
    match uu____6341 with
    | (a, (b, (c, d))) ->
        (((FStar_List_Tot_Base.length a) + (FStar_List_Tot_Base.length b)) +
           (FStar_List_Tot_Base.length c))
          + (FStar_List_Tot_Base.length d)