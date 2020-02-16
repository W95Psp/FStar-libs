/// This module defines a generic function to browse/modify a term
module MetaTools.BrowseTerm

open FStar.Tactics
open Control.Monoid
open FStar.Tactics.Typeclasses
open MetaTools.Util
module L = FStar.List.Tot

/// A browsing `browseFun` function `f` is a function that is called
/// on each and every sub-term `st` of a term `t`:
///  - `beforeTransform`. `f` is called twice during a term browse: once before transforming the tree, once after. `true` is passed when in the former situation, `false` otherwise. 
///  - `rebuildToplevel st'` reconstitutes `t` replacing `st` by `st'`
///  - `boundedVariables` is a list of the bound variables
///  - `parents` is the path between `st` and `t`
///  - `isTypeLevel` is a flag switched to `true` whenever `st` is part of a type
///  - `currentTerm` is the term `st`
///
/// `f` shall return three things:
///  - a term `newTerm`, that replace the current node
///  - a term transformer `endTermTransform`, a transformation that is applied at the very end of the transformation
///  - a new state, if `f` wants to accumulate informations from the browse
unfold let browseFun' s
  = rebuildToplevel: (term -> Tac term)
  -> boundedVariables: list bv
  -> parents: list term_view_head
  -> isTypeLevel: bool
  -> currentTerm: term
  -> Tac ((   newTerm: term
           * endTermTransform: (term -> Tac term)
         ) * newState: s)
unfold let browseFun s
  = beforeTransform: bool
  -> browseFun' s

// (beforeTransform: bool)  (rebuildToplevel: (term -> Tac term))  (boundedVariables: list bv)  (parents: list term_view_head)  (currentTerm: term)
// beforeTransform rebuildToplevel boundedVariables parents currentTerm

/// Due to a bug, one should really use `id_tac` instead of `id`
/// For instance, consider `f: (_ -> Tac _) -> Tac _`
/// `f id` will fail when _evaluated_ (it will even segfault if compiled), while `f id_tac` will work very well
let id_tac (x: 'a): Tac 'a = x 

/// `onlyWhen` forges a `browseFun` our of a `browseFun'`
let onlyWhen (#s: Type0) [| monoid s |] start (f: browseFun' s)
  : browseFun s
  = fun beforeTransform rebuildToplevel boundedVariables parents isTypeLevel currentTerm -> 
    if beforeTransform = start
    then f rebuildToplevel boundedVariables parents isTypeLevel currentTerm
    else (currentTerm, id_tac), mempty

let readOnly (#s: Type0) [| monoid s |] (f: 
    (term -> Tac term) -> list bv -> list term_view_head
  -> term -> Tac s) = onlyWhen true (fun a b c _ d -> (d, id_tac), f a b c d)

/// `browse_term'` shall not be used directly, see `browse_term`
let rec browse_term' (#s: Type0) [| sMonoid: monoid s |]
  (f: browseFun s)
  (getTree: term -> Tac term)
  (variables: list bv)
  (parents': list term_view_head)
  (typeLevel: bool)
  (t: term)
  : Tac (term * s) =
  let returns x = pack x, mempty #s #sMonoid in 
  let bind #a #b = bindM #s #sMonoid #a #b in
  bundle <-- f true getTree variables parents' typeLevel t;
  let t, final_term_transform0 = bundle in
  let parents = term_head t::parents' in
  // the next definitions are just there to provide shortcuts
  let browse_bv (gt: bv -> term_view) vars flag bv = browse_bv #s #sMonoid f (let f v: Tac _ = getTree (pack (gt v)) in f) vars parents flag bv in
  let browse_term'' (gt: _ -> Tac _) vars = browse_term'  #s #sMonoid f (let f t: Tac _ = getTree (pack (gt t)) in f) vars parents' in
  let browse_term' (gt: term -> Tot term_view) vars = browse_term' #s #sMonoid f (let f t: Tac _ = getTree (pack (gt t)) in f) vars parents' in
  let browse_binder gt  vars flag b = browse_binder #s #sMonoid f (let f t: Tac _ = getTree (pack (gt b)) in f) vars parents flag b in
  let browse_comp (gt: comp -> term_view) variables c
    : Tac (comp * s)
    = match inspect_comp c with
    | C_Total ret decr -> 
      let mk ret decr = gt (pack_comp (C_Total ret decr)) in
      ret <-- browse_term'' (fun ret -> mk ret decr) variables true ret;
      decr <-- optmapS (browse_term' (fun decr -> mk ret (Some decr)) variables true) decr;
      pack_comp (C_Total ret decr), mempty #s #sMonoid
    | C_Lemma pre post pats ->
      let mk pre post pats = gt (pack_comp (C_Lemma pre post pats)) in
      pre <-- browse_term'' (fun pre -> mk pre post pats) variables true pre;
      post <-- browse_term'' (fun post -> mk pre post pats) variables true post;
      pats <-- browse_term'' (fun pats -> mk pre post pats) variables true pats;
      pack_comp (C_Lemma pre post pats), mempty #s #sMonoid
    | _ -> c, mempty #s #sMonoid
  in
  t <-- begin match inspect t with
  | Tv_Var bv ->
      bv <-- focusFst (browse_bv Tv_Var variables true bv);
      returns (Tv_Var bv)
  | Tv_BVar bv ->
      bv <-- focusFst (browse_bv Tv_BVar variables false bv);
      returns (Tv_BVar bv)
  | Tv_App fn1 (arg1, q) -> 
      fn2 <-- browse_term' (fun fn2 -> Tv_App fn2 (arg1, q)) variables typeLevel fn1;
      arg2 <-- browse_term' (fun arg2 -> Tv_App fn1 (arg2, q)) variables typeLevel arg1;
      returns (Tv_App fn2 (arg2, q))
  | Tv_Abs b1 body1 ->
      r <-- browse_binder (fun b2 -> Tv_Abs b2 body1) variables false b1;
      let b2, variables = r in
      body2 <-- browse_term' (fun body2 -> Tv_Abs b1 body2) variables typeLevel body1;
      returns (Tv_Abs b2 body2)
  | Tv_Arrow  b1 c1 -> 
      b2 <-- focusFst (browse_binder (fun b2 -> Tv_Arrow b2 c1) variables true b1);
      c2 <-- browse_comp (fun c2 -> Tv_Arrow b1 c2) variables c1;
      pack (Tv_Arrow b2 c2), mempty #s #sMonoid
  | Tv_Refine bv1 t1 ->
      r <-- browse_bv (fun bv2 -> Tv_Refine bv2 t1) variables false bv1;
      let bv2, variables = r in
      t2 <-- browse_term' (fun t2 -> Tv_Refine bv1 t2) variables typeLevel t1;
      returns (Tv_Refine bv2 t2)
  | Tv_Let r attrs bv1 def1 body1 -> 
      res <-- browse_bv (fun bv2 -> Tv_Let r attrs bv2 def1 body1) variables false bv1;
      let bv2, variables = res in
      def2 <-- browse_term' (fun def2 -> Tv_Let r attrs bv1 def2 body1) variables typeLevel def1;
      body2 <-- browse_term' (fun body2 -> Tv_Let r attrs bv1 def1 body2) variables typeLevel body1;
      returns (Tv_Let r attrs bv2 def2 body2)
  | Tv_Match t1 branches -> 
      t2 <-- browse_term' (fun t2 -> Tv_Match t2 branches) variables typeLevel t1;
      let brs = withIndexes branches in
      let raw = map (
        fun (i, (pattern, term)) ->
          let variables = bvs_of_pattern pattern @ variables in
          let term, s0
            = browse_term' (fun term -> Tv_Match t1 (replaceAt branches i (pattern, term))) variables typeLevel term
          in (pattern, term), s0
        ) brs
      in
      pack (Tv_Match t2 (L.map fst raw)), (mconcat #s #sMonoid) (L.map snd raw)
  | Tv_AscribedT e1 t1 tac1 ->
    e2 <-- browse_term' (fun e2 -> Tv_AscribedT e2 t1 tac1) variables typeLevel e1;
    t2 <-- browse_term' (fun t2 -> Tv_AscribedT e1 t2 tac1) variables typeLevel t1;
    tac2 <-- optmapS (browse_term' (fun tac2 -> Tv_AscribedT e1 t1 (Some tac2)) variables typeLevel) tac1;
    returns (Tv_AscribedT e2 t2 tac2)
  | Tv_AscribedC e1 c1 tac1 ->
    e2 <-- browse_term' (fun e2 -> Tv_AscribedC e2 c1 tac1) variables typeLevel e1;
    c2 <-- browse_comp  (fun c2 -> Tv_AscribedC e1 c2 tac1) variables c1;
    tac2 <-- optmapS (browse_term' (fun tac2 -> Tv_AscribedC e1 c1 (Some tac2)) variables typeLevel) tac1;
    returns (Tv_AscribedC e2 c2 tac2)
  | _ -> t, mempty #s #sMonoid end;
  bundle <-- f false getTree variables parents' typeLevel t;
  let t, final_term_transform1 = bundle in
  let finish (): Tac _
    = final_term_transform1 (final_term_transform0 t)
    , mempty #s #sMonoid
  in finish ()
and browse_bv (#s: Type0) [| sMonoid: monoid s |]
  (f: browseFun s)
  (getTree: bv -> Tac term)
  (variables: list bv)
  (parents: list term_view_head)
  (shallow: bool)
  (v: bv)
  : Tac ((bv * list bv) * s)
  = let variables = v::variables in
    let bv = inspect_bv v in
    let pack' sort = pack_bv ({bv with bv_sort=sort}) in
    let getTree sort = getTree (pack' sort) in
    let bv_sort = bv.bv_sort in
    // let s0 = mempty in
    let bv_sort, s0
      = if shallow
        then bv_sort, mempty #s #sMonoid
        else browse_term' #s #sMonoid f getTree variables parents true bv_sort in
    (pack' bv_sort, variables), s0
and browse_binder (#s: Type0) [| sMonoid: monoid s |]
  (f: browseFun s)
  (getTree: binder -> Tac term)
  (variables: list bv)
  (parents: list term_view_head)
  (shallow: bool)
  (b: binder)
  : Tac ((binder * list bv) * s)
  = let bv, aqualv = inspect_binder b in
    let (bv, variables), s0 = browse_bv #s #sMonoid f (fun bv -> getTree (pack_binder bv aqualv)) variables parents shallow bv in
    (pack_binder bv aqualv, variables), s0



// browseFun s = bool -> (term -> Tac term) -> list bv -> list term_view_head -> term -> Tac (term * s)
let browse_term (#s: Type0) [| monoid s |]
  (f: browseFun s)
  (isTypeLevel: bool)
  (t: term)
  : Tac (term * s) = browse_term' f (fun x -> x) [] [] isTypeLevel t


