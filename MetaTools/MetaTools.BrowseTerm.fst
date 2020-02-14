module MetaTools.BrowseTerm

open FStar.Tactics
open Control.Monoid

open FStar.Tactics.Typeclasses
open MetaTools.Util

module L = FStar.List.Tot


unfold let browseFun' s
  = rebuildToplevel: (term -> Tac term)
  -> boundedVariables: list bv
  -> parents: list term_view_head
  -> currentTerm: term
  -> Tac ((   newTerm: term
           * endStateTransform: (term -> Tac term)
         ) * newState: s)
unfold let browseFun s
  = beforeTransform: bool
  -> browseFun' s

// (beforeTransform: bool)  (rebuildToplevel: (term -> Tac term))  (boundedVariables: list bv)  (parents: list term_view_head)  (currentTerm: term)
// beforeTransform rebuildToplevel boundedVariables parents currentTerm

let id_tac (x: 'a): Tac 'a = x 

let onlyWhen (#s: Type0) [| monoid s |] start (f: browseFun' s)
  : browseFun s
  = fun beforeTransform rebuildToplevel boundedVariables parents currentTerm -> 
    if beforeTransform = start
    then f rebuildToplevel boundedVariables parents currentTerm
    else (currentTerm, id_tac), mempty

let readOnly (#s: Type0) [| monoid s |] (f: 
    (term -> Tac term) -> list bv -> list term_view_head
  -> term -> Tac s) = onlyWhen true (fun a b c d -> (d, id_tac), f a b c d)

let rec browse_term' (#s: Type0) [| sMonoid: monoid s |]
  (f: browseFun s)
  (getTree: term -> Tac term)
  (variables: list bv)
  (parents': list term_view_head)
  (t: term)
  : Tac (term * s) =
  let getTree tt
    = let parents = term_head t::parents' in
      let tt' = getTree tt in
      // dump ( "### Child term ###\n"
      //      ^ term_to_string tt
      //      ^ "\n\n### Toplevel term ###\n"
      //      ^ term_to_string tt'
      //      ^ "\n\n### At level ###\n"
      //      ^ String.concat " " (L.map term_head_to_string parents)
      //      );
      tt'
  in
  let returns x = pack x, mempty #s #sMonoid in 
  let bind #a #b = bindM #s #sMonoid #a #b in
  bundle <-- f true getTree variables parents' t;
  let t, final_term_transform0 = bundle in
  let parents = term_head t::parents' in
  let browse_bv (gt: bv -> term_view) vars flag bv = browse_bv #s #sMonoid f (let f v: Tac _ = getTree (pack (gt v)) in f) vars parents flag bv in
  let browse_term'' (gt: _ -> Tac _) vars = browse_term'  #s #sMonoid f (let f t: Tac _ = getTree (pack (gt t)) in f) vars parents' in
  let browse_term' (gt: term -> Tot term_view) vars = browse_term' #s #sMonoid f (let f t: Tac _ = getTree (pack (gt t)) in f) vars parents' in
  let browse_binder gt  vars flag b = browse_binder #s #sMonoid f (let f t: Tac _ = getTree (pack (gt b)) in f) vars parents flag b in
  let browse_comp (gt: comp -> term_view) variables c // Tv_Arrow b
    : Tac (comp * s)
    = match inspect_comp c with
    | C_Total ret decr -> 
      let mk ret decr = gt (pack_comp (C_Total ret decr)) in
      ret <-- browse_term'' (fun ret -> mk ret decr) variables ret;
      decr <-- optmapS (browse_term' (fun decr -> mk ret (Some decr)) variables) decr;
      pack_comp (C_Total ret decr), mempty #s #sMonoid
    | C_Lemma pre post ->
      let mk pre post = gt (pack_comp (C_Lemma pre post)) in
      pre <-- browse_term'' (fun pre -> mk pre post) variables pre;
      post <-- browse_term'' (fun post -> mk pre post) variables post;
      pack_comp (C_Lemma pre post), mempty #s #sMonoid
    | _ -> c, mempty #s #sMonoid
  in
  t <-- begin match inspect t with
  | Tv_Var bv ->
      bv <-- focusFst (browse_bv Tv_Var variables true bv);
      returns (Tv_Var bv)
  | Tv_BVar bv ->
      bv <-- focusFst (browse_bv Tv_BVar variables false bv);
      returns (Tv_BVar bv)
  | Tv_App t (arg, q) -> 
      t <-- browse_term' (fun t -> Tv_App t (arg, q)) variables t;
      arg <-- browse_term' (fun arg -> Tv_App t (arg, q)) variables arg;
      returns (Tv_App t (arg, q))
  | Tv_Abs b body ->
      r <-- browse_binder (fun b -> Tv_Abs b body) variables false b;
      let b, variables = r in
      body <-- browse_term' (fun body -> Tv_Abs b body) variables body;
      returns (Tv_Abs b body)
  | Tv_Arrow  b c -> 
      b <-- focusFst (browse_binder (fun b -> Tv_Arrow b c) variables true b);
      c <-- browse_comp (fun c -> Tv_Arrow b c) variables c;
      pack (Tv_Arrow b c), mempty #s #sMonoid
  | Tv_Refine bv t ->
      r <-- browse_bv (fun bv -> Tv_Refine bv t) variables false bv;
      let bv, variables = r in
      t <-- browse_term' (fun t -> Tv_Refine bv t) variables t;
      returns (Tv_Refine bv t)
  | Tv_Let r attrs bv t1 t2 -> 
      res <-- browse_bv (fun bv -> Tv_Let r attrs bv t1 t2) variables false bv;
      let bv, variables = res in
      t1 <-- browse_term' (fun t1 -> Tv_Let r attrs bv t1 t2) variables t1;
      t2 <-- browse_term' (fun t2 -> Tv_Let r attrs bv t1 t2) variables t2;
      returns (Tv_Let r attrs bv t1 t2)
  | Tv_Match t branches -> 
      t <-- browse_term' (fun t -> Tv_Match t branches) variables t;
      let brs = withIndexes branches in
      let raw = map (
        fun (i, (pattern, term)) ->
          let variables = bvs_of_pattern pattern @ variables in
          let term, s0
            = browse_term' (fun term -> Tv_Match t (replaceAt branches i (pattern, term))) variables term
          in (pattern, term), s0
        ) brs
      in
      pack (Tv_Match t (L.map fst raw)), (mconcat #s #sMonoid) (L.map snd raw)
  | Tv_AscribedT e t tac ->
    e <-- browse_term' (fun e -> Tv_AscribedT e t tac) variables e;
    t <-- browse_term' (fun t -> Tv_AscribedT e t tac) variables t;
    tac <-- optmapS (browse_term' (fun tac -> Tv_AscribedT e t (Some tac)) variables) tac;
    returns (Tv_AscribedT e t tac)
  | Tv_AscribedC e c tac ->
    e <-- browse_term' (fun e -> Tv_AscribedC e c tac) variables e;
    c <-- browse_comp  (fun c -> Tv_AscribedC e c tac) variables c;
    tac <-- optmapS (browse_term' (fun tac -> Tv_AscribedC e c (Some tac)) variables) tac;
    returns (Tv_AscribedC e c tac)
  | _ -> t, mempty #s #sMonoid end;
  bundle <-- f false getTree variables parents' t;
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
    let bv_sort, s0
      = if shallow
        then bv.bv_sort, mempty #s #sMonoid
        else browse_term' #s #sMonoid f getTree variables parents bv.bv_sort in
    (pack' bv_sort, variables), s0
and browse_binder (#s: Type0) [| sMonoid: monoid s |]
  (f: browseFun s)
  (getTree: binder -> Tac term)
  (variables: list bv)
  (parents: list term_view_head)
  (shallow: bool)
  (b: binder)
  : Tac ((binder * list bv) * s)
  = let v, aqualv = inspect_binder b in
    let (bv, variables), s = browse_bv #s #sMonoid f (fun bv -> getTree (pack_binder bv aqualv)) variables parents shallow v in
    (pack_binder bv aqualv, variables), s



// browseFun s = bool -> (term -> Tac term) -> list bv -> list term_view_head -> term -> Tac (term * s)
let browse_term (#s: Type0) [| monoid s |]
  (f: browseFun s)
  (t: term)
  : Tac (term * s) = browse_term' f (fun x -> x) [] [] t


