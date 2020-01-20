module MetaTools.BrowseTerm

open FStar.Tactics
open Control.Monoid

open FStar.Tactics.Typeclasses
open MetaTools.Util

module L = FStar.List.Tot


unfold let browseFun s = bool -> (term -> Tac term) -> list bv -> list term_view_head -> term -> Tac (term * s)

let rec browse_term' (#s: Type0) [| monoid s |]
  (f: browseFun s)
  (getTree: term -> Tac term)
  (variables: list bv)
  (parents': list term_view_head)
  (t: term)
  : Tac (term * s) =
  let t, s0 = f true getTree variables parents' t in
  let parents = term_head t::parents' in
  let t, s1 = match inspect t with
  | Tv_Var bv ->
      let bv, s0, _ = browse_bv f (fun bv -> pack (Tv_Var bv)) variables parents true bv in
      pack (Tv_Var bv), s0
  | Tv_BVar bv ->
      let bv, s0, _ = browse_bv f (fun bv -> pack (Tv_BVar bv)) variables parents false bv in
      pack (Tv_BVar bv), s0
  | Tv_App t (arg, q) -> 
      let t,  s0 = browse_term' f (fun t   -> getTree (pack (Tv_App t (arg, q)))) variables parents t in
      let arg,s1 = browse_term' f (fun arg -> getTree (pack (Tv_App t (arg, q)))) variables parents arg in
      pack (Tv_App t (arg, q)), mempty//s0 @@ s1
  | Tv_Abs b body ->
      let b, s0, variables
        = browse_binder f (fun b -> getTree (pack (Tv_Abs b body))) variables parents false b in 
        
      let body, s1 = browse_term' f (fun body -> getTree (pack (Tv_Abs b body))) variables parents body in
      pack (Tv_Abs b body), s0 <+> s1
  | Tv_Arrow  b c -> 
      let b,s0,_ = browse_binder f (fun b -> getTree (pack (Tv_Arrow b c))) variables parents true b in
      pack (Tv_Arrow b c), s0
  | Tv_Refine bv t ->
      let bv,s0,variables = browse_bv f (fun bv -> pack (Tv_Refine bv t)) variables parents false bv in
      let t, s1 = browse_term' f (fun t -> pack (Tv_Refine bv t)) variables parents t in
      pack (Tv_Refine bv t), s0 <+> s1
  | Tv_Let r attrs bv t1 t2 -> 
      let bv,s0,variables = browse_bv f 
          (fun bv -> pack (Tv_Let r attrs bv t1 t2)) variables parents false bv in
      let t1, s1 = browse_term' f
          (fun t -> pack (Tv_Let r attrs bv t1 t2)) variables parents t1 in
      let t2, s2 = browse_term' f
          (fun t -> pack (Tv_Let r attrs bv t1 t2)) variables parents t2 in
      pack (Tv_Let r attrs bv t1 t2), s1 <+> s2
  | Tv_Match t branches -> 
      let t, s0 = browse_term' f (fun t -> pack (Tv_Match t branches)) variables parents t in
      let brs = withIndexes branches in
      let raw = map (
        fun (i, (pattern, term)) ->
          let getTree term: Tac _
            = pack (Tv_Match t (replaceAt branches i (pattern, term))) in
          let variables = bvs_of_pattern pattern @ variables in
          let term, s0
            = browse_term' f getTree variables parents term
          in (pattern, term), s0
        ) brs
      in
      pack (Tv_Match t (L.map fst raw))
      , s0 <+> mconcat (L.map snd raw)
  // | Tv_AscribedT _ _ _ -> "Tv_AscribedT" //TODO!
  // | Tv_AscribedC _ _ _ -> "Tv_AscribedC" //TODO!
  | _ -> t, mempty
  in
  let t, s2 = f false getTree variables parents' t in
  t, s0 <+> s1 <+> s2
and browse_bv (#s: Type0) [| monoid s |]
  (f: browseFun s)
  (getTree: bv -> Tac term)
  (variables: list bv)
  (parents: list term_view_head)
  (shallow: bool)
  (v: bv)
  : Tac (bv * s * list bv)
  = let variables = v::variables in
    let bv = inspect_bv v in
    let pack' sort = pack_bv ({bv with bv_sort=sort}) in
    let getTree sort = getTree (pack' sort) in
    let bv_sort, s0
      = if shallow
        then bv.bv_sort, mempty
        else browse_term' #s f getTree variables parents bv.bv_sort in
    pack' bv_sort, s0, variables
and browse_binder (#s: Type0) [| monoid s |]
  (f: browseFun s)
  (getTree: binder -> Tac term)
  (variables: list bv)
  (parents: list term_view_head)
  (shallow: bool)
  (b: binder)
  : Tac (binder * s * list bv)
  = let v, aqualv = inspect_binder b in
    let bv, s, variables = browse_bv #s f (fun bv -> getTree (pack_binder bv aqualv)) variables parents shallow v in
    pack_binder bv aqualv, s, variables

let browse_term (#s: Type0) [| monoid s |]
  (f: browseFun s)
  (t: term)
  : Tac (term * s) = browse_term' f id [] [] t
