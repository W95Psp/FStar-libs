module Data.Set.Computable.NonOrdered.PartialOrder

open Data.Set.Computable.NonOrdered
module DSCNO = Data.Set.Computable.NonOrdered
open PartialOrder

open FStar.Tactics
open FStar.Tactics.Typeclasses

instance csetLPO #c : hasLPartialOrder (DSCNO.set c) = {
    l_po_cmp = DSCNO.equal;
    l_po = let h = (fun (a b: DSCNO.set c) -> true == DSCNO.subset a b) in admitP (isPartialOrderL DSCNO.equal h); h
}

instance gsetLPO #c : hasLPartialOrder (GSet.set c) = {
    l_po_cmp = GSet.equal;
    l_po = GSet.subset
}

