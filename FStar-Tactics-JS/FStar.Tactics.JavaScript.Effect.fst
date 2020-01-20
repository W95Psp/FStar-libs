module FStar.Tactics.JavaScript.Effect

open FStar.All

effect JS (a:Type) = ALL a (fun (p:all_post a) _ -> forall (a:result a) h. p a h)




