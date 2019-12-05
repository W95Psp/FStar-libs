module Data.Function

unfold let const (x: 'a) (_: 'b): 'a = x 
unfold let (@@) (g: 'b -> 'c) (f: 'a -> 'b) (a: 'a) = g (f a)
unfold let flip (f: 'a -> 'b -> 'c) (b: 'b) (a: 'a): 'c = f a b

unfold let ( |> ) (v:'a) (f: 'a -> 'b): 'b = f v
unfold let ( <| ) (f: 'a -> 'b) (v:'a): 'b = f v

unfold let on (f: 'a -> 'a -> 'b) (t : 'c -> 'a) (v:'c) (w:'c): 'b = t v `f` t w
