
datatype 'a l = C1 of ('a -> 'a) l | C2 of 'a | C3

(*[ datasort 'a l1 = C1 of ('a -> 'a) l1 ]*)
(*[ datasort 'a l2 = C2 of 'a ]*)

(*[ val f :> ('a l2 -> 'a l1) l1 -> ('a l2 -> 'a l1) l1 l2 ]*)
(* fun f x = C2 x *)

val x = C2 (C1 (C2 (fn x => x)))