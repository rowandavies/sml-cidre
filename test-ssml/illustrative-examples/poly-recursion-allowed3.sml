
datatype 'a l = C1 of 'a * ('a l) l | C2

(*[ datasort 'a l1 = C1 of 'a * 'a l1 l ]*)
(*[ datasort 'a l2 = C2 ]*)

(*[ val f :> 'a l1 l1 l1 l1 -> 'a l1 l1 l1 l1 l
           & 'a l2 l1 l1 l2 -> 'a l l1 l1 l1 l  ]*)
fun f x = C1 (x, C2)
