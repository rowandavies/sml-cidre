
datatype 'a l = C1 of 'a l l | C2

(*[ datasort 'a l1 = C1 of 'a l1 l1 ]*)
(*[ datasort 'a l2 = C2 ]*)

(*[ val f :> 'a l1 l1 l1 l1 -> 'a l2 l1 l1 
           & 'a l2 l1 l1 l1 -> 'a l l1 l ]*)
fun f x = C1 x
