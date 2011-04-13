
(* This example illustrates the calculation of the sorts of
   constructors. *)


datatype three = C1 | C2 | C3
datatype mythree = D of three

(*[ datasort s1 = C1 
    and s2 = C2 
    and r12 = D of s1 | D of s2  ]*)

(*[ datasort s12 = C1 | C2
         and r12' = D of s12 ]*)

(*[ datasort s3 = C3
    and r13 = D of s1 | D of s3
    and r23 = D of s2 | D of s3  ]*)


(*[ val f :> s12 -> r12' ]*)
fun f x = D x