(* This example used to trigger a bug in the way sorts for
   constructors were calculated.  That's now fixed.

   But, it still demonstrates that something more needs to be printed
   for sorts which are equivalent to each other.  Currently only
   information for one representitive is included.  
*)

datatype three = C1 | C2 | C3
     and mythree = D of three

(*[ datasort s1 = C1 and s2 = C2
         and r12 = D of s1 | D of s2 ]*)

(*[ datasort s12 = C1 | C2
         and r12' = D of s12 ]*)
