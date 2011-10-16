
 datatype t = A of int | B of t
            | C | D of t

 (*[ datasort t1 = A of int | B of t1
     datasort t2 = C | D of t2 ]*)

 (*[ val accept_both :  t1 -> int
                      & t2 -> int ]*)
 fun accept_both (A n) = n
   | accept_both (B x) = 0
   | accept_both C = 0
   | accept_both (D y) = 0

   (*[  val f :> (t1 -> t2) & (t2 -> t1)  ]*)
   fun f (A n) = C
     | f (B x) = D (f x)
     | f C = A 0
     | f (D y) = B (f y)
