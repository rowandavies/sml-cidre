(* The following is defined to be sort incorrect unless we uncomment one of the
   two "split" datasort declarations.  The inversion principle for nat_noerr is:
       nat_noerr = Z  |  S of nat_noerr 
   i.e exactly as in the datasort declaration.

   Adding either datasort declaration allows (via subsumption) the use of a
   stronger inversion principle.

   In general declaring a supersort (or equivalent sort) may result in
   an inversion principle being "improved".  But simply declaring ev and od
   above doesn't improve the inversion principle for nat_noerr.  This allows
   some control over the inversion principles used.
*)


datatype nat_err = Z | S of nat_err | err | err2;

(*[ datasort nat_noerr = Z | S of nat_noerr ]*)

(*[ datasort ev = Z | S of od and od = S of ev ]*)

(* (*[ datasort nat_noerr_split = Z | S of od | S of ev ]*) *)
(* (*[ datasort nat_noerr_split2 = Z | S of od | S of ev | err2 ]*) *)

(*[ val f :> ev -> unit & od -> unit  ]*)
fun f x = ()

(*[ val g :> nat_noerr -> unit ]*)
fun g Z = ()
  | g y = f y  (* The stronger inversion principle should be used here. *) 
               (* Declaratively: we can use subsumption to the sort "nat_noerr_split"
                  and make use of its inversion principle. *)

