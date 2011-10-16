
(* This example shows that arrow free datasorts are treated
   differently from those with covariant parameters and arrows.
   This is in contrast to Freeman's design, which implicitly builds
   a form of distributivity into the rule for covariant subsorting.

   Date: 4 Feb 04   Author: Rowan Davies
*)


(*[ datasort tt = true and ff = false; ]*)


(* This first part is rejected because of the presence of the arrows.  *)
(*
datatype 'a t = C of bool -> 'a
(*[ datasort 'a r = C of tt -> 'a ]*)

(* This is the coercion that would be unsound. *)
(*[ val g :> (tt ref r & bool ref r) -> (tt ref & bool ref) r  ]*)
fun g x = x

(* This is the opposite of the above coercion.  It is rejected too.  *)
(*[ val h :> (tt ref & bool ref) r -> (tt ref r & bool ref r)  ]*)
fun h x = x


(*[ val f :> tt -> tt ref &  tt -> bool ref ]*)
fun f x = ref x

val (x (*[ :> tt ref r & bool ref r ]*) ) = C f  (* This part doesn't yet show what I intended because *)
                                                 (* C f isn't considered a value by the checker. *)

*)

(* The coercions above aren't accepted.  Now lets see if this is due
   to the presence of the arrow.  Indeed it is: the following is
   accepted.  There is a form of distributivity, but it only
   applies when there are no functions at all in the declared
   datasorts.  *)
datatype 'a t = C of 'a
(*[ datasort 'a r = C of 'a ]*)


(* This is a coercion that is sound. *)
(*[ val g :> (tt ref r & bool ref r) -> (tt ref & bool ref) r  ]*)
fun g x = x

(* This is the opposite of the above coercion. *)
(*[ val g :> (tt ref & bool ref) r -> (tt ref r & bool ref r)  ]*)
fun g x = x


