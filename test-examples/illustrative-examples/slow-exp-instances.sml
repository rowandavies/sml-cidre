(* This example used to show that instantiation could be slow.  Now it
   demonstrates the use of sort annotations to specify instances, and
   the automatic use of the default sort when there is no annotation
   non-covariant sorts are involved. *)

(*[ datasort tt = true and  ff = false ]*)

(*[ val not :> bool -> bool & tt -> ff ]*)
fun not true = false
  | not false = true

fun id x = x

val inot = (id (*[ :> (tt -> ff) -> (tt -> ff) ]*)) not
val inot2 = (id (*[ :> (tt -> ff) -> ((tt&ff) -> bool) ]*)) not
val inot3 = (id) not  (* Uses default sort: no annotation, non-covariant type.  *)
val t = id true       (* Generates full instance, since lattice is "small" (covariant). *)