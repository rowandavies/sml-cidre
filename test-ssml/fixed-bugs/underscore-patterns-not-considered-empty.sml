(*[ datasort tt = true and ff = false ]*)

(* f is accepted, but g is not.  *)

(*[ val f :> bool -> tt ]*)
fun f x = true
  | f y = false


(*[ val g :> bool -> tt ]*)
fun g x = true
  | g _ = false