
(* This example shows that there may be multiple sorts for a "ref" in
   a pattern. It might be possible to rule some out by checking emptiness of
   the intersection with the pattern. *)

(*[ datasort tt = true and ff = false ]*)

datatype t = C of bool ref

(*[ datasort s = C of tt ref | C of ff ref ]*)


(*[ val f :> s -> tt ]*)
fun f (C (y as ref true)) = ((*y:=false;*) !y)
  | f (C x) = true

