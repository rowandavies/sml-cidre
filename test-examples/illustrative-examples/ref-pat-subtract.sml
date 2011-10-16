
(* This example shows that "ref" in patterns needs special treatment to avoid unsoundness. *)

(*[ datasort tt = true and ff = false ]*)



(*[ val f :> bool ref -> ff ]*)
fun f (y as (ref true)) = (y:=false; (*!y*) false)
  | f (y as (ref false)) = (!y)

