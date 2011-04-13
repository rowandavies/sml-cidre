
(* This example shows that "ref" in patterns needs special treatment to avoid unsoundness. *)

(*[ datasort tt = true and ff = false ]*)



(*[ val f :> bool ref -> tt ]*)
fun f (y as (ref true)) = (y:=false; !y)
  | f (ref false) = true

