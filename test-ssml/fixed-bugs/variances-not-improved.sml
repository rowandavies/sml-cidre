

(*[ datasort tt = true ]*)
(*[ datasort 'a none1 = NONE ]*)
(*[ datasort 'a some1 = SOME of 'a ]*)
(*[ datasort '?a none2 = NONE ]*)
(*[ datasort '+a some2 = SOME of '+a ]*)


(*[ val x :> bool none1 ]*)
val x = NONE

(* The following should succeed, if the variance is improved. *)
(*[ val y :> tt none1 ]*)
val y = x

(*[ val x2 :> bool some1 ]*)
val x2 = SOME false

(* The following should fail, regardless. *)
(*[ val y2 :> tt some1 ]*)
(* val y2 = x *)
