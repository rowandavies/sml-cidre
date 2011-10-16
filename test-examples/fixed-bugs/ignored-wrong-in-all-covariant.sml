
datatype 'a option = NONE | SOME of 'a

(*[ datasort tt = true ]*)
(*[ datasort 'a none = NONE ]*)
(*[ datasort 'a some = SOME of 'a ]*)

(*
(*[ val x :> bool none ]*)
val x = NONE

(*[ val y :> tt none ]*)
val y = x

(*[ val x2 :> bool some ]*)
val x2 = SOME false


(*[ val y2 :> tt some ]*)
val y2 = x
*)