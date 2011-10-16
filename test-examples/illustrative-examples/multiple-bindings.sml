
(*[ datasort tt = true and ff = false ]*)
datatype d = C1 | C2 of bool

(*[ datasort d1 = C2 of tt | C2 of ff
         and d2 = C1 | C2 of tt 
         and d3 = C2 of tt]*)

val y (*[ :> d1 ]*) = C2 true


val (C2 x) = y

(* Even a sort constraint doesn't help.  This is because it is only a bound.  *)
val (C2 (x(*[  :> bool ]*)) ) = y

(* This does work, at the expense of generating a warning about unmatched cases *)
val (C2 x (*[ :> d ]*) ) = y

(* And this won't work, since we specified y :> d1. *)
val (C2 x (*[ :> d3 ]*) ) = y

(* Rewriting like this solves the problem. *)
(*[ val x2 :>  bool ]*)
val x2 = case y of C2 x => x

