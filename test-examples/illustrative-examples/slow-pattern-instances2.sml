
(*[ datasort tt = true and  ff = false ]*)

(*[ val myfun :> (tt -> tt) list -> (tt -> tt) list ]*)
fun myfun (l as []) = l
  | myfun (h::t) = t

