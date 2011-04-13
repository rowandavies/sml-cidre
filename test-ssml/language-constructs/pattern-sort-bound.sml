
(*[ datasort tt = true and ff = false ]*)

(*[ val f :> ff -> unit, tt -> unit ]*)
fun f (x (*[ :> ff, ff&tt ]*) ) = ()