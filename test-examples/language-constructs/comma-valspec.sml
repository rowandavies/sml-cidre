
(*[ datasort tt = true and ff = false ]*)

(*[ val g :> bool -> bool & tt -> tt ]*)
fun g y = 
    let
	val f = (fn x => y (*[ :> bool -> bool , tt -> tt ]*) )
    in
	f y
    end

(*[ val gg :> bool -> bool & tt -> tt ]*)
fun gg y = 
    let
        (*[ val f :> bool -> bool , tt -> tt ]*)
	fun f x = y
    in
	f y
    end
