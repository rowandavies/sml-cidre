
(*[ datasort tt = true and ff = false ]*)


(*[ val bugfun :> tt -> tt ]*)
fun f y = 
    let
      fun bugfun x = x
    in
	bugfun y
    end
