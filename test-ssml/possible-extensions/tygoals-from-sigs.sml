
structure TGNR :> 
    sig 
	type t
        (*[ sortdef s <| t ]*)

        (*[ val f :> s -> s ]*)
        val f : t -> t
    end
= struct
    type t = int option
    (*[ sortdef s = int option ]*)

    fun f (SOME x) = x
       
  end
  