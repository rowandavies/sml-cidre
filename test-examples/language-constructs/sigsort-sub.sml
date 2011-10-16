
(* This example demonstrates the use of upper bounds to declare refinements of
   opaque types.  The current state is that only a single upper bound can be
   given for each sort.  Also, matching a structure against a signature does NOT
   actually check that the bounds are satisfied.
*)

functor SigSortSub(type 'a t 
                   (*[ sortdef 'a s <| t ]*) 
                   (*[ sortdef 'a s2 <| t  ]*) 
                  ) = 
  struct 
    (*[ val f :> 'a s2 -> 'a t ]*) 
    fun f x = x 
  end

(*
structure AppliedSSS = SigSortSub(datatype 'a t = C1 | C2 | C3
                                   (*[ datasort 'a s = C1 | C2
                                            and 'a s2 = C2 ]*)
                                  )
*)