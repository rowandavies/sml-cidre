
(* The signature for this example comes out wrong: strangely, (s & s2) & s2 = s2
   in one place in the table, but not the other.

BUG: RefinedEnvironments.conjoinRL s2, s & s2 
   RL = (t ^ s & s2 = s & s2)
        (s ^ s & s2 = s & s2)
        (s ^ s2 = s & s2)
        (t ^ s2 = s2)
        (t ^ s = s)

> functor SigSortSub(type 'a s
                     type 'a s2
                     type 'a t
                     sort 'a t : 'a t
                     sort 'a s <: 'a t
                     sort 'a s2 <: 'a t
                       
                       Table of Intersections:
                               | t       s       s2      s & s2  
                       --------+---------------------------------
                       t       | t       s       s2      s & s2  
                       s       | s       s       s & s2  s & s2  
                       s2      | s2      s & s2  s2      s2      
                       s & s2  | s & s2  s & s2  s & s2  s & s2  
                       
                    ): sig
                         val f : 'a t -> 'a t
                          (*[ :> 'a s2 -> 'a t ]*)
                       end
    
*)

functor SigSortSub(type t 
                   (*[ sortdef s <| t ]*) 
                   (*[ sortdef s2 <| t  ]*) 
                  ) = 
  struct 
    (*[ val f :> s2 -> t ]*) 
    fun f x = x 
  end

(*
structure AppliedSSS = SigSortSub(datatype 'a t = C1 | C2 | C3
                                   (*[ datasort 'a s = C1 | C2
                                            and 'a s2 = C2 ]*)
                                  )
*)