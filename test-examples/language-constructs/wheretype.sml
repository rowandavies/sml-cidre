signature S = sig  
           type nat

           (*[ val f :> nat -> nat ]*)
           val f : nat -> nat
        end;

  datatype nat' = z | s of nat'
     (*[  datasort ev = z | s of od  and  od = s of ev  ]*)

  structure A = 
  struct

    type nat = nat'

    (*[ val f :> nat -> ev ]*) 
    fun f (s x) = s (s (f x))
      | f z = z
  end :> S  where type nat = nat'

  