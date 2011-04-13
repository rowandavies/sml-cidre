(**  An example showing that the refinement checker handles datasort
     specifications in signatures correctly.  See also
     datasort-spec-error.sml.    - Rowan 12sep03
**)

local
  structure A = 
  struct
    datatype t = c
    datatype nat = z | s of nat
    (*[ datasort ev = z | s of od  
             and od = s of ev     ]*)

    (*[ val f :> nat -> ev ]*) 
    fun f (s x) = s (s (f x))
      | f z = z
  end : sig
          datatype t = c  
          datatype nat = z | s of nat
          (*[ datasort ev = z | s of od  
                   and od = s of ev      ]*)
          (*[ val f : ev -> ev   ]*)
          val f : nat -> nat
        end

in
  val x = A.f
end
  