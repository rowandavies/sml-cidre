
local
  structure A = 
  struct
    datatype nat = z | s of nat
    (*[ datasort ev = z | s of od  and  od = s of ev ]*)

    (*[ val f :> ev -> ev & od -> ev ]*) 
    fun f (s x) = s (s (f x))
      | f z = z
  end : sig  
          type nat  
          (*[ sortdef ev < nat and od < nat ]*)
          (*[ val f : ev -> od ]*)
          val f : nat -> nat
        end

in
  val x = A.f
end
  