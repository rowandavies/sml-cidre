(* A simple example to demonstrate that the checker handles datatype replication. Rowan 14aug02 *)

local
  structure A = 
  struct
    datatype nat = z | s of nat
    (*[ datasort ev = z | s of od  and  od = s of ev ]*)

    datatype t = datatype nat

    (*[ val f :> nat -> ev ]*) 
    fun f (s x) = s (s (f x))
      | f z = z
  end : sig
          datatype t = z | s of t
          (*[ datasort ev = z | s of od  and  od = s of ev ]*) 
          (*[ val f :> ev -> ev ]*)
          val f : t -> t
        end
in
  val x = A.f
end
  