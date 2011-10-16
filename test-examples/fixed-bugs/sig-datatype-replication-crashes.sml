(* A simple example to demonstrate that the checker handles datatype
   replication in signatures. Rowan 17sep04 *)

local
  structure A = 
  struct
    datatype nat = z | s of nat
    (*[ datasort ev = z | s of od  and  od = s of ev ]*)

    structure T =
    struct
      datatype t = datatype nat
    end

    (*[ val f :> nat -> ev ]*) 
    fun f (s x) = s (s (f x))
      | f z = z
  end : sig
          datatype nat = z | s of nat
          structure T :
            sig
                datatype t = datatype nat
            end
          (*[ datasort ev = z | T.s of od  and  od = s of ev ]*) 
          (*[ val f :> ev -> ev ]*)
          val f : T.t -> T.t
        end
in
  structure A = A
  val x = A.f
end
  