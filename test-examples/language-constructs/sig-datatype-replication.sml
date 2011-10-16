(* A simple example to demonstrate that the checker handles datatype
   replication in signatures. Rowan 17sep04 *)

(* local *)
  structure A = 
  struct

    structure T =
    struct
       datatype nat = z | s of nat
       (*[ datasort ev = z | s of od  and  od = s of ev ]*)
    end

    datatype t = datatype T.nat
      (*[ datasort ev = z | s of od  and  od = s of ev ]*)
(*    (*[ sortdef ev = T.ev and od = T.od ]*) *)
  
    (*[ val f :> t -> ev ]*) 
    fun f (s x) = s (s (f x))
      | f z = z
  end (* : sig
          structure T :
            sig
              datatype nat = z | s of nat
             (*[ datasort ev = z | s of od  and  od = s of ev ]*) 
            end
          datatype t = datatype T.nat
          (*[ datasort ev = z | s of od  and  od = s of ev ]*) 
          (*[ val f :> ev -> ev ]*)
          val f : t -> t
        end
in
  val x = A.f
end
  *)