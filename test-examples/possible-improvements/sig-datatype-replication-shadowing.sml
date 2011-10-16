(* An example where the combination of datatype replication and shadowing leads to
   trouble.  The rule is: if a datasort declaration refines a datatype that has some 
   constructors shadowed (e.g. via open) then the first constructor in the datasort
   declaration must be in a structure containing the datatype with the same name
   as the original datatype declaration.  This should cover almost every case that
   will arise in practice, but is somewhat ad-hoc, and perhaps a rethink of the
   environments for constructors is in order eventually.
   - Rowan 17sep04
 *)

local
  structure A = 
  struct
    structure Shadow = struct val s = 5 end

    datatype nat = z | s of nat

    structure TS =
    struct
      datatype t = datatype nat
       open Shadow
    end

    (*[ datasort ev = TS.z ]*)
(*
    (*[ val f :> nat -> ev ]*) 
    fun f (s x) = s (s (f x))
      | f z = z
*)
  end (*: sig
          structure ThisStructure :
            sig
              datatype t = z | s of t
(*              val s : int *)
            end
          datatype nat = datatype ThisStructure.t
          (*[ datasort ev = z | s of od  and  od = s of ev ]*) 
          (*[ val f :> ev -> ev ]*)
          val f : nat -> nat
        end *)
in
  structure A  = A
end
  