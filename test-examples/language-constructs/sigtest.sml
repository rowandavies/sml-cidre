(* Small test of signatures in Kit3 *)

structure S : sig val f : 'a -> 'a end =
struct
  (*[ val f :> 'a -> 'b ]*)
  fun f x = f x
end 
