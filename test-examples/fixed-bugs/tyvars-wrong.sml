
signature Map = 
sig
    eqtype ('a, 'b) map
    val composemap: ('b -> 'c) -> ('a, 'b) map -> ('a, 'c) map
end

structure Map :> Map = 
struct
  type ('a, 'b) map = ('a * 'b) list
  fun composemap x y = raise (Fail "Sorry")
end

(* (*[ val y :> ('b -> 'c) -> ('a, 'b) Map.map -> ('a, 'c) Map.map ]*) *)
val y = Map.composemap