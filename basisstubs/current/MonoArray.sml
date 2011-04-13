(*MONO_ARRAY.sml*)

structure MonoArray:MONO_ARRAY = struct(*[ assumesig MONO_ARRAY ]*)
type array = unit
type elem = unit
  structure Vector : MONO_VECTOR = MonoVector
  val maxLen : int  = 0
  val array : (int * elem) -> array  = fn _ => raise Match
  val fromList : elem list -> array  = fn _ => raise Match
  val tabulate : (int * (int -> elem)) -> array  = fn _ => raise Match
  val length : array -> int  = fn _ => raise Match
  val sub : (array * int) -> elem  = fn _ => raise Match
  val update : (array * int * elem) -> unit  = fn _ => raise Match
  val extract : (array * int * int option) -> Vector.vector  = fn _ => raise Match
  val copy : {src : array, si : int, len : int option, dst : array, di : int} -> unit  = fn _ => raise Match
  val copyVec : {src : Vector.vector, si : int, len : int option, dst : array, di : int} -> unit  = fn _ => raise Match
  val appi : ((int * elem) -> unit) -> (array * int * int option) -> unit  = fn _ => raise Match
  val app : (elem -> unit) -> array -> unit  = fn _ => raise Match
  val foldli : ((int * elem * 'b) -> 'b) -> 'b -> (array * int * int option) -> 'b  = fn _ => raise Match
  val foldri : ((int * elem * 'b) -> 'b) -> 'b -> (array * int * int option) -> 'b  = fn _ => raise Match
  val foldl : ((elem * 'b) -> 'b) -> 'b -> array -> 'b  = fn _ => raise Match
  val foldr : ((elem * 'b) -> 'b) -> 'b -> array -> 'b  = fn _ => raise Match
  val modifyi : ((int * elem) -> elem) -> (array * int * int option) -> unit  = fn _ => raise Match
  val modify : (elem -> elem) -> array -> unit  = fn _ => raise Match
end; (*signature MONO_ARRAY*)

(* Type [array] is the type of one-dimensional, mutable, zero-based
   constant-time-access arrays with elements of type elem.  Arrays a1 and a2
   are equal if both were created by the same call to a primitive (array0,
   array, tabulate, fromList).

   All operations are as for Array.array.
*)
