(*MONO_VECTOR.sml*)

structure MonoVector:MONO_VECTOR = struct(*[ assumesig MONO_VECTOR ]*)
type vector = unit
type elem = unit
  val maxLen : int  = 0
  val fromList : elem list -> vector  = fn _ => raise Match
  val tabulate : (int * (int -> elem)) -> vector  = fn _ => raise Match
  val length : vector -> int  = fn _ => raise Match
  val sub : (vector * int) -> elem  = fn _ => raise Match
  val extract : (vector * int * int option) -> vector  = fn _ => raise Match
  val concat : vector list -> vector  = fn _ => raise Match
  val mapi : ((int * elem) -> elem) -> (vector * int * int option) -> vector  = fn _ => raise Match
  val map : (elem -> elem) -> vector -> vector  = fn _ => raise Match
  val appi : ((int * elem) -> unit) -> (vector * int * int option) -> unit  = fn _ => raise Match
  val app : (elem -> unit) -> vector -> unit = fn _ => raise Match
  val foldli : ((int * elem * 'a) -> 'a) -> 'a -> (vector * int * int option) -> 'a  = fn _ => raise Match
  val foldri : ((int * elem * 'a) -> 'a) -> 'a -> (vector * int * int option) -> 'a  = fn _ => raise Match
  val foldl : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a  = fn _ => raise Match
  val foldr : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a = fn _ => raise Match
end; (*signature MONO_VECTOR*)

(* Type [vector] is the type of one-dimensional, immutable, zero-based
   constant-time-access vectors with elements of type elem.  Type vector
   admits equality, and vectors v1 and v2 are equal if they have the same
   length and their elements are equal.

   All operations are as for Vector.vector.
*)

