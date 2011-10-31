(*MONO_VECTOR.sml*)

signature MONO_VECTOR = sig
(*
  eqtype vector
  eqtype elem
  val maxLen : int 
  val fromList : elem list -> vector 
  val tabulate : (int * (int -> elem)) -> vector 
  val length : vector -> int 
  val sub : (vector * int) -> elem 
  val extract : (vector * int * int option) -> vector 
  val concat : vector list -> vector 
  val mapi : ((int * elem) -> elem) -> (vector * int * int option) -> vector 
  val map : (elem -> elem) -> vector -> vector 
  val appi : ((int * elem) -> unit) -> (vector * int * int option) -> unit 
  val app : (elem -> unit) -> vector -> unit
  val foldli : ((int * elem * 'a) -> 'a) -> 'a -> (vector * int * int option) -> 'a 
  val foldri : ((int * elem * 'a) -> 'a) -> 'a -> (vector * int * int option) -> 'a 
  val foldl : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a 
  val foldr : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a
*)
type vector
type elem
val maxLen : int
val fromList : elem list -> vector
val tabulate : int * (int -> elem) -> vector
val length : vector -> int
val sub : vector * int -> elem
val update : vector * int * elem -> vector
val concat : vector list -> vector
val appi : (int * elem -> unit) -> vector -> unit
val app  : (elem -> unit) -> vector -> unit
val mapi : (int * elem -> elem) -> vector -> vector
val map  : (elem -> elem) -> vector -> vector
val foldli : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
val foldri : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
val foldl  : (elem * 'a -> 'a) -> 'a -> vector -> 'a
val foldr  : (elem * 'a -> 'a) -> 'a -> vector -> 'a
val findi : (int * elem -> bool)
              -> vector -> (int * elem) option
val find  : (elem -> bool) -> vector -> elem option
val exists : (elem -> bool) -> vector -> bool
val all : (elem -> bool) -> vector -> bool
val collate : (elem * elem -> order)
                -> vector * vector -> order
end; (*signature MONO_VECTOR*)



structure Word8Vector :> MONO_VECTOR
  where type elem = Word8.word  = 
struct (*[ assumesig MONO_VECTOR
             where type elem = Word8.word ]*)
end

structure CharVector :> MONO_VECTOR
  where type vector = string
  where type elem = char    = 
struct (*[ assumesig MONO_VECTOR
              where type vector = string
              where type elem = char   ]*)
end


structure WideCharVector = struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type vector = WideString.string
  where type elem = WideChar.char  ]*)
end
structure BoolVector = struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = bool  ]*)
end
structure IntVector = struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = int  ]*)
end
structure WordVector = struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = word  ]*)
end
structure RealVector = struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = real  ]*)
end
structure LargeIntVector = struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = LargeInt.int  ]*)
end
structure LargeWordVector = struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = LargeWord.word  ]*)
end
structure LargeRealVector = struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = LargeReal.real  ]*)
end
(*
structure Int<N>Vector = struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = Int{N}.int  ]*)
end
structure Word<N>Vector = struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = Word{N}.word  ]*)
end
structure Real<N>Vector = struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = Real{N}.real  ]*)
end
*)

(* Type [vector] is the type of one-dimensional, immutable, zero-based
   constant-time-access vectors with elements of type elem.  Type vector
   admits equality, and vectors v1 and v2 are equal if they have the same
   length and their elements are equal.

   All operations are as for Vector.vector.
*)
