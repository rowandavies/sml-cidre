(*MONO_ARRAY.sml*)

signature MONO_ARRAY = sig

eqtype array
type elem
type vector
val maxLen : int
val array : int * elem -> array
val fromList : elem list -> array
val tabulate : int * (int -> elem) -> array
val length : array -> int
val sub : array * int -> elem
val update : array * int * elem -> unit
val vector : array -> vector
val copy    : {src : array, dst : array, di : int} -> unit
val copyVec : {src : vector, dst : array, di : int} -> unit

val appi : (int * elem -> unit) -> array -> unit
val app  : (elem -> unit) -> array -> unit
val modifyi : (int * elem -> elem) -> array -> unit
val modify  : (elem -> elem) -> array -> unit
val foldli : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
val foldri : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
val foldl  : (elem * 'b -> 'b) -> 'b -> array -> 'b
val foldr  : (elem * 'b -> 'b) -> 'b -> array -> 'b
val findi : (int * elem -> bool)
              -> array -> (int * elem) option
val find  : (elem -> bool) -> array -> elem option
val exists : (elem -> bool) -> array -> bool
val all : (elem -> bool) -> array -> bool
val collate : (elem * elem -> order)
                -> array * array -> order
(*
  eqtype array
  eqtype elem
  structure Vector : MONO_VECTOR
  val maxLen : int 
  val array : (int * elem) -> array 
  val fromList : elem list -> array 
  val tabulate : (int * (int -> elem)) -> array 
  val length : array -> int 
  val sub : (array * int) -> elem 
  val update : (array * int * elem) -> unit 
  val extract : (array * int * int option) -> Vector.vector 
  val copy : {src : array, si : int, len : int option, dst : array, di : int} -> unit 
  val copyVec : {src : Vector.vector, si : int, len : int option, dst : array, di : int} -> unit 
  val appi : ((int * elem) -> unit) -> (array * int * int option) -> unit 
  val app : (elem -> unit) -> array -> unit 
  val foldli : ((int * elem * 'b) -> 'b) -> 'b -> (array * int * int option) -> 'b 
  val foldri : ((int * elem * 'b) -> 'b) -> 'b -> (array * int * int option) -> 'b 
  val foldl : ((elem * 'b) -> 'b) -> 'b -> array -> 'b 
  val foldr : ((elem * 'b) -> 'b) -> 'b -> array -> 'b 
  val modifyi : ((int * elem) -> elem) -> (array * int * int option) -> unit 
  val modify : (elem -> elem) -> array -> unit 
*)
end; (*signature MONO_ARRAY*)

(* Type [array] is the type of one-dimensional, mutable, zero-based
   constant-time-access arrays with elements of type elem.  Arrays a1 and a2
   are equal if both were created by the same call to a primitive (array0,
   array, tabulate, fromList).

   All operations are as for Array.array.
*)


structure Word8Array = 
  struct 
    (*[ assumesig MONO_ARRAY 
          where type vector = Word8Vector.vector
          where type elem = Word8.word ]*)
  end
  
structure CharArray =
  struct 
    (*[ assumesig MONO_ARRAY 
          where type vector = CharVector.vector
          where type elem = char ]*)
  end


(*
structure WideCharArray :> MONO_ARRAY  (* OPTIONAL *)
  where type vector = WideCharVector.vector
  where type elem = WideChar.char
structure BoolArray :> MONO_ARRAY  (* OPTIONAL *)
  where type vector = BoolVector.vector
  where type elem = bool
structure IntArray :> MONO_ARRAY  (* OPTIONAL *)
  where type vector = IntVector.vector
  where type elem = int
structure WordArray :> MONO_ARRAY  (* OPTIONAL *)
  where type vector = WordVector.vector
  where type elem = word
structure RealArray :> MONO_ARRAY  (* OPTIONAL *)
  where type vector = RealVector.vector
  where type elem = real
structure LargeIntArray :> MONO_ARRAY  (* OPTIONAL *)
  where type vector = LargeIntVector.vector
  where type elem = LargeInt.int
structure LargeWordArray :> MONO_ARRAY  (* OPTIONAL *)
  where type vector = LargeWordVector.vector
  where type elem = LargeWord.word
structure LargeRealArray :> MONO_ARRAY  (* OPTIONAL *)
  where type vector = LargeRealVector.vector
  where type elem = LargeReal.real
structure Int<N>Array :> MONO_ARRAY  (* OPTIONAL *)
  where type vector = Int{N}Vector.vector
  where type elem = Int{N}.int
structure Word<N>Array :> MONO_ARRAY  (* OPTIONAL *)
  where type vector = Word{N}Vector.vector
  where type elem = Word{N}.word
structure Real<N>Array :> MONO_ARRAY  (* OPTIONAL *)
  where type vector = Real{N}Vector.vector
  where type elem = Real{N}.real
*)
