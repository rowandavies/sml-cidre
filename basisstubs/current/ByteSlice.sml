(* CharVectorSlice/Word8VectorSlice/CharArraySlice/Word8ArraySlice 
 * - code partly from Moscow ML ; mael 2005-11-27 *)
functor ByteSlice (type table
		   type elem
		   type vector
		   type array
		   val maxLen : int) =
struct

    (*[ assumesig MONO_ARRAY_SLICE where type elem=elem and type array=array ]*)

(*
    type vector = vector
    type elem = elem
    type array = array

    [snip]
*)
end

functor ByteSliceV (type table
		   type elem
		   type vector
		   type array
		   val maxLen : int) =
struct

    (*[ assumesig MONO_VECTOR_SLICE where type vector=vector and type elem=elem ]*)

(*
    type vector = vector
    type elem = elem
    type array = array

    [snip]
*)
end

structure CharVectorSlice : MONO_VECTOR_SLICE = 
    ByteSliceV(type table  = CharVector.vector
	      type array  = CharArray.array
	      type vector = CharVector.vector
	      type elem   = char
	      val maxLen  = CharVector.maxLen)

structure Word8VectorSlice : MONO_VECTOR_SLICE = 
    ByteSliceV(type table  = Word8Vector.vector
	      type array  = Word8Array.array
	      type vector = Word8Vector.vector
	      type elem   = word8
	      val maxLen  = Word8Vector.maxLen)

structure CharArraySlice : MONO_ARRAY_SLICE = 
    ByteSlice(type table  = CharArray.array
	      type array  = CharArray.array
	      type vector = CharVector.vector
	      type elem   = char
	      val maxLen  = CharArray.maxLen)

structure Word8ArraySlice : MONO_ARRAY_SLICE = 
    ByteSlice(type table  = Word8Array.array
	      type array  = Word8Array.array
	      type vector = Word8Vector.vector
	      type elem   = word8
	      val maxLen  = Word8Array.maxLen)
