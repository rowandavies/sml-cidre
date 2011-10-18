(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PRIM_IO_ARG =
   sig
      structure Vector: MONO_VECTOR
      structure VectorSlice: MONO_VECTOR_SLICE
      structure Array: MONO_ARRAY
      structure ArraySlice: MONO_ARRAY_SLICE
      sharing type Vector.elem = VectorSlice.elem 
         = Array.elem = ArraySlice.elem 
      sharing type Vector.vector = VectorSlice.vector 
         = Array.vector = ArraySlice.vector 
      sharing type VectorSlice.slice = ArraySlice.vector_slice
      sharing type Array.array = ArraySlice.array

      val someElem: Vector.elem

      eqtype pos
      val compare: pos * pos -> order
   end

functor PrimIO (S: PRIM_IO_ARG): PRIM_IO = 
   struct (*[ assumesig PRIM_IO ]*) end
