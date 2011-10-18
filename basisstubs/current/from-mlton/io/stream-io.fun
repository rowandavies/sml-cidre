(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature STREAM_IO_ARG = 
   sig 
      structure Array: MONO_ARRAY
      structure ArraySlice: MONO_ARRAY_SLICE
      structure PrimIO: PRIM_IO  
      structure Vector: MONO_VECTOR
      structure VectorSlice: MONO_VECTOR_SLICE
      sharing type PrimIO.elem = Vector.elem = VectorSlice.elem = Array.elem
         = ArraySlice.elem 
      sharing type PrimIO.vector = Vector.vector = VectorSlice.vector
         = Array.vector = ArraySlice.vector 
      sharing type PrimIO.vector_slice = VectorSlice.slice
         = ArraySlice.vector_slice
      sharing type PrimIO.array = Array.array = ArraySlice.array 
      sharing type PrimIO.array_slice = ArraySlice.slice

      val someElem: PrimIO.elem
   end

functor StreamIO (S: STREAM_IO_ARG): STREAM_IO = struct (*[ assumesig STREAM_IO ]*) end
