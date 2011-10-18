(* Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature IMPERATIVE_IO_ARG =
   sig
      structure Array: MONO_ARRAY
(*      structure ArraySlice: MONO_ARRAY_SLICE *)
      structure StreamIO: STREAM_IO
      structure Vector: MONO_VECTOR
(*      structure VectorSlice: MONO_VECTOR_SLICE *)
(*      sharing type Array.array = ArraySlice.array *)
      sharing type
         Array.elem
(*       = ArraySlice.elem *)
         = StreamIO.elem
         = Vector.elem
(*       = VectorSlice.elem *)
      sharing type
         Array.vector
(*       = ArraySlice.vector *)
         = Vector.vector
(*       = VectorSlice.vector *)
(*      sharing type ArraySlice.vector_slice = VectorSlice.slice *)
   end

functor ImperativeIO (S: IMPERATIVE_IO_ARG) = struct (*[ assumesig IMPERATIVE_IO ]*) end
