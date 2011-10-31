signature PACK_WORD =
   sig
      val bytesPerElem: int 
      val isBigEndian: bool 
      val subArr: Word8Array.array * int -> LargeWord.word 
      val subArrX: Word8Array.array * int -> LargeWord.word 
      val subVec: Word8Vector.vector * int -> LargeWord.word 
      val subVecX: Word8Vector.vector * int -> LargeWord.word 
      val update: Word8Array.array * int * LargeWord.word -> unit
   end

signature PACK_WORD_EXTRA =
   sig
      include PACK_WORD
      val unsafeSubArr: Word8Array.array * int -> LargeWord.word 
      val unsafeSubArrX: Word8Array.array * int -> LargeWord.word 
      val unsafeSubVec: Word8Vector.vector * int -> LargeWord.word 
      val unsafeSubVecX: Word8Vector.vector * int -> LargeWord.word 
      val unsafeUpdate: Word8Array.array * int * LargeWord.word -> unit
   end



local structure s = 
struct (*[ assumesig sig
  structure PackWord32Big: PACK_WORD_EXTRA
  structure PackWord32Little: PACK_WORD_EXTRA
  structure PackWord32Host: PACK_WORD_EXTRA

  structure PackWord64Big: PACK_WORD_EXTRA
  structure PackWord64Little: PACK_WORD_EXTRA
  structure PackWord64Host: PACK_WORD_EXTRA
end ]*) end

in open s 
end
