signature PACK_REAL =
   sig
      type real

      val bytesPerElem: int 
      val isBigEndian: bool 
      val toBytes: real -> Word8Vector.vector 
      val fromBytes: Word8Vector.vector -> real 
      val subVec: Word8Vector.vector * int -> real 
      val subArr: Word8Array.array * int -> real 
      val update: Word8Array.array * int * real -> unit
   end


local structure s = struct 
(*[ assumesig sig
structure PackRealBig : PACK_REAL  (* OPTIONAL *)
  where type real = Real.real
structure PackRealLittle : PACK_REAL  (* OPTIONAL *)
  where type real = Real.real

structure PackReal32Big : PACK_REAL  (* OPTIONAL *)
  where type real = Real32.real
structure PackReal32Little : PACK_REAL  (* OPTIONAL *)
  where type real = Real32.real

structure PackReal64Big : PACK_REAL  (* OPTIONAL *)
  where type real = Real64.real
structure PackReal64Little : PACK_REAL  (* OPTIONAL *)
  where type real = Real64.real

(* MLton has more, but it looks like SML/NJ may not define any PackReal* at all ?? *)

end ]*)
end

in  open s
end
