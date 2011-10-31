
structure Word8Vector :> MONO_VECTOR
  where type elem = Word8.word  = 
struct (*[ assumesig MONO_VECTOR
             where type elem = Word8.word ]*)
end

structure CharVector :> MONO_VECTOR
  where type vector = String.string
  where type elem = char            = 
struct (*[ assumesig MONO_VECTOR
              where type vector = String.string
              where type elem = char   ]*)
end


structure WideCharVector   = 
struct (*[ assumesig MONO_VECTOR  (* OPTIONAL *)
  where type vector = WideString.string
  where type elem = WideChar.char ]*) end

structure BoolVector   = 
struct (*[ assumesig MONO_VECTOR  (* OPTIONAL *)
  where type elem = bool ]*) end
structure IntVector   = 
struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = int ]*) end
structure WordVector   = 
struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = word ]*) end
structure RealVector   = 
struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = real ]*) end
structure LargeIntVector   = 
struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = LargeInt.int ]*) end
structure LargeWordVector   = 
struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = LargeWord.word ]*) end
structure LargeRealVector   = 
struct (*[ assumesig  MONO_VECTOR  (* OPTIONAL *)
  where type elem = LargeReal.real ]*) end


(*
structure Int<N>Vector :> MONO_VECTOR  (* OPTIONAL *)
  where type elem = Int{N}.int
structure Word<N>Vector :> MONO_VECTOR  (* OPTIONAL *)
  where type elem = Word{N}.word


structure Real<N>Vector :> MONO_VECTOR  (* OPTIONAL *)
  where type elem = Real{N}.real
*)
