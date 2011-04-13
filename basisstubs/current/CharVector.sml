
structure Word8Vector :> MONO_VECTOR
  where type elem = Word8.word  = 
struct (*[ assumesig MONO_VECTOR
             where type elem = Word8.word ]*)
end

structure CharVector :> MONO_VECTOR
  where type vector = String.string
  where type elem = char    = 
struct (*[ assumesig MONO_VECTOR
              where type vector = String.string
              where type elem = char   ]*)
end


(*
structure WideCharVector :> MONO_VECTOR  (* OPTIONAL *)
  where type vector = WideString.string
  where type elem = WideChar.char
structure BoolVector :> MONO_VECTOR  (* OPTIONAL *)
  where type elem = bool
structure IntVector :> MONO_VECTOR  (* OPTIONAL *)
  where type elem = int
structure WordVector :> MONO_VECTOR  (* OPTIONAL *)
  where type elem = word
structure RealVector :> MONO_VECTOR  (* OPTIONAL *)
  where type elem = real
structure LargeIntVector :> MONO_VECTOR  (* OPTIONAL *)
  where type elem = LargeInt.int
structure LargeWordVector :> MONO_VECTOR  (* OPTIONAL *)
  where type elem = LargeWord.word
structure LargeRealVector :> MONO_VECTOR  (* OPTIONAL *)
  where type elem = LargeReal.real
structure Int<N>Vector :> MONO_VECTOR  (* OPTIONAL *)
  where type elem = Int{N}.int
structure Word<N>Vector :> MONO_VECTOR  (* OPTIONAL *)
  where type elem = Word{N}.word
structure Real<N>Vector :> MONO_VECTOR  (* OPTIONAL *)
  where type elem = Real{N}.real
*)