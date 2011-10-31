signature MONO_ARRAY_SLICE =
  sig
    type elem
    type array
    type slice
    type vector
    type vector_slice
    val length   : slice -> int
    val sub      : slice * int -> elem
    val update   : slice * int * elem -> unit
    val full     : array -> slice
    val slice    : array * int * int option -> slice
    val subslice : slice * int * int option -> slice
    val base     : slice -> array * int * int
    val vector   : slice -> vector
    val copy     : {src : slice, dst : array, di : int} -> unit
    val copyVec  : {src : vector_slice, dst : array, di : int} -> unit
    val isEmpty  : slice -> bool
    val getItem  : slice -> (elem * slice) option
    val appi     : (int * elem -> unit) -> slice -> unit
    val app      : (elem -> unit) -> slice -> unit
    val modifyi  : (int * elem -> elem) -> slice -> unit
    val modify   : (elem -> elem) -> slice -> unit
    val foldli   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
    val foldr    : (elem * 'b -> 'b) -> 'b -> slice -> 'b
    val foldl    : (elem * 'b -> 'b) -> 'b -> slice -> 'b
    val foldri   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
    val findi    : (int * elem -> bool) -> slice -> (int * elem) option
    val find     : (elem -> bool) -> slice -> elem option
    val exists   : (elem -> bool) -> slice -> bool
    val all      : (elem -> bool) -> slice -> bool
    val collate  : (elem * elem -> order) -> slice * slice -> order 
  end


structure Word8ArraySlice = struct (*[ assumesig  MONO_ARRAY_SLICE
  where type vector = Word8Vector.vector
  where type vector_slice = Word8VectorSlice.slice
  where type array = Word8Array.array
  where type elem = Word8.word
]*) end
structure CharArraySlice = struct (*[ assumesig  MONO_ARRAY_SLICE
  where type vector = CharVector.vector
  where type vector_slice = CharVectorSlice.slice
  where type array = CharArray.array
  where type elem = char
]*) end
structure WideCharArraySlice = struct (*[ assumesig  MONO_ARRAY_SLICE  (* OPTIONAL *)
  where type vector = WideCharVector.vector
  where type vector_slice = WideCharVectorSlice.slice
  where type array = WideCharArray.array
  where type elem = WideChar.char
]*) end
structure BoolArraySlice = struct (*[ assumesig  MONO_ARRAY_SLICE  (* OPTIONAL *)
  where type vector = BoolVector.vector
  where type vector_slice = BoolVectorSlice.slice
  where type array = BoolArray.array
  where type elem = bool
]*) end
structure IntArraySlice = struct (*[ assumesig  MONO_ARRAY_SLICE  (* OPTIONAL *)
  where type vector = IntVector.vector
  where type vector_slice = IntVectorSlice.slice
  where type array = IntArray.array
  where type elem = int
]*) end
structure WordArraySlice = struct (*[ assumesig  MONO_ARRAY_SLICE  (* OPTIONAL *)
  where type vector = WordVector.vector
  where type vector_slice = WordVectorSlice.slice
  where type array = WordArray.array
  where type elem = word
]*) end
structure RealArraySlice = struct (*[ assumesig  MONO_ARRAY_SLICE  (* OPTIONAL *)
  where type vector = RealVector.vector
  where type vector_slice = RealVectorSlice.slice
  where type array = RealArray.array
  where type elem = real
]*) end
structure LargeIntArraySlice = struct (*[ assumesig  MONO_ARRAY_SLICE  (* OPTIONAL *)
  where type vector = LargeIntVector.vector
  where type vector_slice = LargeIntVectorSlice.slice
  where type array = LargeIntArray.array
  where type elem = LargeInt.int
]*) end
structure LargeWordArraySlice = struct (*[ assumesig  MONO_ARRAY_SLICE  (* OPTIONAL *)
  where type vector = LargeWordVector.vector
  where type vector_slice = LargeWordVectorSlice.slice
  where type array = LargeWordArray.array
  where type elem = LargeWord.word
]*) end
structure LargeRealArraySlice = struct (*[ assumesig  MONO_ARRAY_SLICE  (* OPTIONAL *)
  where type vector = LargeRealVector.vector
  where type vector_slice = LargeRealVectorSlice.slice
  where type array = LargeRealArray.array
  where type elem = LargeReal.real
]*) end

(*
structure Int<N>ArraySlice = struct (*[ assumesig  MONO_ARRAY_SLICE  (* OPTIONAL *)
  where type vector = Int{N}Vector.vector
  where type vector_slice = Int{N}VectorSlice.slice
  where type array = Int{N}Array.array
  where type elem = Int{N}.int
]*) end
structure Word<N>ArraySlice = struct (*[ assumesig  MONO_ARRAY_SLICE  (* OPTIONAL *)
  where type vector = Word{N}Vector.vector
  where type vector_slice = Word{N}VectorSlice.slice
  where type array = Word{N}Array.array
  where type elem = Word{N}.word
]*) end
structure Real<N>ArraySlice = struct (*[ assumesig  MONO_ARRAY_SLICE  (* OPTIONAL *)
  where type vector = Real{N}Vector.vector
  where type vector_slice = Real{N}VectorSlice.slice
  where type array = Real{N}Array.array
  where type elem = Real{N}.real
]*) end
*)


(*
Description

type array

    The underlying monomorphic array type. We denote the length of an
    array arr of type array by |arr|.

type vector

    The underlying monomorphic vector type. We denote the length of a
    vector vec of type vector by |vec|.

type vector_slice

    Slices of the monomorphic vector type.

length sl

    returns |sl|, the length (i.e., number of elements) of the slice.

sub (sl, i)

    returns the i(th) element of the slice sl. If i < 0 or |sl| <= i,
    then the Subscript exception is raised.

update (sl, i, a)

    sets the i(th) element of the slice sl to a. If i < 0 or |sl| <=
    i, then the Subscript exception is raised.

full arr

    creates a slice representing the entire array arr. It is
    equivalent to slice(arr, 0, NONE).

slice (arr, i, sz)

    creates a slice based on the array arr starting at index i of the
    array arr. If sz is NONE, the slice includes all of the elements
    to the end of the array, i.e., arr[i..|arr|-1]. This raises
    Subscript if i < 0 or |arr| < i. If sz is SOME(j), the slice has
    length j, that is, it corresponds to arr[i..i+j-1]. It raises
    Subscript if i < 0 or j < 0 or |arr| < i + j. Note that, if
    defined, slice returns an empty slice when i = |arr|.

subslice (sl, i, sz)

    creates a slice based on the given slice sl starting at index i of
    sl. If sz is NONE, the slice includes all of the elements to the
    end of the slice, i.e., sl[i..|sl|-1]. This raises Subscript if i
    < 0 or |sl| < i. If sz is SOME(j), the slice has length j, that
    is, it corresponds to sl[i..i+j-1]. It raises Subscript if i < 0
    or j < 0 or |sl| < i + j. Note that, if defined, slice returns an
    empty slice when i = |sl|.

base sl

    returns a triple (arr, i, n) representing the concrete
    representation of the slice. arr is the underlying array, i is the
    starting index, and n is the length of the slice.

vector sl

    generates a vector from the slice sl. Specifically, if vec is the
    resulting vector, we have |vec| = length sl and, for 0 <= i <
    length sl, element i of vec is sub (sl, i).

copy {src, dst, di}
copyVec {src, dst, di}

    These functions copy the given slice into the array dst, with
    element sub (src,i), for 0 <= i < |src|, being copied to position
    di + i in the destination array. If di < 0 or if |dst| < di+|src|,
    then the Subscript exception is raised.

        Implementation note:

        The copy function must correctly handle the case in which dst
        and the base array of src are equal, and the source and
        destination slices overlap.


isEmpty sl

    returns true if sl has length 0.

getItem sl

    returns the first item in sl and the rest of the slice, or NONE if
    sl is empty.

appi f sl
app f sl

    These apply the function f to the elements of a slice in left to
    right order (i.e., increasing indices). The more general appi
    function supplies f with the index of the corresponding element in
    the slice. The expression app f sl is equivalent to appi (f o #2)
    sl.

modifyi f sl
modify f sl

    These apply the function f to the elements of an array slice in
    left to right order (i.e., increasing indices), and replace each
    element with the result. The more general modifyi supplies f with
    the index of the corresponding element in the slice. The
    expression modify f sl is equivalent to modifyi (f o #2) sl.

foldli f init sl
foldr f init sl
foldl f init sl
foldri f init sl

    These fold the function f over all the elements of an array slice,
    using the value init as the initial value. The functions foldli
    and foldl apply the function f from left to right (increasing
    indices), while the functions foldri and foldr work from right to
    left (decreasing indices). The more general functions foldli and
    foldri supply f with the index of the corresponding element in the
    slice.

    Refer to the MONO_ARRAY manual pages for reference implementations
    of the indexed versions.

    The expression foldl f init sl is equivalent to:

foldli (fn (_, a, x) => f(a, x)) init sl

    The analogous equivalence holds for foldri and foldr.

findi f sl
find f sl

    These apply f to each element of the slice sl, from left to right
    (i.e., increasing indices), until a true value is returned. If
    this occurs, the functions return the element; otherwise, they
    return NONE. The more general version findi also supplies f with
    the index of the element in the slice and, upon finding an entry
    satisfying the predicate, returns that index with the element.

exists f sl

    applies f to each element x of the slice sl, from left to right
    (i.e., increasing indices), until f x evaluates to true; it
    returns true if such an x exists and false otherwise.

all f sl

    applies f to each element x of the slice sl, from left to right
    (i.e., increasing indices), until f x evaluates to false; it
    returns false if such an x exists and true otherwise. It is
    equivalent to not(exists (not o f) l)).

collate f (sl, sl2)

    performs lexicographic comparison of the two slices using the
    given ordering f on elements.
*)
