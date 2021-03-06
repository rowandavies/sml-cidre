(*ARRAY.sml*)

signature ARRAY = sig

(*TODO 21/10/1997 11:36. tho.: skal det ikke v�re eqtype?*)
  eqtype 'a array
  type 'a vector

  val maxLen   : int
  val array    : int * 'a -> 'a array
  val fromList : 'a list -> 'a array
  val tabulate : int * (int -> 'a) -> 'a array
  val length  : 'a array -> int
  val sub     : 'a array * int -> 'a
  val update  : 'a array * int * 'a  -> unit
  val vector : 'a array -> 'a vector

  val copy    : {src: 'a array, dst: 'a array, di: int} -> unit
  val copyVec : {src: 'a vector, dst: 'a array, di: int} -> unit

  val appi    : (int * 'a -> unit) -> 'a array -> unit
  val app     : ('a -> unit) -> 'a array -> unit
  val modifyi : (int * 'a -> 'a) -> 'a array -> unit
  val modify  : ('a -> 'a) -> 'a array -> unit
  val foldli  : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
  val foldri  : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
  val foldl   : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
  val foldr   : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b


  val findi : (int * 'a -> bool)
                -> 'a array -> (int * 'a) option
  val find  : ('a -> bool) -> 'a array -> 'a option
  val exists : ('a -> bool) -> 'a array -> bool
  val all : ('a -> bool) -> 'a array -> bool
  val collate : ('a * 'a -> order)
                  -> 'a array * 'a array -> order
end (*signature ARRAY*)
where type 'a array = 'a array
where type 'a vector = 'a vector

(* Type [ty array] is the type of one-dimensional, mutable, zero-based
   constant-time-access arrays with elements of type ty.  Type ty
   array admits equality even if ty does not.  Arrays a1 and a2 are
   equal if both were created by the same call to a primitive (array,
   tabulate, fromList).

   Some functions work on a *slice* of an array:

   The slice (a, i, SOME n) denotes the subarray a[i..i+n-1].  That is,
   a[i] is the first element of the slice, and n is the length of the
   slice.  Valid only if 0 <= i <= i+n <= length a.

   The slice (a, i, NONE) denotes the subarray a[i..length a-1].  That
   is, the slice denotes the suffix of the array starting at i.  Valid
   only if 0 <= i <= length a.  Equivalent to (a, i, SOME(length a - i)).

       slice             meaning 
       ----------------------------------------------------------
       (a, 0, NONE)      the whole array              a[0..len-1]   
       (a, 0, SOME n)    a left subarray (prefix)     a[0..n-1]
       (a, i, NONE)      a right subarray (suffix)    a[i..len-1]
       (a, i, SOME n)    a general slice              a[i..i+n-1] 

   [maxLen] is the maximal number of elements in an array.

   [array(n, x)] returns a new array of length n whose elements are all x.
   Raises Size if n<0 or n>maxLen.

   [tabulate(n, f)] returns a new array of length n whose elements
   are f 0, f 1, ..., f (n-1), created from left to right.  Raises
   Size if n<0 or n>maxLen.

   [fromList xs] returns an array whose elements are those of xs.
   Raises Size if length xs > maxLen.

   [array0] is a zero-length array.

   [length a] returns the number of elements in a.

   [sub(a, i)] returns the i'th element of a, counting from 0.  
   Raises Subscript if i<0 or i>=length a.  To make `sub' infix, use
   the declaration 
                             infix 9 sub

   [update(a, i, x)] destructively replaces the i'th element of a by x.
   Raises Subscript if i<0 or i>=length a.

   [extract(a, i, NONE)] returns a vector of the elements a[i..length a-1] 
   of a.  Raises Subscript if i<0 or i>length a.

   [extract(a, i, SOME len)] returns a vector of the elements a[i..i+len-1] 
   of a.  Raises Subscript if i<0 or len<0 or i+len>length a or
   len>Vector.maxLen.

   [copy{src, si, len, dst, di}] destructively copies the slice
   (src, si, len) to dst, starting at index di.  More precisely:
   If len=NONE and n=length src, it copies src[si..n-1] to dst[di..di+n-si].
   If len=SOME k, it copies src[si..si+k-1] to dst[di..di+k-1].  
   Works also if src and dst are the same and the segments overlap.
   Raises Subscript if si < 0 or di < 0, 
   or if len=NONE and di + length src - si > length dst,
   or if len=SOME k and k < 0 or si + k > length src or di + k > length dst.

   [copyVec{src, si, len, dst, di}] destructively copies the slice
   (src, si, len) to dst, starting at index di.  More precisely:
   If len=NONE and n=length src, it copies src[si..n-1] to dst[di..di+n-si].
   If len=SOME k, it copies src[si..si+k-1] to dst[di..di+k-1].  
   Works also if src and dst are the same and the segments overlap.
   Raises Subscript if si < 0 or di < 0, 
   or if len=NONE and di + length src - si > length dst,
   or if len=SOME k and k < 0 or si + k > length src or di + k > length dst.

   [foldl f e a] folds function f over a from left to right.  That is,
   computes f(a[len-1], f(a[len-2], ..., f(a[1], f(a[0], e)) ...)),
   where len is the length of a.

   [foldr f e a] folds function f over a from right to left.  That is,
   computes f(a[0], f(a[1], ..., f(a[len-2], f(a[len-1], e)) ...)),
   where len is the length of a.

   [app f a] applies f to a[j] for j=0,1,...,length a-1.

   [modify f a] applies f to a[j] and updates a[j] with the result
   f(a[j]) for j=0,1,...,length a-1. 

   The following iterators generalize the above ones in two ways:

    . the index j is also being passed to the function being iterated;
    . the iterators work on a slice (subarray) of an array.

   [foldli f e (a, i, SOME n)] folds function f over the subarray
   a[i..i+n-1] from left to right.  That is, computes 
   f(i+n-1, a[i+n-1], f(..., f(i+1, a[i+1], f(i, a[i], e)) ...)).  
   Raises Subscript if i<0 or n<0 or i+n > length a.

   [foldli f e (a, i, NONE)] folds function f over the subarray
   a[i..len-1] from left to right, where len =  length a.  That is, 
   computes f(len-1, a[len-1], f(..., f(i+1, a[i+1], f(i, a[i], e)) ...)).  
   Raises Subscript if i<0 or i > length a.

   [foldri f e (a, i, SOME n)] folds function f over the subarray
   a[i..i+n-1] from right to left.  That is, computes 
   f(i, a[i], f(i+1, a[i+1], ..., f(i+n-1, a[i+n-1], e) ...)).
   Raises Subscript if i<0 or n<0 or i+n > length a.

   [foldri f e (a, i, NONE)] folds function f over the subarray
   a[i..len-1] from right to left, where len = length a.  That is, 
   computes f(i, a[i], f(i+1, a[i+1], ..., f(len-1, a[len-1], e) ...)).
   Raises Subscript if i<0 or i > length a.

   [appi f (a, i, SOME n)] applies f to successive pairs (j, a[j]) for
   j=i,i+1,...,i+n-1.  Raises Subscript if i<0 or n<0 or i+n > length a.

   [appi f (a, i, NONE)] applies f to successive pairs (j, a[j]) for
   j=i,i+1,...,len-1, where len = length a.  Raises Subscript if i<0
   or i > length a.

   [modifyi f (a, i, SOME n)] applies f to (j, a[j]) and updates a[j]
   with the result f(j, a[j]) for j=i,i+1,...,i+n-1.  Raises Subscript
   if i<0 or n<0 or i+n > length a.

   [modifyi f (a, i, NONE)] applies f to (j, a[j]) and updates a[j]
   with the result f(j, a[j]) for j=i,i+1,...,len-1.  Raises Subscript
   if i<0 or i > length a.
*)
