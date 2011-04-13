(*LIST.sml*)

structure List:LIST = struct(*[ assumesig LIST ]*)
  type 'a list = unit
  exception Empty
  val null : 'a list -> bool  = fn _ => raise Match
  val length : 'a list -> int  = fn _ => raise Match
  val @ : ('a list * 'a list) -> 'a list  = fn _ => raise Match
  val hd : 'a list -> 'a  = fn _ => raise Match
  val tl : 'a list -> 'a list  = fn _ => raise Match
  val last : 'a list -> 'a  = fn _ => raise Match
  val getItem : 'a list -> ('a * 'a list) option = fn _ => raise Match
  val nth : ('a list * int) -> 'a  = fn _ => raise Match
  val take : ('a list * int) -> 'a list  = fn _ => raise Match
  val drop : ('a list * int) -> 'a list  = fn _ => raise Match
  val rev : 'a list -> 'a list  = fn _ => raise Match
  val concat : 'a list list -> 'a list  = fn _ => raise Match
  val revAppend : ('a list * 'a list) -> 'a list  = fn _ => raise Match
  val app : ('a -> unit) -> 'a list -> unit  = fn _ => raise Match
  val map : ('a -> 'b) -> 'a list -> 'b list  = fn _ => raise Match
  val mapPartial : ('a -> 'b option) -> 'a list -> 'b list  = fn _ => raise Match
  val find : ('a -> bool) -> 'a list -> 'a option  = fn _ => raise Match
  val filter : ('a -> bool) -> 'a list -> 'a list  = fn _ => raise Match
  val partition : ('a -> bool) -> 'a list -> ('a list * 'a list)  = fn _ => raise Match
  val foldl : (('a * 'b) -> 'b) -> 'b -> 'a list -> 'b  = fn _ => raise Match
  val foldr : (('a * 'b) -> 'b) -> 'b -> 'a list -> 'b  = fn _ => raise Match
  val exists : ('a -> bool) -> 'a list -> bool  = fn _ => raise Match
  val all : ('a -> bool) -> 'a list -> bool  = fn _ => raise Match
  val tabulate : (int * (int -> 'a)) -> 'a list  = fn _ => raise Match
end; (*signature LIST*)

fun null a = List.null a
fun hd a = List.hd a
fun tl a = List.tl a
fun length a = List.length a
fun rev a = List.rev a
fun op @ (l1, l2)  = List.@(l1, l2)
fun app f l = List.app f l
fun map f al = List.map f al
fun foldr f e l = List.foldr f e l
fun foldl f e l = List.foldl f e l

(* 
   [null xs] is true iff xs is nil.

   [hd xs] returns the first element of xs.  Raises Empty if xs is nil.

   [tl xs] returns all but the first element of xs.  
   Raises Empty if xs is nil.

   [last xs] returns the last element of xs.  Raises Empty if xs is nil.

   [nth(xs, i)] returns the i'th element of xs, counting from 0.
   Raises Subscript if i<0 or i>=length xs.

   [take(xs, i)] returns the first i elements of xs.  Raises Subscript
   if i<0 or i>length xs.

   [drop(xs, i)] returns what is left after dropping the first i
   elements of xs.  Raises Subscript if i<0 or i>length xs.  
   It holds that take(xs, i) @ drop(xs, i) = xs when 0 <= i <= length xs.

   [length xs] returns the number of elements in xs.

   [rev xs] returns the list of xs's elements, reversed.

   [xs @ ys] returns the list which is the concatenation of xs and ys.

   [concat xss] returns the list which is the concatenation of all the
   lists in xss.

   [app f xs] applies f to the elements of xs, from left to right.

   [map f xs] applies f to each element x of xs, from left to
   right, and returns the list of f's results.
 
   [mapPartial f xs] applies f to each element x of xs, from left
   to right, and returns the list of those y's for which f(x)
   evaluated to SOME y.

   [find p xs] applies f to each element x of xs, from left to
   right until p(x) evaluates to true; returns SOME x if such an x
   exists otherwise NONE.

   [filter p xs] applies p to each element x of xs, from left to
   right, and returns the sublist of those x for which p(x) evaluated
   to true.

   [partition p xs] applies p to each element x of xs, from left
   to right, and returns a pair (pos, neg) where pos is the sublist
   of those x for which p(x) evaluated to true, and neg is the sublist of
   those for which p(x) evaluated to false.

   [foldr op% e xs] evaluates x1 % (x2 % ( ... % (x(n-1) % (xn % e)) ... ))
   where xs = [x1, x2, ..., x(n-1), xn], and % is taken to be infixed.

   [foldl op% e xs] evaluates xn % (x(n-1) % ( ... % (x2 % (x1 % e))))
   where xs = [x1, x2, ..., x(n-1), xn], and % is taken to be infixed.

   [exists p xs] applies p to each element x of xs, from left to
   right until p(x) evaluates to true; returns true if such an x
   exists, otherwise false.

   [all p xs] applies p to each element x of xs, from left to
   right until p(x) evaluates to false; returns false if such an x
   exists, otherwise true.

   [tabulate(n, f)] returns a list of length n whose elements are
   f(0), f(1), ..., f(n-1), created from left to right.  Raises Size
   if n<0.

   [getItem xs] attempts to extract an element from the list xs.  It
   returns NONE if xs is empty, and returns SOME (x, xr) if xs=x::xr.
   This can be used for scanning booleans, integers, reals, and so on
   from a list of characters.  For instance, to scan a decimal integer
   from a list cs of characters, compute 
	Int.scan StringCvt.DEC List.getItem cs
*)
