(* Option -- SML Basis Library *)

signature OPTION =
sig

  datatype 'a option = NONE | SOME of 'a
  exception Option
  val getOpt : 'a option * 'a -> 'a
  val isSome : 'a option -> bool
  val valOf : 'a option -> 'a
  val filter : ('a -> bool) -> 'a -> 'a option
  val join : 'a option option -> 'a option
  val app : ('a -> unit) -> 'a option -> unit
  val map : ('a -> 'b) -> 'a option -> 'b option
  val mapPartial : ('a -> 'b option)
                     -> 'a option -> 'b option
  val compose : ('a -> 'b) * ('c -> 'a option)
                  -> 'c -> 'b option
  val composePartial : ('a -> 'b option) * ('c -> 'a option)
                         -> 'c -> 'b option

end 
where type 'a option = 'a option
;

(* 
   [getOpt (xopt, d)] returns x if xopt is SOME x; returns d otherwise.

   [isSome vopt] returns true if xopt is SOME x; returns false otherwise.

   [valOf vopt] returns x if xopt is SOME x; raises Option otherwise.

   [filter p x] returns SOME x if p x is true; returns NONE otherwise.

   [map f xopt] returns SOME (f x) if xopt is SOME x; returns NONE otherwise.

   [join xopt] returns x if xopt is SOME x; returns NONE otherwise.

   [compose (f, g) x] returns SOME (f y) if g x is SOME y; returns NONE 
   otherwise.  It holds that compose (f, g) = map f o g.

   [mapPartial f xopt] returns f x if xopt is SOME x; returns NONE otherwise.  
   It holds that mapPartial f = join o map f.

   [composePartial (f, g) x] returns f y if g x is SOME y; returns NONE 
   otherwise.  It holds that composePartial (f, g) = mapPartial f o g.

   The operators (map, join, SOME) form a monad.
*)
