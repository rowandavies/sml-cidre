 
(* Stream Library *)
(* Author: Frank Pfenning. *)
(* Additonal sorts added by Rowan Davies *)

exception Subscript;

(*[  datasort truebool = true
     datasort falsebool = false ]*)

signature STREAM =
sig
  type 'a stream
  datatype 'a front = Empty | Cons of 'a * 'a stream

  (*  datasort 'a infStream
           and 'a infFront = Cons of 'a * 'a infStream
  *)

  (* Lazy stream construction and exposure *)
  val delay : (unit -> 'a front) -> 'a stream
  val expose : 'a stream -> 'a front




  (* Eager stream construction *)
  val empty : 'a stream
  val cons : 'a * 'a stream -> 'a stream

  exception EmptyStream
  val null : 'a stream -> bool
  val hd : 'a stream -> 'a
  val tl : 'a stream -> 'a stream

  val map : ('a -> 'b) -> 'a stream -> 'b stream
  val filter : ('a -> bool) -> 'a stream -> 'a stream
  val exists : ('a -> bool) -> 'a stream -> bool

  val take : 'a stream * int -> 'a list
  val drop : 'a stream * int -> 'a stream

  val fromList : 'a list -> 'a stream
  val toList : 'a stream -> 'a list

  val tabulate : (int -> 'a) -> 'a stream
end;

structure Stream (* : STREAM *) =
struct
  datatype 'a stream = Stream of unit -> 'a front
       and 'a front = Empty | Cons of 'a * 'a stream

  (*[ datasort 'a infStream = Stream of unit -> 'a infFront
           and 'a infFront = Cons of 'a * 'a infStream       ]*)

  (*[ val delay :  (unit -> 'a front) -> 'a stream
                 & (unit -> 'a infFront) -> 'a infStream     ]*)
  fun delay (d(*:unit -> 'a front*)) = Stream(d)

  (*[ val expose : 'a stream -> 'b front
                   & 'a infStream -> 'b infFront             ]*)
  fun expose (Stream(d):'b stream) = (raise Match)
(*
  (* val empty : 'a stream *)
  val empty = Stream (fn () => Empty:'a front)

  (*[ val cons : 'a * 'a stream -> 'a stream
                 & 'a * 'a infStream -> 'a infStream ]*)
  fun cons (x, s:'a stream) = Stream (fn () => Cons (x, s))

  exception EmptyStream

  (* functions null, hd, tl, map, filter, exists, take, drop *)
  (* parallel the functions in the List structure *)
  (*[ val null : 'a stream -> bool & 'a infStream -> falsebool
      val null' : 'a front -> bool & 'a infFront -> falsebool  ]*)
  fun null (s:'a stream) = null' (expose s)
  and null' (Empty:'a front) = true
    | null' (Cons _) = false

  fun hd (s) = hd' (expose s)
  and hd' (Empty) = raise EmptyStream
    | hd' (Cons (x,s)) = x

  fun tl (s) = tl' (expose s)
  and tl' (Empty) = raise EmptyStream
    | tl' (Cons (x,s)) = s

  (*[ val map : ('a -> 'b) -> 'a stream -> 'b stream
                & ('a -> 'b) -> 'a infStream -> 'b infStream 
      val map' : ('a -> 'b) -> 'a front -> 'b front
                 & ('a -> 'b) -> 'a infFront -> 'b infFront    ]*)
  fun map (f:'a -> 'b)  s = delay (fn () => map' f (expose s))
  and map' (f:'a -> 'b) (Empty) = Empty
    | map' f (Cons(x,s)) = Cons (f(x), map f s)

  fun filter p s = delay (fn () => filter' p (expose s))
  and filter' p (Empty) = Empty
    | filter' p (Cons(x,s)) =
        if p(x) then Cons (x, filter p s)
	else filter' p (expose s)

  (*[ val exists : ('a -> bool) -> ('a stream -> bool & 'a infStream -> truebool)              
      val exists' : ('a -> bool) -> ('a front -> bool & 'a infFront -> truebool)  ]*)
  fun exists p (s:'a stream) = exists' p (expose s)  (* could add to the sort *)
  and exists' p (Empty:'a front) = false
    | exists' p (Cons(x,s)) =
        p(x) orelse exists p s

  fun takePos (s, 0) = nil
    | takePos (s, n) = take' (expose s, n)
  and take' (Empty, _) = nil
    | take' (Cons(x,s), n) = x::takePos(s, n-1)

  fun take (s,n) = if n < 0 then raise Subscript else takePos (s,n)

  fun fromList (nil) = empty
    | fromList (x::l) = cons(x,fromList(l))

  fun toList (s) = toList' (expose s)
  and toList' (Empty) = nil
    | toList' (Cons(x,s)) = x::toList(s)
  (* withsort toList' : 'a infFront -> empty[list] *)

  (*[ val dropPos : 'a stream * int -> 'a stream & 'a infStream * int -> 'a infStream
      val drop' : 'a front * int -> 'a stream & 'a infFront * int -> 'a infStream   ]*)
  fun dropPos (s, 0) = s
    | dropPos (s, n) = drop' (expose s, n)
  and drop' (Empty, _) = empty
    | drop' (Cons(x,s), n) = dropPos (s, n-1)

  (*[ val drop : 'a stream * int -> 'a stream & 'a infStream * int -> 'a infStream  ]*)
  fun drop (s,n) = if n < 0 then raise Subscript else dropPos (s,n)

  (*[ val tabulate' : (int -> 'a) -> 'a infFront
      val tabulate : (int -> 'a) -> 'a infStream   ]*)
  fun tabulate (f) = delay (fn () => tabulate' f)
  and tabulate' (f) = Cons (f(0), tabulate (fn i => f(i+1)))
*)
end;

