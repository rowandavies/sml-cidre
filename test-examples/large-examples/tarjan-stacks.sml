
(*** Implementations of Mihaescu/Tarjan real-time purely functional
     lists.

     As far as possible the implementations are checked by datasort
     refinements as implemented by Davies.


     Kevin Watkins
     10 May 2004
 ***)



signature STACK =
sig
  type 'a stack

(*[
  sortdef 'a valid < stack
  sortdef 'a nonempty < stack
]*)

  val push : 'a * 'a stack -> 'a stack
  val pop : 'a stack -> 'a * 'a stack

(*[
  val push <: 'a * 'a valid -> ('a valid & 'a nonempty)
  val pop <: ('a valid & 'a nonempty) -> 'a * 'a valid
]*)

end



(*** This is a strange kind of stack implementation.

     Its value lies mainly in being a simplification of the following
     non-catenable deque construction (although it has the nice
     property of log-time access and overwrite to elements anywhere
     in the stack).
 ***)
  
structure Stack :> STACK =
struct

  datatype 'a bal = Leaf of 'a | Pair of 'a bal * 'a bal

  infixr 3 $
  infixr 2 $$
  nonfix @ @@

  (* A "digit" in this representation is a list (of length 0, 1, or 2)
     of perfectly balanced trees (each of which has the same depth)

     A "word" is a list of digits (of increasing depth)

     A "sentence" is a list of words

     The "word" and "sentence" structures exist just to give us
     pointers to various positions within the data structure where
     delayed work is waiting to be done.  This interruption of the
     natural flow of the data structure is what makes the associated
     refinements complicated.  If we could state the refinements for
     the "telescoped" structure instead, and then describe the
     real structure as a sequence of "pieces" of the whole structure,
     where each "piece" fits into a hole in the next, it might be
     simpler.

     This is sort of analogous to the way a stack-machine based
     operational semantics can be stated in terms of a list of
     continuations, each fitting into the hole in the next one, with
     the types (here the refinements) having to mesh in the right way. *)

  datatype 'a word = @ | $ of 'a bal list * 'a word
  datatype 'a sentence = @@ | $$ of 'a word * 'a sentence

  type 'a stack = 'a sentence
                                    
(*[
  datasort 'a zero = nil
  datasort 'a one = :: of 'a * 'a zero
  datasort 'a two = :: of 'a * 'a one
  
  datasort 'a nonempty_w = $ of 'a bal list * 'a word
  datasort 'a nonempty_s = $$ of 'a word * 'a sentence

  datasort 'a ones_w = @ | $ of 'a bal one * 'a ones_w
  datasort 'a zero_ones_w = $ of 'a bal zero * 'a ones_w
  datasort 'a two_ones_w = $ of 'a bal two * 'a ones_w
  
  datasort 'a two_s = @@ | $$ of 'a two_ones_w * 'a zero_s
  and      'a zero_s = $$ of 'a zero_ones_w * 'a two_s

  datasort 'a valid = $$ of 'a ones_w * 'a two_s

  datasort 'a nonempty = $$ of 'a nonempty_w * 'a two_s
                       | $$ of 'a ones_w * 'a nonempty_s
]*)
                                    
(*[
  val two_to_zero <: 'a two_s -> 'a zero_s
]*)
  fun two_to_zero @@ = ([] $ @) $$ @@
    | two_to_zero (([x,y] $ [z] $ wd) $$ sn) =
        ([] $ @) $$ ([Pair(x,y),z] $ wd) $$ sn
    | two_to_zero (([x,y] $ @) $$ ([] $ wd) $$ sn) =
        ([] $ [Pair(x,y)] $ wd) $$ sn

(*[
  val push <: 'a * 'a valid -> ('a valid & 'a nonempty)
]*)
  fun push (x, @ $$ sn) =
        let val ([] $ wd) $$ sn = two_to_zero sn
        in ([Leaf(x)] $ wd) $$ sn end
    | push (x, ([y] $ wd) $$ sn) =
        let val sn = two_to_zero sn
        in @ $$ ([Leaf(x),y] $ wd) $$ sn end

  (* These cannot be validated without index refinements
     to keep track of the depths of the balanced trees within
     the structure. *)
  fun unleaf (Leaf(x)) = x
    | unleaf _ = raise Match
  fun unpair (Pair(x,y)) = (x,y)
    | unpair _ = raise Match

(*[
  val zero_to_two <: 'a zero_s -> 'a two_s
]*)
  fun zero_to_two (([] $ @) $$ @@) = @@
    | zero_to_two (([] $ @) $$ ([xy,z] $ wd) $$ sn) =
        let val (x,y) = unpair xy
        in ([x,y] $ [z] $ wd) $$ sn end
    | zero_to_two (([] $ [xy] $ wd) $$ sn) =
        let val (x,y) = unpair xy
        in ([x,y] $ @) $$ ([] $ wd) $$ sn end

(*[
  val pop <: ('a valid & 'a nonempty) -> 'a * 'a valid
]*)
  fun pop (([xx] $ wd) $$ sn) =
        let val x = unleaf xx
            val sn = zero_to_two (([] $ wd) $$ sn)
        in (x, @ $$ sn) end
    | pop (@ $$ ([xx,y] $ wd) $$ sn) =
        let val x = unleaf xx
            val sn = zero_to_two sn
        in (x, ([y] $ wd) $$ sn) end

end

