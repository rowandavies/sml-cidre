(*
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
  val push : 'a * 'a valid -> ('a valid & 'a nonempty)
  val pop : ('a valid & 'a nonempty) -> 'a * 'a valid
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
  val two_to_zero :> 'a two_s -> 'a zero_s
]*)
  fun two_to_zero @@ = ([] $ @) $$ @@
    | two_to_zero (([x,y] $ [z] $ wd) $$ sn) =
        ([] $ @) $$ ([Pair(x,y),z] $ wd) $$ sn
    | two_to_zero (([x,y] $ @) $$ ([] $ wd) $$ sn) =
        ([] $ [Pair(x,y)] $ wd) $$ sn

(*[
  val push :> 'a * 'a valid -> ('a valid & 'a nonempty)
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
  val zero_to_two :> 'a zero_s -> 'a two_s
]*)
  fun zero_to_two (([] $ @) $$ @@) = @@
    | zero_to_two (([] $ @) $$ ([xy,z] $ wd) $$ sn) =
        let val (x,y) = unpair xy
        in ([x,y] $ [z] $ wd) $$ sn end
    | zero_to_two (([] $ [xy] $ wd) $$ sn) =
        let val (x,y) = unpair xy
        in ([x,y] $ @) $$ ([] $ wd) $$ sn end

(*[
  val pop :> ('a valid & 'a nonempty) -> 'a * 'a valid
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




signature DEQUE =
sig

  type 'a deque

  (* operations at the front *)
  val push : 'a * 'a deque -> 'a deque
  val pop : 'a deque -> 'a * 'a deque

  (* operations at the end *)
  val inject : 'a deque * 'a -> 'a deque
  val eject : 'a deque -> 'a deque * 'a

  (* reverse *)
  val flip : 'a deque -> 'a deque
  
end



(*** Non-catenable deques. ***)
  
structure Deque (* :> DEQUE *) =
struct
*)
  datatype 'a bal = Leaf of 'a | Pair of 'a bal * 'a bal

  infixr 3 $
  infixr 2 $$
  infixr 1 $$$
  nonfix @ @@ @@@
         
  datatype 'a word = @ | $ of ('a bal list * 'a bal list) * 'a word
  datatype 'a sentence = @@ | $$ of 'a word * 'a sentence
  datatype 'a paragraph = @@@ | $$$ of 'a sentence * 'a paragraph

  type 'a deque = 'a paragraph


(*[
  datasort 'a zero = nil
  datasort 'a one = :: of 'a * 'a zero
  datasort 'a two = :: of 'a * 'a one

  datasort 'a ones_w = @ | $ of ('a bal one * 'a bal one) * 'a ones_w
  datasort 'a Lzero_w = $ of ('a bal zero * 'a bal one)  * 'a ones_w
  datasort 'a Ltwo_w  = $ of ('a bal two  * 'a bal one)  * 'a ones_w
  datasort 'a Rzero_w = $ of ('a bal one  * 'a bal zero) * 'a ones_w
  datasort 'a Rtwo_w  = $ of ('a bal one  * 'a bal two)  * 'a ones_w
  datasort 'a zerozero_w = $ of ('a bal zero * 'a bal zero) * 'a ones_w
  datasort 'a zerotwo_w  = $ of ('a bal zero * 'a bal two)  * 'a ones_w
  datasort 'a twozero_w  = $ of ('a bal two  * 'a bal zero) * 'a ones_w
  datasort 'a twotwo_w   = $ of ('a bal two  * 'a bal two)  * 'a ones_w

  datasort 'a empty_s = @@
  datasort 'a Lzerozero_s = $$ of 'a Lzero_w * 'a empty_s
                          | $$ of 'a Lzero_w * 'a Ltwozero_s
  and      'a Lzerotwo_s  = $$ of 'a Lzero_w * 'a Ltwotwo_s
  and      'a Ltwozero_s  = $$ of 'a Ltwo_w  * 'a Lzerozero_s
  and      'a Ltwotwo_s   = $$ of 'a Ltwo_w  * 'a empty_s
                          | $$ of 'a Ltwo_w  * 'a Lzerotwo_s
  datasort 'a Rzerozero_s = $$ of 'a Rzero_w * 'a empty_s
                          | $$ of 'a Rzero_w * 'a Rtwozero_s
  and      'a Rzerotwo_s  = $$ of 'a Rzero_w * 'a Rtwotwo_s
  and      'a Rtwozero_s  = $$ of 'a Rtwo_w  * 'a Rzerozero_s
  and      'a Rtwotwo_s   = $$ of 'a Rtwo_w  * 'a empty_s
                          | $$ of 'a Rtwo_w  * 'a Rzerotwo_s

  datasort 'a zerozero_s = $$ of 'a zerozero_w * 'a empty_s
  datasort 'a zerotwo_s  = $$ of 'a zerotwo_w  * 'a empty_s
  datasort 'a twozero_s  = $$ of 'a twozero_w  * 'a empty_s
  datasort 'a twotwo_s   = $$ of 'a twotwo_w   * 'a empty_s


  datasort 'a Lzero_p = $$$ of 'a Lzerozero_s * 'a Ltwo_p
                      | $$$ of 'a Lzerotwo_s  * 'a Lzero_p
                      | $$$ of 'a Rzerozero_s * 'a Lzero_p
                      | $$$ of 'a Rzerotwo_s  * 'a Lzero_p
                      | $$$ of 'a Rtwozero_s  * 'a Lzero_p
                      | $$$ of 'a Rtwotwo_s   * 'a Lzero_p
                      | $$$ of 'a zerozero_s  * 'a Ltwo_p
                      | $$$ of 'a zerotwo_s   * 'a Ltwo_p
  and      'a Ltwo_p  = @@@
                      | $$$ of 'a Ltwozero_s  * 'a Ltwo_p
                      | $$$ of 'a Ltwotwo_s   * 'a Lzero_p
                      | $$$ of 'a Rzerozero_s * 'a Ltwo_p
                      | $$$ of 'a Rzerotwo_s  * 'a Ltwo_p
                      | $$$ of 'a Rtwozero_s  * 'a Ltwo_p
                      | $$$ of 'a Rtwotwo_s   * 'a Ltwo_p
                      | $$$ of 'a twozero_s   * 'a Lzero_p
                      | $$$ of 'a twotwo_s    * 'a Lzero_p
(*
  datasort 'a Rzero_p = $$$ of 'a Rzerozero_s * 'a Rtwo_p
                      | $$$ of 'a Rzerotwo_s  * 'a Rzero_p
                      | $$$ of 'a Lzerozero_s * 'a Rzero_p
                      | $$$ of 'a Lzerotwo_s  * 'a Rzero_p
                      | $$$ of 'a Ltwozero_s  * 'a Rzero_p
                      | $$$ of 'a Ltwotwo_s   * 'a Rzero_p
                      | $$$ of 'a zerozero_s  * 'a Rtwo_p
                      | $$$ of 'a zerotwo_s   * 'a Rtwo_p
  and      'a Rtwo_p  = @@@
                      | $$$ of 'a Rtwozero_s  * 'a Rtwo_p
                      | $$$ of 'a Rtwotwo_s   * 'a Rzero_p
                      | $$$ of 'a Lzerozero_s * 'a Rtwo_p
                      | $$$ of 'a Lzerotwo_s  * 'a Rtwo_p
                      | $$$ of 'a Ltwozero_s  * 'a Rtwo_p
                      | $$$ of 'a Ltwotwo_s   * 'a Rtwo_p
                      | $$$ of 'a twozero_s   * 'a Rzero_p
                      | $$$ of 'a twotwo_s    * 'a Rzero_p
*)

]*)

(*  
(*
  Failed attempt 1 (Rowan's system is still processing these datasorts
  after an hour on my machine)
  
  datatype 'a bal = Leaf of 'a | Pair of 'a bal * 'a bal

  infixr 3 $
  infixr 2 $$
  infixr 1 $$$
  nonfix @ @@ @@@
         
  datatype 'a word = @ | $ of 'a bal list * 'a word
  datatype 'a ww = WW of 'a word * 'a word
  datatype 'a sentence = @@ | $$ of ('a ww) * 'a sentence
  datatype 'a paragraph = @@@ | $$$ of 'a sentence * 'a paragraph

  type 'a deque = 'a paragraph

(*[
  datasort 'a zero = nil
  datasort 'a one = :: of 'a * 'a zero
  datasort 'a two = :: of 'a * 'a one
  
  datasort 'a ones_w = @ | $ of 'a bal one * 'a ones_w
  datasort 'a zero_ones_w = $ of 'a bal zero * 'a ones_w
  datasort 'a two_ones_w = $ of 'a bal two * 'a ones_w

  (* Strangely, there's no syntax for this in Davies' implementation
     datasort 'a switch_w = 'a zero_ones_w | 'a two_ones_w *)
  datasort 'a switch_w = $ of 'a bal zero * 'a ones_w
                       | $ of 'a bal two * 'a ones_w

  datasort 'a empty_s = @@
  datasort 'a short_s = $$ of ('a word * 'a word) * 'a empty_s
  datasort 'a left_s  = $$ of ('a switch_w * 'a word) * 'a sentence
  datasort 'a right_s = $$ of ('a word * 'a switch_w) * 'a sentence


  datasort 'a Lzero_end_s = $$ of ('a zero_ones_w * 'a word)        * 'a empty_s
                          | $$ of ('a word        * 'a word)        * 'a Lzero_end_s
  datasort 'a Ltwo_end_s  = $$ of ('a two_ones_w  * 'a word)        * 'a empty_s
                          | $$ of ('a word        * 'a word)        * 'a Ltwo_end_s
  datasort 'a Rzero_end_s = $$ of ('a word        * 'a zero_ones_w) * 'a empty_s
                          | $$ of ('a word        * 'a word)        * 'a Rzero_end_s
  datasort 'a Rtwo_end_s  = $$ of ('a word        * 'a zero_ones_w) * 'a empty_s
                          | $$ of ('a word        * 'a word)        * 'a Rtwo_end_s

  datasort 'a Lzero_s = @@ | $$ of ('a zero_ones_w * 'a word)        * 'a Ltwo_s
  and      'a Ltwo_s  = @@ | $$ of ('a two_ones_w  * 'a word)        * 'a Lzero_s
  datasort 'a Lone_s  = @@ | $$ of ('a ones_w      * 'a word)        * 'a Lone_s
  datasort 'a Rzero_s = @@ | $$ of ('a word        * 'a zero_ones_w) * 'a Rtwo_s
  and      'a Rtwo_s  = @@ | $$ of ('a word        * 'a two_ones_w)  * 'a Rzero_s
  datasort 'a Rone_s  = @@ | $$ of ('a word        * 'a ones_w)      * 'a Rone_s

  datasort 'a switch_p = @@@
                       | $$$ of 'a short_s * 'a switch_p
                       | $$$ of 'a left_s  * 'a switch_p
                       | $$$ of 'a right_s * 'a switch_p

  datasort 'a Lzero_p = $$$ of ('a Lzero_s & 'a Lzero_end_s) * 'a Ltwo_p
                      | $$$ of ('a Lzero_s & 'a Ltwo_end_s)  * 'a Lzero_p
                      | $$$ of  'a Lone_s                    * 'a Lzero_p
  and      'a Ltwo_p  = @@@
                      | $$$ of ('a Ltwo_s  & 'a Lzero_end_s) * 'a Ltwo_p
                      | $$$ of ('a Ltwo_s  & 'a Ltwo_end_s)  * 'a Lzero_p
                      | $$$ of  'a Lone_s                    * 'a Ltwo_p
  datasort 'a Rzero_p = $$$ of ('a Rzero_s & 'a Rzero_end_s) * 'a Rtwo_p
                      | $$$ of ('a Rzero_s & 'a Rtwo_end_s)  * 'a Rzero_p
                      | $$$ of  'a Rone_s                    * 'a Rzero_p
  and      'a Rtwo_p  = @@@
                      | $$$ of ('a Rtwo_s  & 'a Rzero_end_s) * 'a Rtwo_p
                      | $$$ of ('a Rtwo_s  & 'a Rtwo_end_s)  * 'a Rzero_p
                      | $$$ of  'a Rone_s                    * 'a Rtwo_p

  ... incomplete; there are more conditions ...  
  datasort 'a valid = $$$ of ('a short_s & 'a Lone_s & 'a Rone_s) * ('a switch_p & 'a Ltwo_p & 'a Rtwo_p)

]*)
*)


(*
  Failed attempt 2

  datatype 'a bal = Leaf of 'a | Pair of 'a bal * 'a bal

  infixr 3 $
  infixr 2 $$
  infixr 1 $$$
  nonfix @ @@ @@@
         
  datatype 'a word = @ | $ of ('a bal list * 'a bal list) * 'a word
  datatype 'a sentence = @@ | $$ of 'a word * 'a sentence
  datatype 'a paragraph = @@@ | $$$ of 'a sentence * 'a paragraph

  type 'a deque = 'a paragraph

(*[
  datasort 'a zero = nil
  datasort 'a one = :: of 'a * 'a zero
  datasort 'a two = :: of 'a * 'a one

  datasort 'a ones_w = @ | $ of ('a bal one * 'a bal one) * 'a ones_w
  datasort 'a Lzero_w = $ of ('a bal zero * 'a bal one)  * 'a ones_w
  datasort 'a Ltwo_w  = $ of ('a bal two  * 'a bal one)  * 'a ones_w
  datasort 'a Rzero_w = $ of ('a bal one  * 'a bal zero) * 'a ones_w
  datasort 'a Rtwo_w  = $ of ('a bal one  * 'a bal two)  * 'a ones_w
  datasort 'a zerozero_w = $ of ('a bal zero * 'a bal zero) * 'a ones_w
  datasort 'a zerotwo_w  = $ of ('a bal zero * 'a bal two)  * 'a ones_w
  datasort 'a twozero_w  = $ of ('a bal two  * 'a bal zero) * 'a ones_w
  datasort 'a twotwo_w   = $ of ('a bal two  * 'a bal two)  * 'a ones_w

  datasort 'a empty_s = @@
  datasort 'a Lzero_s = @@ | $$ of 'a Lzero_w * 'a Ltwo_s
  and      'a Ltwo_s  = @@ | $$ of 'a Ltwo_w  * 'a Lzero_s
  datasort 'a Rzero_s = @@ | $$ of 'a Rzero_w * 'a Rtwo_s
  and      'a Rtwo_s  = @@ | $$ of 'a Rtwo_w  * 'a Rzero_s
  datasort 'a zerozero_s = $$ of 'a zerozero_w * 'a empty_s
  datasort 'a zerotwo_s  = $$ of 'a zerotwo_w  * 'a empty_s
  datasort 'a twozero_s  = $$ of 'a twozero_w  * 'a empty_s
  datasort 'a twotwo_s   = $$ of 'a twotwo_w   * 'a empty_s

  datasort 'a Lzero_end_s = $$ of 'a Lzero_w * 'a empty_s
                          | $$ of 'a word    * 'a Lzero_end_s
  datasort 'a Ltwo_end_s  = $$ of 'a Ltwo_w  * 'a empty_s
                          | $$ of 'a word    * 'a Ltwo_end_s
  datasort 'a Rzero_end_s = $$ of 'a Rzero_w * 'a empty_s
                          | $$ of 'a word    * 'a Rzero_end_s
  datasort 'a Rtwo_end_s  = $$ of 'a Rtwo_w  * 'a empty_s
                          | $$ of 'a word    * 'a Rtwo_end_s

  datasort 'a Lzero_p = $$$ of ('a Lzero_s & 'a Lzero_end_s) * 'a Ltwo_p
                      | $$$ of ('a Lzero_s & 'a Ltwo_end_s)  * 'a Lzero_p
                      | $$$ of 'a Rzero_s                    * 'a Lzero_p
                      | $$$ of 'a Rtwo_s                     * 'a Lzero_p
                      | $$$ of 'a zerozero_s                 * 'a Ltwo_p
                      | $$$ of 'a zerotwo_s                  * 'a Ltwo_p
  and      'a Ltwo_p  = @@@
                      | $$$ of ('a Ltwo_s  & 'a Lzero_end_s) * 'a Ltwo_p
                      | $$$ of ('a Ltwo_s  & 'a Ltwo_end_s)  * 'a Lzero_p
                      | $$$ of 'a Rzero_s                    * 'a Ltwo_p
                      | $$$ of 'a Rtwo_s                     * 'a Ltwo_p
                      | $$$ of 'a twozero_s                  * 'a Lzero_p
                      | $$$ of 'a twotwo_s                   * 'a Lzero_p

  datasort 'a Rzero_p = $$$ of ('a Rzero_s & 'a Rzero_end_s) * 'a Rtwo_p
                      | $$$ of ('a Rzero_s & 'a Rtwo_end_s)  * 'a Rzero_p
                      | $$$ of 'a Lzero_s                    * 'a Rzero_p
                      | $$$ of 'a Ltwo_s                     * 'a Rzero_p
                      | $$$ of 'a zerozero_s                 * 'a Rtwo_p
                      | $$$ of 'a twozero_s                  * 'a Rtwo_p
  and      'a Rtwo_p  = @@@
                      | $$$ of ('a Rtwo_s  & 'a Rzero_end_s) * 'a Rtwo_p
                      | $$$ of ('a Rtwo_s  & 'a Rtwo_end_s)  * 'a Rzero_p
                      | $$$ of 'a Lzero_s                    * 'a Rtwo_p
                      | $$$ of 'a Ltwo_s                     * 'a Rtwo_p
                      | $$$ of 'a zerotwo_s                  * 'a Rzero_p
                      | $$$ of 'a twotwo_s                   * 'a Rzero_p

  ... incomplete ...

]*)
*)
end
*)