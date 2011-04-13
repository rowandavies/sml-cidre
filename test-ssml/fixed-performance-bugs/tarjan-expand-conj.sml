
(*** Implementations of Mihaescu/Tarjan real-time purely functional
     lists.

     As far as possible the implementations are checked by datasort
     refinements as implemented by Davies.


     Kevin Watkins
     10 May 2004
 ***)

structure Deque (* :> DEQUE *) = 
struct 

  datatype 'a bal = Leaf of 'a | Pair of 'a bal * 'a bal

  infixr 3 $
  infixr 2 $$
  infixr 1 $$$
  nonfix @ @@ @@@
         
  datatype 'a word = @ | $ of 'a bal list * 'a word
  datatype 'a sentence = @@ | $$ of ('a word  * 'a word) * 'a sentence
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

 
  datasort 'a (left_s & Lzero_end_s & Ltwo_s) = 
            $$ of ('a two_ones_w * 'a word) * 'a (left_s & Lzero_end_s & Lzero_s)

  and 'a (left_s & Lzero_end_s & Lzero_s) = 
            $$ of ('a zero_ones_w * 'a word) * 'a empty_s | 
            $$ of ('a zero_ones_w * 'a word) * 'a (left_s & Lzero_end_s & Ltwo_s)

  sortdef 'a (Ltwo_s & Lzero_end_s) = (left_s * Lzero_end_s * Ltwo_s)

  datasort 'a (Ltwo_end_s & Ltwo_s) = 
            $$ of ('a two_ones_w * 'a word) * 'a empty_s | 
            $$ of ('a two_ones_w * 'a word) * 'a (Ltwo_end_s & Lzero_s)

  and 'a (Ltwo_end_s & Lzero_s) = 
            $$ of ('a zero_ones_w * 'a word) * 'a (Ltwo_end_s & Ltwo_s)


  datasort 'a (left_s & Lzero_end_s & Lzero_s) = 
            $$ of ('a zero_ones_w * 'a word) * 'a empty_s | 
            $$ of ('a zero_ones_w * 'a word) * 'a (left_s & Lzero_end_s & Ltwo_s)

  and 'a (left_s & Lzero_end_s & Ltwo_s) = 
        $$ of ('a two_ones_w * 'a word) * 'a (left_s & Lzero_end_s & Lzero_s)

  sortdef 'a (Lzero_s & Lzero_end_s) = (left_s & Lzero_end_s & Lzero_s)

  datasort 'a (Ltwo_end_s & Lzero_s) = 
           $$ of ('a zero_ones_w * 'a word) * 'a (Ltwo_end_s & Ltwo_s)

  and 'a (Ltwo_end_s & Ltwo_s) = 
            $$ of ('a two_ones_w * 'a word) * 'a empty_s | 
            $$ of ('a two_ones_w * 'a word) * 'a (Ltwo_end_s & Lzero_s)

  sortdef 'a (Lzero_s & Ltwo_end_s) = 'a (Ltwo_end_s & Lzero_s)




(*
  (* Swapped the order of the following. *)
  datasort      'a Ltwo_p  = @@@
                      | $$$ of ('a Ltwo_s  & 'a Lzero_end_s) * 'a Ltwo_p
                      | $$$ of ('a Ltwo_s  & 'a Ltwo_end_s)  * 'a Lzero_p
                      | $$$ of  'a Lone_s                    * 'a Ltwo_p
       and 'a Lzero_p = $$$ of ('a Lzero_s & 'a Lzero_end_s) * 'a Ltwo_p
                      | $$$ of ('a Lzero_s & 'a Ltwo_end_s)  * 'a Lzero_p
                      | $$$ of  'a Lone_s                    * 'a Lzero_p

  (* Swapped the order of the following. *)
  datasort      'a Rtwo_p  = @@@
                      | $$$ of ('a Rtwo_s  & 'a Rzero_end_s) * 'a Rtwo_p
                      | $$$ of ('a Rtwo_s  & 'a Rtwo_end_s)  * 'a Rzero_p
                      | $$$ of  'a Rone_s                    * 'a Rtwo_p
       and 'a Rzero_p = $$$ of ('a Rzero_s & 'a Rzero_end_s) * 'a Rtwo_p
                      | $$$ of ('a Rzero_s & 'a Rtwo_end_s)  * 'a Rzero_p
                      | $$$ of  'a Rone_s                    * 'a Rzero_p
*)

]*)

(*
  ... incomplete; there are more conditions ...  
  datasort 'a valid = $$$ of ('a short_s & 'a Lone_s & 'a Rone_s) * ('a switch_p & 'a Ltwo_p & 'a Rtwo_p)
*)

end