
(* Type for natural numbers *)
datatype nat = z | s of nat

(* Even and odd natural numbers *)
(*[ datasort even = z | s of odd and odd = s of even  ]*)

(*[ datasort 'a evlist = nil | :: of 'a * 'a odlist 
         and 'a odlist = :: of 'a * 'a evlist  ]*)

(* Even and odd length lists.  *)

(* addition on natural numbers *)
(*[ val plus :>   nat * nat -> nat  &  even * even -> even  &  odd * even -> odd 
               &  even * odd -> odd  &  odd * odd -> even  (* & has lowest precedence *) ]*)
fun plus (s x, y) = plus (x, s y)
  | plus (z, y) = y


(* add the elements in a list *)
(*[ val add_list :>  nat list -> nat  &  even list -> even 
                  &  odd evlist -> even  &  odd odlist -> odd  ]*)
fun add_list  (h::t) = plus(h, add_list t)
  | add_list [] = z

(* The same as the previous example, but with an incorrect sort. *)
(*[ val add_list' :>  nat list -> nat  &  even list -> even 
                   &  nat evlist -> even  &  odd odlist -> odd  ]*)
fun add_list' (h::t) = plus(h, add_list' t)  (* Sort is wrong, error is caught *)
  | add_list' [] = z

(******** SORT CHECKER SAYS:

../test-ssml/old-examples/small.sml, line 29, column 23:
  fun add_list' (h::t) = plus(h, add_list' t)  (* Sort is wrong, error is caught *)
                         ^^^^^^^^^^^^^^^^^^^^
  Sort mismatch: nat
      expecting: even

***********************)


(* composition of two functions *)
(*[ val compose :> ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b  ]*)
fun compose (f: 'a -> 'b) g (x:'c) = f (g x)

(* double of a nat *)
(*[ val double :> nat -> even  ]*)
fun double z = z | double (s x) = s (s (double x)) 

(* half of an even nat *)
(*[ val half :> even -> nat  ]*)
fun half (s (s x)) = half x | half z = z

val f = compose double half  (* the identity on even naturals *)
val g = compose half double  (* the identity on naturals *)

(* min of two nats *)
(*[ val min :>  even * even * even -> even  &  odd * odd * odd -> even
             &  even * even * odd -> odd  &  odd * odd * even -> odd  ]*)
fun min (s x, s y, acc) = min (x, y, s acc)
  | min (z, y, acc) = acc
  | min (x, z, acc) = acc

(* min of two nats, with a mistake: non-exhaustive patterns *)
(*[ val min' :>  even * even * even -> even  &  odd * odd * odd -> even
              &  even * even * odd -> odd  &  odd * odd * even -> odd  ]*)
fun min' (s x, s y, acc) = min' (x, y, s acc)
  | min' (z, y, acc) = acc
  | min' (z, z, acc) = acc

(******** SORT CHECKER SAYS:

../test-ssml/old-examples/small.sml, line 68, column 9:
  fun min' (s x, s y, acc) = min' (x, y, s acc)
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    | min' (z, y, acc) = acc
  ^^^^^^^^^^^^^^^^^^^^^^^^^^
    | min' (z, z, acc) = acc
  ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Patterns do not cover sort:
    even * even * odd
------------------------------------------------------------------
../test-ssml/old-examples/small.sml, line 68, column 9:
  fun min' (s x, s y, acc) = min' (x, y, s acc)
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    | min' (z, y, acc) = acc
  ^^^^^^^^^^^^^^^^^^^^^^^^^^
    | min' (z, z, acc) = acc
  ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Patterns do not cover sort:
    even * even * even
------------------------------------------------------------------

*********************)

(* reversing a list, using an accumlator *)
local
(*[ val rev :>  'a evlist -> 'a evlist -> 'a evlist  &  'a evlist -> 'a odlist -> 'a odlist
             &  'a odlist -> 'a evlist -> 'a odlist  &  'a odlist -> 'a odlist -> 'a evlist  ]*)
  fun rev ([] : 'a list) acc = acc
    | rev (h::t) acc = rev t (h::acc)
in
(*[ val reverse :> 'a evlist -> 'a evlist  &  'a odlist -> 'a odlist  ]*)
  fun reverse (l: 'a list) = rev l [] 
end


(* A slighly larger example than those above: arithmetic expressions,
   with refinements for sums of products, and ground (variable free) expressions.  *)
datatype exp = Num of int | Var of string | Plus of exp * exp | Times of exp * exp
(*[ 
datasort term = Num of int | Var of string | Times of term * term
datasort sum_terms = Num of int | Var of string | Times of term * term
                   | Plus of sum_terms * sum_terms 
datasort ground = Num of int | Plus of ground * ground | Times of ground * ground
]*)

(*[ val st_times :>  sum_terms * sum_terms -> sum_terms
                  &  ground * ground -> ground   ]*)
fun st_times(Plus(st1a, st1b), st2) = Plus(st_times(st1a, st2), st_times(st1b, st2))
  | st_times(st1, Plus(st2a, st2b)) = Plus(st_times(st1, st2a), st_times(st1, st2b))
  | st_times(st1, st2) = Times (st1, st2)

(*[ val exp_to_st :>  exp -> sum_terms  &  ground -> ground  ]*)
fun exp_to_st(Times(exp1, exp2)) = st_times(exp_to_st exp1, exp_to_st exp2)
  | exp_to_st(Plus(exp1, exp2)) = Plus(exp_to_st exp1, exp_to_st exp2)
  | exp_to_st exp = exp

