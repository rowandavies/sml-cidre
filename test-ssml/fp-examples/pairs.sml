(* Pairs as functions *)
(* Almost a Church encoding *)
(* Author: Frank Pfenning *)
(* Based on a student's idea form 15-312, Fall'03 *)

datatype ('a,'b) sum = inl of 'a | inr of 'b
(*[
datasort ('a,'b) left = inl of 'a
     and ('a,'b) right = inr of 'b
]*)

type ('a,'b) pair = ('a -> 'b -> ('a,'b) sum) -> ('a,'b) sum
(*[
sortdef ('a,'b) prod = ('a -> 'b -> ('a,'b) left) -> ('a,'b) left
                     & ('a -> 'b -> ('a,'b) right) -> ('a,'b) right
]*)

(*[ val pair : 'a -> 'b -> ('a,'b) prod ]*)
fun pair (x:'a) (y:'b) : ('a,'b) pair =
    fn f => f x y
(*[ val fst : ('a,'b) prod -> 'a ]*)
fun fst (p:('a,'b) pair) : 'a =
    case p (fn x => fn y => inl x) of inl x => x (* missing case impossible! *)
(*[ val snd : ('a,'b) prod -> 'b ]*)
fun snd (p:('a,'b) pair) : 'b =
    case p (fn x => fn y => inr y) of inr y => y (* missing case impossible! *)

val p12 = pair 1 2;
val one = fst p12;
val two = snd p12;

val p1t = pair 1 true;
val one = fst p1t;
val t = snd p1t;

val ptf = pair true false;
val t = fst ptf;
val f = snd ptf;
