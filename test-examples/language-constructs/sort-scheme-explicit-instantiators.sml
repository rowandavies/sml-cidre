
datatype ('a,'b,'c,'d) t = C1 of 'b | C2 of 'c | C3 of 'a | C4 of 'd

(* The instantiators are in the same order as the their first
   occurrences in the type scheme *)

val v = C1 (*[[ 'a->'a, bool, int, real ]]*)  (fn x => x)

val v2 = fn () => C1 (*[[ 'a->'a, 'd, 'c, 'b ]]*)  (fn x => x)

val v3 = v2 (*[[ int, real, bool, word ]]*)


fun f x y z = x y z

val fInst = f (*[[ int, real, bool ]]*)

fun g x y = x
val g2 = g (*[[ int, bool ]]*)

fun h x (y,z) = x
val h2 = h (*[[ int, bool, real ]]*)
