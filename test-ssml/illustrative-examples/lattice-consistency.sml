

(* This example should be rejected: the second subsorting isn't validated
   by the structure, (not: r1 & r2 < s1 & s2).  An additional check needs
   to be added to the implementation.
*)

structure LatticeConsistency :
  sig
    type t
    (*[ sortdef s1 < t ]*)
    (*[ sortdef s2 < t ]*)
    (*[ sortdef r1 < t ]*)
    (*[ sortdef r2 < t ]*)

    (*[ subsort s1 & s2 < r1 & r2 ]*)
    (*[ subsort r1 & r2 < s1 & s2 ]*)
  end
=
struct
  datatype t = C | D1 | D2 | E1 | E2 | F

  (*[ datasort s1 = C | D1 and s2 = C | D2 ]*)
  (*[ datasort r1 = C | E1 | F and r2 = C | E2 | F ]*)

end