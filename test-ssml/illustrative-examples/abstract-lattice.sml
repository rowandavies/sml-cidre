
signature S = 
sig
  type t
  (*[ sortdef s1 |: t 
      sortdef s2 |: t ]*)

  (*[ subsort s1 & s2 < t & s1 ]*)
  (*[ subsort t & s1 < t & s2 ]*)
  (*[ subsort t & s2 < s1 & s2 ]*)
end

(*
structure S :> S = 
struct
  datatype t = C of t -> t | C2 of unit

  (*[ datasort s1 = C of s1 -> s1 ]*)
  (*[ datasort s2 = C2 of unit ]*)
end
*)