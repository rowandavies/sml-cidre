
signature S = 
sig
  type t
  (*[ sortdef s1 |: t 
      sortdef s2 |: t ]*)

  (*[ subsort s2 < t ]*)
end

structure S :> S = 
struct
  datatype t = C of t -> t | C2 of unit

  (*[ datasort s1 = C of s1 -> s1 ]*)
  (*[ datasort s2 = C2 of unit ]*)
end