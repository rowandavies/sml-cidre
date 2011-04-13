
signature S = 
sig
  type t
  (*[ sortdef s1 < t ]*)

  type t2
  (*[ sortdef s2 < t2 ]*)  

end

structure S :> S = 
struct
  datatype t = C1 | C2
  (*[ datasort s1 = C1 ]*)
  (*[ datasort s2 = C2 ]*)

  (* datatype t2 = datatype t *)
  type t2 = t
end

functor F() :> S = 
struct
  datatype t = C1 of t -> t | C2 of t -> t
  (*[ datasort s1 = C1 of t -> s1 ]*)
  (*[ datasort s2 = C2 of t -> s2 ]*)

  (* datatype t2 = datatype t *)
  type t2 = t
end