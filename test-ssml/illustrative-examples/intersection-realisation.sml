
(* This example demonstrates the checking for intersections during signature matching. *)

signature S =
sig
  type t
  (*[ sortdef s < t ]*)
  (*[ sortdef s2 < t ]*)
  (*[ sortdef s3 |: t ]*)

  (*[ subsort s3 < s & s2 ]*)

  val f : t
end

structure S : S = 
struct
  (*[ datasort tt = true and ff = false ]*)
  type t = bool -> bool
  (*[ sortdef t = (tt & ff) -> bool ]*)

  (*[ sortdef s = bool -> bool ]*)
  (*[ sortdef s2 = tt -> tt ]*)
  (*[ sortdef s3 = bool -> bool & tt -> bool ]*)

  fun f x = x
end