
structure S :> sig type 'a t (*[ sortdef 'a d < t ]*) end = 
struct
  datatype 'a t = T of 'a -> 'a | TT of 'a -> unit
  (*[ datasort 'a d = T of 'a -> 'a ]*)
end

signature S2 = sig

   type 'a d = 'a S.t

   (* Now, what's the equivalent for refinements?  *)
   (* There's no way to just "package" them up with the type.  *)
   
   (*[ sortdef 'a d < S.t ]*)  (* Accepted due to a bug currently, but crashes below. *)

end


(* This at least prints an appropriate error about giving the whole signature - 
   but there's no way to give the whole signature! *)
(*
signature S3 = S2 where type 'a d = 'a S.t
*)

(* Reports a bug, then prints a rigidity error. *)
structure ST2 :> S2 = S
