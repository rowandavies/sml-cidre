(* This isn't actually a bug currently.  But, it's a potential one in
   future once it's possible to include refinements along with an
   opaque type in a signature.
 *)


structure S :> sig type 'a t (*[ sortdef 'a s < t sortdef 'a d < t sortdef 'a srt2 < t sortdef 'a d2 < d val x : ('a->'a) -> 'a s ]*) end
= struct
  datatype 'a t = T of 'a -> 'a | TT of 'a -> unit
  (*[ datasort 'a d2 = T of 'a -> 'a | TT of 'a -> unit ]*)
  (*[ datasort 'a s = T of 'a -> 'a ]*)
  (*[ datasort 'a srt2 = TT of 'a -> unit ]*)
  type 'a d = 'a t

  val x : ('a -> 'a) -> 'a t = T
end


structure SS :> sig type 'a t = 'a S.t end
= struct
  type 'a t = 'a S.t
end


signature S2 = sig

   type 'a t = 'a S.t

(*   (*[ sortdef '+a d2 < d ]*)
   (*[ subsort d < d2 ]*)
   (*[ sortdef '+a srt2 < d ]*)
*)
  (*[ x <: ('a->'a) -> 'a S.s ]*)
  val x : ('a->'a) -> 'a S.s
end


(*signature S3 = S2 where type 'a d = 'a S.t
*)

structure ST2 :> S2 = struct
  open SS
  
  val x = S.x 
end


(*
structure ST = struct (*[ assumesig S2 ]*) end
type 'a x = 'a ST2.d
type 'a x = 'a ST2.d    (* reports a type.  I"m guessing because the sort equals the mlTy *)
*)

(* type 'a x = 'a ST.str2 *)

(* This is just for comparison - for datasorts it seems to work. *)
(*
signature S = sig

   datatype 'a d = D
   (*[ datasort '+a d = D ]*)
(*   (*[ sortdef '+a d |: d ]*)
   (*[ sortdef '+a srt |: d ]*)
*)
end
*)
