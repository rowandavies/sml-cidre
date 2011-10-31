
(* There's two things wrong:
   a) The variance of d isn't improved, instead d2 loses it's variance
   b) The variance for the struct in the assumesig doesn't match the signature.
      Instead it's always mixed variance.

  Okay, this works now. When d becomes a subsort of d2, it acquires covariance.
  And it fails when you remove the "+" in d2 in S2.  As it should.
*)
signature S2 = sig

   type 'a d
   (*[ sortdef '+a d2 < d ]*)
   (*[ subsort d < d2 ]*)
   (*[ sortdef '+a srt2 < d ]*)

end

structure ST = struct (*[ assumesig S2 ]*) end


structure S3 = struct

   (*[ f <: 'a ST.srt2 ST.d -> 'a ST.d ST.d ]*)
   fun f x = x

   (*[ g <: 'a ST.srt2 VectorSlice.slice -> 'a ST.d VectorSlice.slice ]*)
   fun g x = x


end





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
