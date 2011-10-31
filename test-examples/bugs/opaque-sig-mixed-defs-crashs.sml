
structure S :> sig type 'a d (*[ sortdef 'a srt2 < d ]*) end =
struct
  datatype 'a d = T of 'a -> 'a | TT of 'a -> unit
  (*[ datasort 'a srt2 = TT of 'a -> unit ]*)
end


signature S2 = sig
   type 'a d = 'a S.d
   (*[ sortdef '+a srt2 < d ]*)    (* This should be caught as an opaque refinement of an existing type. *)
end

structure ST2 :> S2 = S     (* Instead here it prints "BUG: RefinedEnvironments.conjoinRL srt2<1454>, srt2<1451>" *)
                            (* an then correctly reports a rigid type clash error for srt2. *)
