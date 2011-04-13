(* This is a snapshot just after modifying expandConj, with the original code in comments.  
   - Rowan 6oct04 *)

         (* Helper function with accumulator for expandConj *)
         (* NOT USED CURRENTLY *)
         (*[ val expC :> con * SortName list * SortFcn option -> SortFcn option list ]*)
         fun expC (con, [], sortfcn_opt) = [sortfcn_opt]
           | expC (con, sortname::sns_tail, sortfcn_opt) =
             case lookupR (R, sortname)
               of NONE => Crash.impossible "RefinedEnvironments.expC(1)"
                | SOME RC => 
                  (case (lookupRC (RC, con), sortfcn_opt)
                     of (NONE, _) => []  (* Constructor not in datasort. *)
                      | (SOME NONE, NONE) => expC (con, sns_tail, NONE)
                      | (SOME (SOME sortfcn_list), SOME sf) =>
                        List.foldl 
                          (fn (sf', r) => 
                             (expC (con, sns_tail, SOME (RO.conjSortFcn conjSN (sf, sf'))))
                             @ r)
                          []
                          sortfcn_list
                      | _ => Crash.impossible "RefinedEnvironment.expC(2)"
                  )
         (* Expand a conjunction by looking up a constructor, then multiplying out alternatives *)
         (* e.g. muliplying out: (c of R_1 | c of R_2) & (c of S_1 | c of S_2) 
                 yields:         (R_1 & S_1) | (R_1 & S_2) | (R_2 & S_1) | (R_2 & S_2)  *)
         (* Try to simplify this code!  *)
         (* Well, one simplification might be to use intersectRC.  I think that does essentially 
            the same thing. - Rowan 6oct04. *)
         (*[ val expandConj :> con * SortName list -> SortFcn option list ]*)
         fun expandConj (_, []) = Crash.impossible "RefinedEnvironment.expandConj(1)"
           | expandConj (con, sortname::sortnames) = 
           case lookupR (R, sortname)
             of NONE => Crash.impossible "RefinedEnvironments.expandConj(2)"
              | SOME RC => 
                (case lookupRC (RC, con)
                   of NONE => []                            (* con absent from datasort body. *)
                    | SOME NONE =>                        (* con present and has no argument. *)
		        (case sortnames of [] => [NONE] 
					 | _ => expandConj (con, sortnames))
			(* expC (con, sortnames, NONE) *)
                    | SOME (SOME sortfcn_list) =>               (* con present with argument. *)
		      case sortnames of [] => map SOME sortfcn_list
                                      | _ =>
		      let val expanded_sortnames = expandConj (con, sortnames)
                          val conj_sfcn = RO.conjSortFcn conjSN
			  fun conj_exp_sns sf' = map (fn SOME sf => SOME (conj_sfcn (sf, sf')))
						     expanded_sortnames
		      in
                        ListHacks.flatten (map conj_exp_sns sortfcn_list)
		      end
                      (* List.foldl (fn (sf', r) => (expC (con, sortnames, SOME sf')) @ r)
                                    []
                                    sortfcn_list *)
                )
