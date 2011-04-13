
       fun expandConj (_, []) = Crash.impossible "RefinedEnvironment.expandConj(1)"
           | expandConj (con, sortname::sortnames) = 
           case lookupR (R, sortname)
             of NONE => Crash.impossible "RefinedEnvironments.expandConj(2)"
              | SOME RC => 
                (case lookupRC (RC, con)
                   of NONE => []                            (* con absent from datasort body. *)
                   of NONE => []
                      | SOME NONE =>                        (* con present and has no argument. *)
		        (case sortnames of [] => [NONE] 
 					 | _ => expandConj (con, sortnames))
                      | SOME (SOME sortfcn_list) =>               (* con present with argument. *)
		      case sortnames of [] => map SOME sortfcn_list
                                      | _ =>
		      let val expanded_sortnames = expandConj (con, sortnames)
                          val conj_sfcn = RO.conjSortFcn conjSN
			  fun conj_exp_sns sf' = map (fn SOME sf => SOME (conj_sfcn (sf, sf')))
						     expanded_sortnames
		      in
                        ListHacks.flatten (map conj_exp_sns sortfcn_list)
		      end)

