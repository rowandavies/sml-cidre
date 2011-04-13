
(* This is some code that replaces the use of expandConj by
   intersectRC.  It runs slightly slower, but would be faster if we could
   memoize the calls to intersectRC.  But, doing so is hard because we
   improve the RCs. Perhaps we could tag each RC with a unique ID? *)

         fun expandConj (_, []) = Crash.impossible "RefinedEnvironment.expandConj(1)"
           | expandConj (con, sortname::sortnames) = 
           case lookupR (R, sortname)
             of NONE => Crash.impossible "RefinedEnvironments.expandConj(2)"
              | SOME RC => 
	       let fun findRC sn = case lookupR (R, sn) of SOME RC => RC
		   fun conjRC sns = ListHacks.foldl' (intersectRC conjSN) (map findRC sns)
		   val RC1 = conjRC sortnames1'
                   val RCs2= map conjRC sortnames_list2'
                   fun project con RC = case lookupRC (RC, con) 
					  of SOME NONE => [NONE]
					   | SOME (SOME sfs_opt) => map SOME sfs_opt
                                           | NONE => [] (* con absent from datasort body *)
	       in
		 foldl    (* Expand for each constructor to get sub-goals.  *)
		   (fn (con, (false, _)) => (false, assumptions)
		     | (con, (true, r_assumptions)) =>
		     let val sortoptRHS = ListHacks.flatten (map (project con) RCs2)
		     in
		       foldl (fn (sortoptLHS, (false, _)) => (false, assumptions)
			       | (sortoptLHS, (true, r_assumptions2)) => 
						  let val (res3, r_assumptions3) = 
							  subSortopt r_assumptions2 
								     (sortoptLHS, sortoptRHS)
						  in  (res3, r_assumptions3)
						  end 
			     )
			     (true, r_assumptions)
			     (project con RC1)
		     end)
		   (true, assumptions)  (* Result and assumptions *)
		   (RCdom RC1)  (* previously: (domCE CE) *)

	       end
