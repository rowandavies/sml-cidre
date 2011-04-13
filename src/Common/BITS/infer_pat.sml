    (****** Patterns (sort output) ******)
    (* Pattern should have a sort constraint at or near the top. *)
    (* If not, the default (ML) sort is used (needed for recursion). *)
    (* Returns a boolean for whether a constraint was found.   *)
    and infer_pat (C, pat, default_srt) errflag : (bool * Env.VarEnv * RO.Sort) Result =
      case pat of
        RG.TYPEDpat(i, pat, sort) =>
	  let 
	    val srt = ref_ty(C, sort, (true, false)) 
	    val (VE_list, out_patsort) = ref_pat_max(C, pat, SORTps srt)
	  in
	    case (VE_list, patSort_to_Sorts (C, out_patsort))
	      of ([VE], []) => (true, VE, srt)
	       | ([VE], _::_) => 
		 (false, VE, srt, OG.TYPEDpat(errorConv(i, REI.UNMATCHED srt), out_pat, out_sort))
	       | _ => case ref_pat_max(C, pat, SORTps srt) 
			of (_, VE::_, _, _) =>
			   (false, VE, srt, 
			    OG.TYPEDpat(errorConv(i, REI.MULTIPLE_BINDINGS), out_pat, out_sort))
			 | _ => impossible "infer_pat"  (* MLSort must match *)
	  end
      | RG.ATPATpat(i1, RG.LONGIDatpat(i2, RG.OP_OPT(longid, withOp))) => 
        (case (getPostElabTypeInfo i2)  of
           SOME (TypeInfo.VAR_PAT_INFO _) =>
              (case Env.TG.lookup (Env.TG_of_E (Env.E_of_C C)) (Ident.id_of_longid longid)  of
                 SOME sort =>
		  let 
		    val (srt, out_sort) = ref_ty(C, sort, (true, false)) 
		  in
		    (true, Env.singleVarVE(Ident.id_of_longid longid, RO.Sort_in_SortScheme srt), 
		     srt,
		     OG.ATPATpat(okConv i1, OG.LONGIDatpat(okConv i2, OG.OP_OPT(longid, withOp))))
		  end
	       | NONE =>
		   let val (_, VE::_, _) = ref_pat_max(C, pat, SORTps default_srt)
		   in  (false, VE, default_srt)  end)
	 | _ =>   (* not VAR_PAT_INFO *)
		   let val (VE::_, _) = ref_pat_max(C, pat, SORTps default_srt)
		   in  (false, VE, default_srt)  end
      | RG.ATPATpat(i1, RG.PARatpat(i2, pat)) =>  infer_pat(C, pat, default_srt)
      | _ => let val (VE::_, _) = ref_pat_max(C, pat, SORTps default_srt)
	     in  (false, VE, default_srt)  end
