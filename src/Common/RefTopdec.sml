
functor RefTopdec (structure IG: TOPDEC_GRAMMAR
		   structure RefinedEnvironments: REFINED_ENVIRONMENTS
		     sharing type RefinedEnvironments.strid = IG.strid
		         sharing type RefinedEnvironments.longstrid = IG.longstrid
		   structure RefDec: REFDEC
		     sharing type RefDec.Context = RefinedEnvironments.Context
		         sharing type RefDec.TyNameEnv = RefinedEnvironments.TyNameEnv
		         sharing type RefDec.Env = RefinedEnvironments.Env
			 sharing type RefDec.PostElabDec = IG.dec
                         sharing type RefDec.PostElabTy = IG.ty
		         sharing type RefDec.Context = RefinedEnvironments.Context
		   structure StrId: STRID
		     sharing type StrId.strid = IG.strid
			 sharing type StrId.longstrid = IG.longstrid
		   structure SigId: SIGID
		     sharing type SigId.sigid = IG.sigid

		   structure RefineErrorInfo: REFINE_ERROR_INFO

		   structure BasicIO: BASIC_IO
		   structure PP : PRETTYPRINT
		   structure Report: REPORT
		     sharing type Report.Report= PP.Report
		   structure Crash: CRASH
                  ): REFTOPDEC  =
let
  open RefDec.Comp
in
  struct
    structure Comp = RefDec.Comp
    type Basis = RefinedEnvironments.Basis
    type Env = RefinedEnvironments.Env
    type TyNameEnv = RefinedEnvironments.TyNameEnv
    structure Env = RefinedEnvironments

    type PostElabTopdec  = IG.topdec

(*    infix B_plus_E   val op B_plus_E = Env.B_plus_E
      infix E_plus_E   val op E_plus_E = Env.E_plus_E
*)

    fun opt_ap f (B, SOME x) = SOME (f (B, x))
      | opt_ap f (B, NONE) = NONE

    (* val ref_strexp : Basis * IG.strdec -> TyNameEnv * Env.Str *)
    fun ref_strexp(B, strexp) errflag =
    case strexp of
      IG.STRUCTstrexp(i, strdec) =>
	letCV (ref_strdec (B, strdec))  (fn (T, E) =>
        (T, Env.mkStr(Env.freshStrName(), E))  ) errflag
      | IG.LONGSTRIDstrexp(i, longstrid) =>
          (case Env.Lookup_lstrid(B, longstrid)
	      of SOME str => noErr (Env.emptyT, str)
	       | NONE => Crash.impossible "RefTopdec.ref_strexp:LONGSTRID")
      | IG.APPstrexp(i, funid, strexp) =>
	  Crash.unimplemented "RefDec.ref_strexp:APP"
      | IG.LETstrexp(i, strdec, strexp) =>
	letC (ref_strdec (B, strdec))  (fn (T1, E1) =>
	letCV (ref_strexp (Env.B_plus_B (B, Env.E_in_B E1), strexp))  (fn (T2, Str2) =>
        (Env.T_plus_T (T1,T2), Str2)  )) errflag
      | IG.TRANSPARENT_CONSTRAINTstrexp(i, strexp, sigexp) =>
	  ref_strexp(B, strexp) errflag  (* ignore signatures *)
      | IG.OPAQUE_CONSTRAINTstrexp(i, strexp, sigexp) =>
	  ref_strexp(B, strexp) errflag  (* ignore signatures *)


    and ref_strdec (B: Basis, strdec: IG.strdec) errflag =
      case strdec
	of IG.DECstrdec(i, dec) =>
	    RefDec.ref_dec(Env.C_of_B B, dec) errflag
         | IG.STRUCTUREstrdec(i, strbind) =>
  	     letCV (ref_strbind(B, strbind))  (fn (T, SE) =>
             (T, Env.SE_in_E SE)  )  errflag
         | IG.LOCALstrdec(i, strdec1, strdec2) =>
	     letC (ref_strdec(B, strdec1)) (fn (T1, E1) =>
	     letCV (ref_strdec(Env.B_plus_C (B, Env.mkC(T1, E1)), strdec2)) 
             (fn (T2, E2) => (Env.T_plus_T (T1, T2), E2)  )) errflag
	 | IG.EMPTYstrdec i =>
	     noErr (Env.emptyT, Env.emptyE)
	 | IG.SEQstrdec(i, strdec1, strdec2) =>
	     letC (ref_strdec(B, strdec1))  (fn (T1, E1) =>
             letCV (ref_strdec(Env.B_plus_C (B, Env.mkC(T1, E1)), strdec2))  (fn (T2, E2) =>
             (Env.T_plus_T (T1, T2), Env.E_plus_E(E1, E2))  )) errflag

    and ref_strbind (B: Basis, strbind: IG.strbind) errflag =
      case strbind
	of IG.STRBIND(i, strid, strexp, strbind_opt) =>
	 letC (ref_strexp(B, strexp))  (fn (T1, S1) => fn errflag2 =>
	 case strbind_opt
	   of NONE => noErr (T1, Env.singleSE(strid, S1))
	    | SOME strbind2 => 
                  letCV (ref_strbind(B, strbind2)) (fn (T2, SE2) => 
                  (Env.T_plus_T(T1, T2), 
                   Env.SE_plus_SE(Env.singleSE(strid, S1), SE2))  ) errflag2
         ) errflag

    and ref_sigexp (B, IG.SIGsigexp(i, spec)) errflag = 
          (* ref_spec (B, spec) *)     (* ignore signatures *)
           noErr ()
      | ref_sigexp (B, IG.SIGIDsigexp(i, sigid)) errflag = 
	  noErr ()

      | ref_sigexp (B, IG.WHERE_TYPEsigexp(i, sigexp, tyvar_list, longtycon, ty)) errflag =
        let val sort = RefDec.ref_ty_covar(Env.C_of_B B, ty) errflag
            val _ = ref_sigexp(B, sigexp) errflag
	in
            noErr ()
	end


    and ref_sigdec (B, IG.SIGNATUREsigdec(i, sigbind)) errflag =
          (ref_sigbind (B, sigbind); 
           noErr ())

    and ref_sigbind (B, IG.SIGBIND(i, sigid, sigexp, sigbind_opt)) errflag =
          (ref_sigexp(B, sigexp); 
           opt_ap ref_sigbind (B, sigbind_opt);
           noErr ())

   (* No point in going any further with this yet...  *)
   and ref_spec(B, _) = Crash.unimplemented "RefTopDec.ref_spec"

   and ref_fundec (B, _) = Crash.unimplemented "RefTopDec.ref_fundec"

   and ref_topdec_opt (B:Basis, NONE:IG.topdec option) errflag = 
	 noErr Env.emptyB
     | ref_topdec_opt (B, SOME topdec) errflag = 
	 ref_topdec(B, topdec) errflag

   and ref_topdec (B: Basis, topdec: IG.topdec) errflag =
     case topdec
       of IG.STRtopdec(i, strdec, topdec_opt) =>
	    let
	      val ((T, E), errs1) = ref_strdec(B, strdec) errflag
	      val B2 = Env.C_in_B (Env.mkC (T, E))
	      val (B3, errs3) = ref_topdec_opt(Env.B_plus_B(B, B2), topdec_opt) errflag
	    in
	      (Env.B_plus_B (B2, B3), errs3 @ errs1)
	    end
	| IG.SIGtopdec(i, sigdec, topdec_opt) =>
	  let     
	    val ((), errs1) = ref_sigdec(B, sigdec) errflag
	    val (B2, errs2) = ref_topdec_opt(B, topdec_opt) errflag
	  in
	    (B2, errs2 @ errs1)
	  end

	| IG.FUNtopdec(i, fundec, topdec_opt) =>
	  let     
	    val ((), errs1) = ref_fundec(B, fundec) errflag
	    val (B3, errs2) = ref_topdec_opt(B, topdec_opt) errflag
	  in
	    (B3, errs2 @ errs1)
	  end
	     
  end (* of struct *)
end;  (* of local open for functor RefTopdec *)
