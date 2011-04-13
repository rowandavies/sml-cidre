(*$RefineErrorTraverse:
	TOPDEC_GRAMMAR DEC_GRAMMAR GRAMMAR_INFO SOURCE_INFO REFINE_ERROR_INFO
	REPORT CRASH REFINE_ERROR_TRAVERSE
*)

(* Topdec error traversal.  Essentially an exact copy of ErrorTraverse from Kit 1.? *)
(* This is no longer used: instead ErrorTraverse is reused.  *)


functor RefineErrorTraverse(
                     structure TopdecGrammar: TOPDEC_GRAMMAR
	             structure GrammarInfo: REFINE_INFO
			sharing type TopdecGrammar.info
			 	     = DecGrammar.info
				     = GrammarInfo.PostRefineGrammarInfo
		      structure SourceInfo: SOURCE_INFO
			sharing type SourceInfo.info = GrammarInfo.SourceInfo
		      structure ErrorInfo: REFINE_ERROR_INFO
			sharing type ErrorInfo.info 
			           = GrammarInfo.RefineErrorInfo
		      structure Report: REPORT
			sharing type SourceInfo.Report
				     = ErrorInfo.Report
				     = Report.Report
		      structure Crash: CRASH
		     ): REFINE_ERROR_TRAVERSE =
  struct
    open TopdecGrammar TopdecGrammar.DecGrammar

   (* Simple-minded first attempt: walk over a topdec, accumulating any
      error nodes we encounter. *)

    val ok = Report.null
    infix //
    val op // = Report.//

   (* Yup, side-effect time; this is the best way to take note when an
      error has been spotted. *)

    local
      val b = ref false
      val last_pos = ref (None : SourceInfo.info Option)
    in
      fun spot() = (b := true)
      fun spotted() = !b
      fun now_at pos = (last_pos := Some pos)
      fun last_known_pos() = !last_pos
      fun reset() = (b := false; last_pos := None)     
    end

    fun check i =
      let
	val si_opt = case GrammarInfo.getPostRefineSourceInfo i
		       of Some si => (now_at si; Some si)
		        | None => None
      in
        case GrammarInfo.getPostRefineErrorInfo i
	  of Some ei => 
	     let val rep = 
	       (case si_opt
		  of Some si =>
		       SourceInfo.report si
		       // ErrorInfo.reportInfo ei
		   | None =>
		       Report.line "(exact position not known)"
		       // (case last_known_pos()
			     of Some lk_si => SourceInfo.report lk_si
                              | None => Report.null)
		       // ErrorInfo.reportInfo ei
	       ) // Report.line ""
	     in      (* NON-EXHAUSTIVENESS is a warning *)
	       case ei of ErrorInfo.UNMATCHED _ => 
		          (Report.print (Report.line "WARNING:" // rep); Report.null)
	                | _ => (spot(); rep)
	     end
	   | None =>
	       Report.null
      end

    fun walk_opt _ None = ok
      | walk_opt walk (Some obj) = walk obj

    fun walk_IdInfoList list =
      case list
	of nil => ok
	 | WITH_INFO(i, _) :: rest => check i // walk_IdInfoList rest


    fun walk_Topdec topdec =
      case topdec
	of STRtopdec(i, strdec) =>
	     check i // walk_Strdec strdec

	 | SIGtopdec(i, sigdec) =>
	     check i // walk_Sigdec sigdec

	 | FUNtopdec(i, fundec) =>
	     check i // walk_Fundec fundec

   (* MODULES: *)
    and walk_Strdec strdec =
      case strdec
	of DECstrdec(i, dec) =>
	     check i // walk_Dec dec

	 | STRUCTUREstrdec(i, strbind) =>
	     check i // walk_Strbind strbind

	 | LOCALstrdec(i, strdec1, strdec2) =>
	     check i // walk_Strdec strdec1 // walk_Strdec strdec2

	 | EMPTYstrdec i =>
	     check i

	 | SEQstrdec(i, strdec1, strdec2) =>
	     check i // walk_Strdec strdec1 // walk_Strdec strdec2

    and walk_Strbind strbind =
      case strbind
	of STRBIND(i, _, sigexp_opt, strexp, strbind_opt) =>
	     check i
	     // walk_opt walk_Sigexp sigexp_opt
	     // walk_Strexp strexp
	     // walk_opt walk_Strbind strbind_opt

    and walk_Sigexp sigexp =
      case sigexp
	of SIGsigexp(i, spec) =>
	     check i // walk_Spec spec

	 | SIGIDsigexp(i, _) =>
	     check i

    and walk_Strexp strexp =
      case strexp
	of STRUCTstrexp(i, strdec) =>
	     check i // walk_Strdec strdec

	 | ASSUMEstrexp(i, sigexp) =>
	     check i // walk_Sigexp sigexp

	 | LONGSTRIDstrexp(i, _) =>
	     check i

	 | APPstrexp(i, _, strexp) =>
	     check i // walk_Strexp strexp

	 | LETstrexp(i, strdec, strexp) =>
	     check i // walk_Strdec strdec // walk_Strexp strexp

    and walk_Sigdec sigdec =
      case sigdec
	of SIGNATUREsigdec(i, sigbind) =>
	     check i // walk_Sigbind sigbind

	 | EMPTYsigdec i =>
	     check i

	 | SEQsigdec(i, sigdec1, sigdec2) =>
	     check i // walk_Sigdec sigdec1 // walk_Sigdec sigdec2

    and walk_Sigbind sigbind =
      case sigbind
	of SIGBIND(i, _, sigexp, sigbind_opt) =>
	     check i
	     // walk_Sigexp sigexp
	     // walk_opt walk_Sigbind sigbind_opt

    and walk_Fundec fundec =
       case fundec
	 of FUNCTORfundec(i, funbind) =>
	      check i // walk_Funbind funbind

	  | EMPTYfundec i =>
	      check i

	  | SEQfundec(i, fundec1, fundec2) =>
	      check i // walk_Fundec fundec1 // walk_Fundec fundec2

    and walk_Funbind funbind =
      case funbind
	of FUNBIND(i, _, _, sigexp, sigexp_opt, strexp, funbind_opt) =>
	     check i
	     // walk_Sigexp sigexp
	     // walk_opt walk_Sigexp sigexp_opt
	     // walk_Strexp strexp
	     // walk_opt walk_Funbind funbind_opt

    and walk_Spec spec =
      case spec
	of VALspec(i, valdesc) =>       check i // walk_Valdesc valdesc
	 | TYPEspec(i, typdesc) =>      check i // walk_Typdesc typdesc
	 | EQTYPEspec(i, typdesc) =>    check i // walk_Typdesc typdesc
	 | DATATYPEspec(i, datdesc) =>  check i // walk_Datdesc datdesc
	 | DATASORTspec(i, datsortdesc) =>  
		check i // walk_Datsortdesc datsortdesc
	 | EXCEPTIONspec(i, exdesc) =>  check i // walk_Exdesc exdesc
	 | STRUCTUREspec(i, strdesc) => check i // walk_Strdesc strdesc
	 | SHARINGspec(i, shareq) =>    check i // walk_Shareq shareq

	 | LOCALspec(i, spec1, spec2) =>
	     check i // walk_Spec spec1 // walk_Spec spec2

	 | OPENspec(i, list) =>    check i // walk_IdInfoList list
	 | INCLUDEspec(i, list) => check i // walk_IdInfoList list
	 | EMPTYspec i =>       check i

	 | SEQspec(i, spec1, spec2) =>
	     check i // walk_Spec spec1 // walk_Spec spec2

    and walk_Valdesc valdesc =
      case valdesc
	of VALDESC(i, _, ty, valdesc_opt) =>
	     check i // walk_Ty ty // walk_opt walk_Valdesc valdesc_opt

    and walk_Typdesc typdesc =
      case typdesc
	of TYPDESC(i, _, _, typdesc_opt) =>
	  check i // walk_opt walk_Typdesc typdesc_opt

    and walk_Datdesc datdesc =
      case datdesc
	of DATDESC(i, _, _, condesc, datdesc_opt) =>
	     check i
	     // walk_Condesc condesc
	     // walk_opt walk_Datdesc datdesc_opt

    and walk_Datsortdesc datsortdesc =
      case datsortdesc
	of DATSORTDESC(i, _, _, consortdesc, datsortdesc_opt) =>
	     check i
	     // walk_Consortdesc consortdesc
	     // walk_opt walk_Datsortdesc datsortdesc_opt

    and walk_Condesc condesc =
      case condesc
	of CONDESC(i, _, ty_opt, condesc_opt) =>
	     check i
	     // walk_opt walk_Ty ty_opt
	     // walk_opt walk_Condesc condesc_opt

    and walk_Consortdesc consortdesc =
      case consortdesc
	of CONSORTDESC(i, _, sort_opt, consortdesc_opt) =>
	     check i
	     // walk_opt walk_Sort sort_opt
	     // walk_opt walk_Consortdesc consortdesc_opt

    and walk_Exdesc exdesc =
      case exdesc
	of EXDESC(i, _, ty_opt, exdesc_opt) =>
	     check i
	     // walk_opt walk_Ty ty_opt
	     // walk_opt walk_Exdesc exdesc_opt

    and walk_Strdesc strdesc =
      case strdesc
	of STRDESC(i, _, sigexp, strdesc_opt) =>
	     check i
	     // walk_Sigexp sigexp
	     // walk_opt walk_Strdesc strdesc_opt

    and walk_Shareq shareq =
      case shareq
	of STRUCTUREshareq(i, list) => check i // walk_IdInfoList list
	 | TYPEshareq(i, list) =>      check i // walk_IdInfoList list

	 | ANDshareq(i, shareq1, shareq2) =>
	     check i // walk_Shareq shareq1 // walk_Shareq shareq2

   (* CORE: *)
    and walk_Dec dec =
      case dec
	of VALdec(i, valbind) =>
	     check i // walk_Valbind valbind

	 | UNRES_FUNdec _ =>
	     Crash.impossible "ErrorTraverse.walk_Dec(UNRES_FUN)"

	 | TYPEdec(i, typbind) =>
	     check i // walk_Typbind typbind

	 | SORTdec(i, srtbind) =>
	     check i // walk_Srtbind srtbind

	 | DATATYPEdec(i, datbind) =>
	     check i // walk_Datbind datbind

	 | DATASORTdec(i, datsortbind) =>
	     check i // walk_Datsortbind datsortbind

         | ABSTYPEdec(i, datbind, dec) =>
	     check i // walk_Datbind datbind // walk_Dec dec

	 | EXCEPTIONdec(i, exbind) =>
	     check i // walk_Exbind exbind

	 | LOCALdec(i, dec1, dec2) =>
	     check i // walk_Dec dec1 // walk_Dec dec2

         | OPENdec(i, list) =>
	     check i // walk_IdInfoList list

         | SEQdec(i, dec1, dec2) =>
	     check i // walk_Dec dec1 // walk_Dec dec2

	 | INFIXdec(i, _, _) =>  check i
	 | INFIXRdec(i, _, _) => check i
	 | NONFIXdec(i, _) =>    check i
	 | EMPTYdec i =>         check i

    and walk_Valbind valbind =
      case valbind
	of PLAINvalbind(i, pat, exp, valbind_opt) =>
	     check i
	     // walk_Pat pat
	     // walk_Exp exp
	     // walk_opt walk_Valbind valbind_opt

	 | RECvalbind(i, valbind) =>
	     check i // walk_Valbind valbind

    and walk_Typbind typbind =
      case typbind
	of TYPBIND(i, _, _, ty, typbind_opt) =>
	     check i // walk_Ty ty // walk_opt walk_Typbind typbind_opt

    and walk_Srtbind srtbind =
      case srtbind
	of SRTBIND(i, _, _, srt, srtbind_opt) =>
	     check i // walk_Sort srt // walk_opt walk_Srtbind srtbind_opt

    and walk_Datbind datbind =
      case datbind
	of DATBIND(i, _, _, conbind, datbind_opt) =>
	     check i
	     // walk_Conbind conbind
	     // walk_opt walk_Datbind datbind_opt

    and walk_Datsortbind datsortbind =
      case datsortbind
	of DATSORTBIND(i, _, _, consortbind, datsortbind_opt) =>
	     check i
	     // walk_Consortbind consortbind
	     // walk_opt walk_Datsortbind datsortbind_opt

    and walk_Conbind conbind =
      case conbind
	of CONBIND(i, _, ty_opt, conbind_opt) =>
	     check i
	     // walk_opt walk_Ty ty_opt
	     // walk_opt walk_Conbind conbind_opt

    and walk_Consortbind consortbind =
      case consortbind
	of CONSORTBIND(i, _, sort_opt, consortbind_opt) =>
	     check i
	     // walk_opt walk_Sort sort_opt
	     // walk_opt walk_Consortbind consortbind_opt

    and walk_Exbind exbind =
      case exbind
	of EXBIND(i, _, ty_opt, exbind_opt) =>
	     check i
	     // walk_opt walk_Ty ty_opt
	     // walk_opt walk_Exbind exbind_opt

	 | EXEQUAL(i, _, _, exbind_opt) =>
	     check i // walk_opt walk_Exbind exbind_opt

    and walk_Pat pat =
      case pat
	of ATPATpat(i, atpat) =>
	     check i // walk_Atpat atpat

	 | CONSpat(i, _, atpat) =>
	     check i // walk_Atpat atpat

	 | TYPEDpat(i, pat, ty) =>
	     check i // walk_Pat pat // walk_Ty ty

	 | SORTEDpat(i, pat, sort) =>
	     check i // walk_Pat pat // walk_Sort sort

	 | LAYEREDpat(i, _, ty_opt, pat) =>
	     check i // walk_opt walk_Ty ty_opt // walk_Pat pat

	 | UNRES_INFIXpat _ =>
	     Crash.impossible "ErrorTraverse.walk_Pat(UNRES_INFIX)"

    and walk_Atpat atpat =
      case atpat
	of WILDCARDatpat i =>   check i
	 | SCONatpat(i, _) =>   check i
	 | LONGIDatpat(i, _) => check i

	 | RECORDatpat(i, patrow_opt) =>
	     check i // walk_opt walk_Patrow patrow_opt

	 | PARatpat(i, pat) =>
	     check i // walk_Pat pat

    and walk_Patrow patrow =
      case patrow
	of DOTDOTDOT i =>
	     check i

	 | PATROW(i, _, pat, patrow_opt) =>
	     check i // walk_Pat pat // walk_opt walk_Patrow patrow_opt

    and walk_Exp exp =
      case exp
	of ATEXPexp(i, atexp) =>
	     check i // walk_Atexp atexp

	 | APPexp(i, exp, atexp) =>
	     check i // walk_Exp exp // walk_Atexp atexp

	 | TYPEDexp(i, exp, ty) =>
	     check i // walk_Exp exp // walk_Ty ty

	 | SORTEDexp(i, exp, sort) =>
	     check i // walk_Exp exp // walk_Sort sort

	 | HANDLEexp(i, exp, match) =>
	     check i // walk_Exp exp // walk_Match match

	 | RAISEexp(i, exp) =>
	     check i // walk_Exp exp

	 | FNexp(i, match) =>
	     check i // walk_Match match

	 | UNRES_INFIXexp _ =>
	     Crash.impossible "ErrorTraverse.walk_Exp(UNRES_INFIX)"

    and walk_Atexp atexp =
      case atexp
	of SCONatexp(i, _) =>  check i
	 | IDENTatexp(i, _) => check i

	 | RECORDatexp(i, exprow_opt) =>
	     check i // walk_opt walk_Exprow exprow_opt

	 | LETatexp(i, dec, exp) =>
	     check i // walk_Dec dec // walk_Exp exp

	 | PARatexp(i, exp) =>
	     check i // walk_Exp exp

    and walk_Exprow exprow =
      case exprow
	of EXPROW(i, _, exp, exprow_opt) =>
	     check i // walk_Exp exp // walk_opt walk_Exprow exprow_opt

    and walk_Match match =
      case match
	of MATCH(i, mrule, match_opt) =>
	     check i // walk_Mrule mrule // walk_opt walk_Match match_opt

    and walk_Mrule mrule =
      case mrule
	of MRULE(i, pat, exp) =>
	     check i // walk_Pat pat // walk_Exp exp

    and walk_Ty ty =
      case ty
	of TYVARty(i, _) =>
	     check i

	 | RECORDty(i, tyrow_opt) =>
	     check i // walk_opt walk_Tyrow tyrow_opt

	 | CONty(i, tys, _) =>
	     check i // List.foldR (fn a => fn b => walk_Ty a // b) ok tys

         | FNty(i, ty1, ty2) =>
	     check i // walk_Ty ty1 // walk_Ty ty2

	 | PARty(i, ty) =>
	     check i // walk_Ty ty

    and walk_Tyrow tyrow =
      case tyrow
	of TYROW(i, _, ty, tyrow_opt) =>
	     check i // walk_Ty ty // walk_opt walk_Tyrow tyrow_opt

    and walk_Sort sort =
      case sort
	of TYVARsort(i, _) =>
	     check i

	 | RECORDsort(i, sortrow_opt) =>
	     check i // walk_opt walk_Sortrow sortrow_opt

	 | CONsort(i, sorts, _) =>
	     check i // List.foldR (fn a => fn b => walk_Sort a // b) ok sorts

         | FNsort(i, sort1, sort2) =>
	     check i // walk_Sort sort1 // walk_Sort sort2

	 | PARsort(i, sort) =>
	     check i // walk_Sort sort

	 | CNJsort(i, sort1, sort2) =>
	     check i // walk_Sort sort1 // walk_Sort sort2

    and walk_Sortrow sortrow =
      case sortrow
	of SORTROW(i, _, sort, sortrow_opt) =>
	     check i // walk_Sort sort // walk_opt walk_Sortrow sortrow_opt


    type Report = Report.Report

    datatype result = SUCCESS
      		    | FAILURE of Report

    fun traverse topdec =
      let
	val _ = reset()
	val report = walk_Topdec topdec
      in
	case spotted()
	  of true => FAILURE report
	   | false => SUCCESS
      end
  end;
