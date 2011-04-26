(* Top-level reporting: ties static and dynamic basis together, generates
   a report of bindings. *)

functor TopLevelReport(structure FunId: FUNID
		       structure SigId: SIGID
		       structure StrId: STRID
		       structure Ident: IDENT
                         where type strid = StrId.strid

		       structure InfixBasis: INFIX_BASIS

		       structure StatObject: STATOBJECT

		       structure Environments: ENVIRONMENTS
			 sharing type Environments.strid = StrId.strid
			 sharing type Environments.id = Ident.id
			 sharing type Environments.Type = StatObject.Type
			 sharing type Environments.TypeScheme
				      = StatObject.TypeScheme

		       structure RefObject: REFOBJECT
                         sharing type RefObject.TVNames = StatObject.TVNames

		       structure RefinedEnvironments: REFINED_ENVIRONMENTS
			 sharing type RefinedEnvironments.strid = StrId.strid
			     and type RefinedEnvironments.id = Ident.id
			     and type RefinedEnvironments.longid = Ident.longid
			     and type RefinedEnvironments.SortScheme
				      = RefObject.SortScheme
                             and type RefinedEnvironments.tycon = Environments.tycon
                             and type RefinedEnvironments.SortName = RefObject.SortName
                             and type RefinedEnvironments.SortFcn = RefObject.SortFcn
                             and type RefinedEnvironments.Type = RefObject.Type
                             and type RefinedEnvironments.Sort = RefObject.Sort

		       structure ModuleStatObject: MODULE_STATOBJECT
			 sharing type ModuleStatObject.Env = Environments.Env
                             and type ModuleStatObject.rEnv = RefinedEnvironments.Env
                             and type ModuleStatObject.rT = RefinedEnvironments.TyNameEnv


		       structure ModuleEnvironments: MODULE_ENVIRONMENTS
                         where type rEnv.SortScheme = RefObject.SortScheme
                         where type longid = Ident.longid
                         where type rContext = RefinedEnvironments.Context
                         where type rTyNameEnv = RefinedEnvironments.TyNameEnv
                         where type rEnv = RefinedEnvironments.Env
                         where type rEnv.SortName = RefObject.SortName
                         where type rEnv.Type = RefObject.Type
                         where type rEnv.Sort = RefObject.Sort
                         where type rEnv.SortFcn = RefObject.SortFcn
                         where type rEnv.Report = RefinedEnvironments.Report
                         where type tycon = RefinedEnvironments.tycon
			 sharing type ModuleEnvironments.sigid = SigId.sigid
			 sharing type ModuleEnvironments.funid = FunId.funid
			 sharing type ModuleEnvironments.strid = StrId.strid 
			 sharing type ModuleEnvironments.Sig
				      = ModuleStatObject.Sig
			 sharing type ModuleEnvironments.FunSig
				      = ModuleStatObject.FunSig
			 sharing type ModuleEnvironments.Env
 			              = Environments.Env
(*                         sharing ModuleEnvironments.rEnv = RefinedEnvironments *)

		       structure Report: REPORT
			 sharing type InfixBasis.Report
				      = Environments.Report
			   	      = ModuleEnvironments.Report
			              = RefinedEnvironments.Report 
				      = Report.Report

		       structure Crash: CRASH
		      ): TOP_LEVEL_REPORT =
  struct
    (* Local abbreviation *)
    structure rEnv = ModuleEnvironments.rEnv

    type ElabBasis = ModuleEnvironments.Basis
    type InfixBasis = InfixBasis.Basis
    type Report = Report.Report
    type RefBasis = rEnv.Basis

    (*import from Environments:*)
    structure VE           = Environments.VE
    structure TyStr        = Environments.TyStr
    structure TE           = Environments.TE
    structure SE           = Environments.SE
    structure E            = Environments.E
    structure C            = Environments.C

    (*import from ModuleEnvironments:*)
    structure G            = ModuleEnvironments.G
    structure F            = ModuleEnvironments.F
    structure B            = ModuleEnvironments.B

    type rContext = ModuleEnvironments.rContext

    (*import from ModuleStatObject:*)
    structure Sigma        = ModuleStatObject.Sigma
    structure Phi          = ModuleStatObject.Phi

    (*import from StatObject:*)
    structure TyVar        = StatObject.TyVar
    structure TyName       = StatObject.TyName
    structure Type         = StatObject.Type
    structure TypeScheme   = StatObject.TypeScheme
    structure Substitution = StatObject.Substitution
    structure TypeFcn      = StatObject.TypeFcn
    structure Realisation  = StatObject.Realisation

    structure RO = RefObject

    val // = Report.//     infix //


   (* We report a top-level environment (a Basis in fact) by traversing
      the static environment. For each item found, we print out a
      sensible dynamic value, looking in the dynamic environment for
      a representation if required. `report' can also be told to ignore
      the dynamic side of things. Note that Environments and
      ModuleEnvironments have a lot of reporting functions already, and
      can happily handle the cases where no dynamic information is needed.
      Oh: we deal with the infix basis here as well. *)


    val report_ve_absent_sort_warn = ref true (* false *)
(*    val _ = Flags.add_flag ("report_ve_absent_sort_warn",
                              "report_ve_absent_sort_warn?", report_ve_absent_sort_warn) *)

    val omit_default_sorts = ref true

    fun reportVE (render, pathR, VE:Environments.VarEnv, rC : rContext, bindings) =
      VE.report
	(fn (id, VE.LONGVAR sigma) => 
	    let
	      val SVnames = RefObject.newSVNames()
	      val TVnames = RefObject.TVNamesOfSVNames SVnames

	      val heading = "val "
			   ^ Ident.pr_id id
			   ^ (if bindings then " = " ^ render (pathR, id, sigma)
			      else "")
	      val tab = 1 (* String.size heading - 4 *)

	      val ts_str = 
		  TypeScheme.pretty_string TVnames sigma
	      val VERange_opt = 
		  rEnv.Lookup_longid (rC, Ident.implode_LongId([], id))
	      val ss_report =
		  case VERange_opt
		   of SOME (rEnv.LONGVAR sortScheme) =>
                      let  val ss_str = RefObject.pr_SortSchemePRETTY SVnames sortScheme
                      in   if (ss_str = ts_str) andalso !omit_default_sorts 
                           then  Report.null
                           else  Report.line (StringCvt.padRight #" " 
						  ((String.size heading) -1)
						  "(*[ "
					      ^ "<: " ^ ss_str ^ " ]*)")
                      end
		    | NONE => if !report_ve_absent_sort_warn then 
                                   Report.line "<Warning: no sort found for this variable.>" //
                                   (if not (!omit_default_sorts) then (* Not quite the right flag...*)
                                      RefinedEnvironments.reportLayoutC rC
				    else
                                      Report.null)
                              else Report.null
		    | _ => Crash.impossible "TopLevelReport.reportVE"
	    in
	      Report.line (heading ^ " : " ^ ts_str)
              // Report.indent (tab, ss_report)
	    end

          | (id, VE.LONGCON sigma) =>
	      Report.null 	(*We'll get the cons when we walk over
				 the TyStr's in the TE.*)
	  | (id, VE.LONGEXCON tau) =>
	      Report.line ("exception " ^ Ident.pr_id id
			   ^ (if Type.is_Exn tau then ""
			      else
				" of "
				^ (case Type.un_Arrow tau of
				     SOME (domTy, _) => Type.string domTy
				   | NONE => Crash.impossible "TopLevelReport.reportVE" ) ) ) ,
	VE)

    fun reportSig rTfull (sigid, Sig) =
      let
	val (_, E, rT, rE) = Sigma.to_T_E_rT_rE Sig
        val rTnew = rEnv.T_plus_T(rTfull, rT)
      in
	   Report.line ("signature " ^ SigId.pr_SigId sigid ^ " =")
	// Report.line "  sig"
	// Report.indent (4, reportEnvSTATIC (E, rEnv.mkC (rTnew, rE)))
	// Report.line "  end"
      end

   (* I can't explain how I print out functors; run the damn thing and see
      for yourself. *)

    and reportFunSig rTfull (funid, funsig') =
      let
	val (_, E, rT, rE, N'E') = Phi.to_T_E_rT_rE_Sigma funsig'
	val (_, E', rT', rE') = Sigma.to_T_E_rT_rE N'E'
        val rT2 = rEnv.T_plus_T(rTfull, rT)
        val rT3 = rEnv.T_plus_T(rT2, rT')

	val heading = "functor " ^ FunId.pr_FunId funid ^ "("
	val tab = String.size heading - 1
      in
	Report.decorate (heading, reportEnvSTATIC (E, rEnv.mkC (rT2, rE)))
	  // Report.indent (tab,    Report.line "): sig"
			    // Report.indent(5, reportEnvSTATIC (E', rEnv.mkC (rT3, rE')))
			    // Report.line "   end")
      end

    and rCsubstr(rC, strId) = (* project rE on strId.  Used in reportSE.  *)
        let val rT = rEnv.T_of_C rC
            val rE = rEnv.E_of_C rC
            val (rSE, _, _, _, _) = rEnv.unEnv rE
            val (SOME rEsubstr) = rEnv.lookupSE (rSE, strId)
        in
          rEnv.mkC (rT, rEsubstr)
        end

    and reportSE(render, pathR, SE, rC : rContext, bindings) =
      SE.report(
	fn (strId, E) =>          
	  Report.line("structure "
		      ^ StrId.pr_StrId strId
		      ^ (if bindings then " =" else " :")
		     )
	  // Report.line(if bindings then "  struct" else "  sig")
	  // Report.indent(4, reportEnv (render, strId :: pathR, E, rCsubstr(rC, strId), bindings))
	  // Report.line "  end",
	SE
      )

    and reportEnvSTATIC (E, rC : rContext) = 
          reportEnv (fn _ => Crash.impossible "TopLevelReport.reportEnvSTATIC",
		     [], E, rC, false)

    and reportEnv(render, pathR, env, rC : rContext, bindings) =
      let
	val (SE, TE, VE) = E.un env
	val (rSE, rTE, rRE, rVE, rEE) = rEnv.unEnv (rEnv.E_of_C rC)
	val T = rEnv.T_of_C rC

        val conjSN = rEnv.conjSortNameT T
        (* Remove elements in TE added for sorts.  Really, these should be separated
           into an RE during elaboration.  *)
        val nonSortTE = TE.Fold (fn (tycon, tystr) => fn newTE => 
 				   case rEnv.lookupRE (rRE, tycon) of
				       NONE => newTE (* Must be skipping sort checking. *)
				     | SOME sfcn =>
 			           let 
				       val (_, srt) = RO.applySortFcnVars sfcn
                                   in  if RO.equal_Sort conjSN (srt, (rEnv.ty_to_srt rC 
							               (RO.tyOfSort srt)))
					    handle _ => true
                                       then  TE.plus (newTE, TE.singleton (tycon, tystr))
                                       else  newTE
                                   end)
                                TE.empty
                                TE
       in
	   reportSE(render, pathR, SE, rC, bindings)
	   // TE.report {tyEnv=nonSortTE, bindings=bindings}
	   // rEnv.reportRE{sortEnv=rRE, T=T, bindings=bindings}
	   // rEnv.reportEE rEE
	   // reportVE(render, pathR, VE, rC, bindings)
      end

    fun reportStaticBasis(render, sb: ElabBasis, bindings: bool)
          : Report =
      let
	val funenv = B.to_F sb
	val sigenv = B.to_G sb
	val env = B.to_E sb
        val rC = B.to_rC sb
        val rTfull = rEnv.T_of_C rC
      in		(* Sigs first; looks better (though in fact SML's
			   top-level syntax is knobbled so they can't be
			   mixed). *)
	G.report (reportSig rTfull, sigenv)
	// F.report (reportFunSig rTfull, funenv)
	// reportEnv(render, nil, env, rC, bindings)
      end

    fun report{infB=ib,elabB=sb, bindings} =
      let
(*	val db =  (* Might be void (ELAB_ONLY) *) *)

	fun render(pathR, id, tyScheme) = ""
(*	  ValPrint.print(ValPrint.locate(db, rev pathR, id), tyScheme) *)
      in
	Report.decorate("> ", InfixBasis.reportBasis ib
			      // reportStaticBasis(render, sb, bindings)
		       )
      end
  end;
