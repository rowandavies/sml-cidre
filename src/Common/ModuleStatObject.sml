
functor ModuleStatObject(structure StrId  : STRID
			 structure SigId  : SIGID
			 structure FunId  : FUNID
			 structure TyName : TYNAME
			 structure Name   : NAME
			   where type name = TyName.name
			 structure TyCon : TYCON
			   where type strid = StrId.strid
			   where type tycon = TyName.tycon

			 structure StatObject : STATOBJECT
                where type tycon = TyName.tycon
                where type TyName.name = TyName.name
			    where type TyName = TyName.TyName

             structure Environments : ENVIRONMENTS
                where type TyName = TyName.TyName
                where type tycon = StatObject.tycon
                where type TyName.Set.Set = StatObject.TyName.Set.Set
                where type TypeFcn    = StatObject.TypeFcn
                where type Type       = StatObject.Type
                where type TyVar      = StatObject.TyVar
                where type TypeScheme = StatObject.TypeScheme
                where type strid      = StrId.strid
                where type realisation = StatObject.realisation
                where type ExplicitTyVar = StatObject.ExplicitTyVar

             structure RefObject : REFOBJECT
                where type trealisation = StatObject.realisation
                where type TypeFcn = StatObject.TypeFcn
                where type ExplicitTyVar.SyntaxTyVar = StatObject.ExplicitTyVar
                where type Type = StatObject.Type
                where type TyVar = StatObject.TyVar

             structure RefinedEnvironments : REFINED_ENVIRONMENTS
                where type strid = StrId.strid
                where type SortVar = RefObject.SortVar
                where type SortName    = RefObject.SortName
                where type Rea.trealisation 
                                        = StatObject.realisation
                where type TyName = TyName.TyName
                where type SortFcn = RefObject.SortFcn
                where type tycon = TyCon.tycon
                where type Variance = RefObject.ExplicitTyVar.Variance
                where type Sort = RefObject.Sort
                where type SortScheme = RefObject.SortScheme
                where type id = Environments.id

			 structure PP : PRETTYPRINT

			 structure Flags : FLAGS
			) : MODULE_STATOBJECT =
  struct

    (*import from StatObject:*)
    type realisation       = StatObject.realisation
    type TyName            = StatObject.TyName.TyName
    type TyVar             = StatObject.TyVar
    type Type              = StatObject.Type
    type TypeScheme        = StatObject.TypeScheme
    type TypeFcn           = StatObject.TypeFcn
    structure TyVar        = StatObject.TyVar
    structure TyName       = StatObject.TyName
    structure Type         = StatObject.Type
    structure TypeScheme   = StatObject.TypeScheme
    structure TypeFcn      = StatObject.TypeFcn   
	    
    (*import from Environments:*)
    type id                = Environments.id
    type Env               = Environments.Env
    structure VE           = Environments.VE
    structure TyStr        = Environments.TyStr
    structure TE           = Environments.TE
    structure SE           = Environments.SE
    structure E            = Environments.E
    structure C            = Environments.C
    structure Realisation  = Environments.Realisation

    structure Env = Environments
    structure rEnv = RefinedEnvironments
    type rEnv = rEnv.Env
    type rT = rEnv.TyNameEnv

    structure rRea = rEnv.Rea  (* refinement realisations *)
    type rRea = rEnv.realisation

    structure SortName = rEnv.SortName

    (*import from other modules:*)
    structure RO = RefObject
    type SortScheme = RO.SortScheme
    type Sort = RO.Sort

    type tycon             = TyCon.tycon
    type strid             = StrId.strid
    type longstrid         = StrId.longstrid
    type longtycon         = TyCon.longtycon

    fun E_eq a = E.eq a         (* for profiling *)
    fun E_match a = E.match a
    fun mark_names T = app (Name.mark_gen o TyName.name) T
    fun unmark_names T = app (Name.unmark_gen o TyName.name) T

    val DEBUG = ref false (* true *)
    val _ = Flags.add_flag ("DEBUG_MODULE_STATOBJECT", "Debug ModuleStatObjects?", DEBUG)

    val doRename = ref false (* Renaming was done in the ML Kit, but causes trouble with sorts. *)
    val _ = Flags.add_flag ("doRename", "Do renaming when matching signatures?", doRename)

    fun found_error() = Flags.lookup_flag_entry "elaboration_error_found"

    (* This is used to avoid printing debugging info during basis stub processing. *)
    fun report_file_sig () = Flags.is_on "report_file_sig"

    fun do_maybe f x = () (* f x *)
    fun debug_push str = do_maybe rEnv.debug_push str
    fun debug_push_must str = do_maybe rEnv.debug_push_must str
    fun debug_pop str = do_maybe rEnv.debug_pop str
    fun debug_print_must str = 
          do_maybe (fn str => (debug_push_must str; debug_pop (fn () => []))) str
    fun assert str = do_maybe rEnv.assert str

    fun debug_layout string_tree : string list = 
	PP.lines_minipage (PP.format (120, (string_tree)))

    fun out_debug str = if !DEBUG andalso report_file_sig() then 
                           TextIO.output (TextIO.stdErr, str() ^"\n")
                        else ()
    fun prtree (t : StringTree.t) : unit =
           (* PP.outputTree (print, t, 100) *)
           (debug_push_must (fn () => debug_layout t); 
            debug_pop (fn () => []))

    fun prtree_debug (t : unit -> StringTree.t) : unit =
          if !DEBUG andalso report_file_sig() then
            prtree (t())
          else ()


      
    (*Plan of this code: first two functions that are used for matching
     both signatures and functor signatures, viz. sigMatchRea and
     check_enrichment.  Then the structure Sigma and Phi.
     (TODO 02/02/1997 23:17. tho.   well, sigMatchRea ought'a be inside
     structure Sigma, as it uses datatype Sig.)*)

    (***** signature matching definition page 35 sec. 5.12 *****)
    (*sigMatchRea is used by both Sigma.match and Phi.match below.*)

    datatype Sig = SIGMA of {T : TyName.Set.Set, E : Env, rT : rT, rE : rEnv}

    fun rTdomContainsT rT T str () =  (* An assertion for Sig, for use with assert. *)
	  let val Rdom = TyName.Set.fromList (rEnv.Tdom rT)
	      fun prT T = PP.flatten1 (Env.layoutT T)
	  in if TyName.Set.isEmpty (TyName.Set.difference T Rdom) then
		 NONE
	     else (Flags.lookup_flag_entry "print_type_name_stamps" := true;
		   SOME [str ^ ": T=" ^ prT T ^ " \tdom(rT)=" ^ prT Rdom] )
	  end

    (* For use with assert. *)
    fun eqTypes str ty1 ty2 () =  (* Use strings, since Type.eq is tied to unification. *)
	let val flag = Flags.lookup_flag_entry "print_type_name_stamps"
            val keep = !flag
	    val () = flag := true
	    val str1 = PP.flatten1 (Type.layout ty1)
            val str2 = PP.flatten1 (Type.layout ty2)
	    val () = flag := false
	in
  	  if str1 = str2 (* Type.eq(ty1, ty2) *) then NONE
  	  else (Flags.lookup_flag_entry "print_type_name_stamps" := true;
 	        SOME [str ^ "types not equal: \n" ^ PP.flatten1 (Type.layout ty1) ^
		          "\nis not equal to: \n" ^ PP.flatten1 (Type.layout ty2)])
	end

    (* For use with assert. *)
    fun srt_refines str srt ty () = eqTypes str (RO.tyOfSort srt) ty ()
    fun ssch_refines str ssch tysch () = 
	let val (tvs, ty) = TypeScheme.to_TyVars_and_Type tysch
            val srts = map (RO.mkSortSortVar o RO.SVofTV) tvs
            val srt = RO.instance (ssch, srts)
            val ty2 = RO.tyOfSort srt
	in
	    eqTypes str ty ty2 ()
	end

    fun venv_refines str rVE VE () = 
	(VE.apply (fn (id, VErng) => 
		     case (VErng, rEnv.lookupVE (rVE, id)) of
			 (VE.LONGVAR tysch, SOME (rEnv.LONGVAR ssch)) => 
  			        assert (ssch_refines str ssch tysch)
		       | (VE.LONGCON tysch, SOME (rEnv.LONGCON ssch)) => 
  			        assert (ssch_refines str ssch tysch)
		       | (VE.LONGEXCON ty, SOME (rEnv.LONGEXCON srt)) => 
			        assert (srt_refines str srt ty)
		       | _ => assert (fn () => SOME [str, "VE components"])  )
	           VE;
	 NONE)
    fun strenv_refines str rSE SE () = 
        (SE.apply (fn (strid, E) => 
                       case rEnv.lookupSE (rSE, strid) of
			   SOME rE => assert (env_refines str rE E)
                            | _ => assert (fn () => SOME [str, "SE components"])  )
	          SE; 
	 NONE)
    and env_refines str rE E () = 
	  (assert (venv_refines str (rEnv.VE_of_E rE) (E.to_VE E));
           assert (strenv_refines str (rEnv.SE_of_E rE) (E.to_SE E));
	   NONE)

    datatype SigMatchError = 
      MISSINGSTR  of longstrid
    | MISSINGTYPE of longtycon
    | S_CONFLICTINGARITY of longtycon * (TyName * TypeFcn)
    | CONFLICTINGEQUALITY of longtycon * (TyName * TypeFcn)
    | MISSINGVAR of strid list * id
    | MISSINGEXC of strid list * id
    | S_RIGIDTYCLASH of longtycon
    | S_CONFLICTING_DOMCE of longtycon
    | NOTYENRICHMENT of {qualid: strid list * id, 
			 str_sigma : TypeScheme, str_vce: string,
			 sig_sigma : TypeScheme, sig_vce: string}
    | NOSORTENRICHMENT of {qualid: strid list * id,
			 str_sigma : SortScheme, str_vce: string,
			 sig_sigma : SortScheme, sig_vce: string}
    | EXCNOTEQUAL of strid list * id * (Type * Type)
    | INCOMPATIBLE_REALISATION of longtycon
    | SUBSORT  of Sort * Sort
    | VARIANCE  of tycon

    exception No_match of SigMatchError
    fun fail reason = raise No_match reason

    (* sigMatchRea will raise exception No_match if no realisation can be found. *)
    (* Use "check_enrichment" to check whether the realiser renames enough tynames *)
    fun sigMatchRea (SIGMA{T, E, rT, rE}, E') : realisation =
	let
	  fun implies(a,b) = (* a --> b *) not (a andalso (not b))
	  fun matchEnv (E, E', path) : realisation =   (*The path is for error messages.*)
              let val (SE, TE, _) = E.un E
                  val (SE', TE', _) = E.un E'
                  val phi = matchSE (SE, SE', path)
                  val TEnew = Realisation.on_TyEnv phi TE
              in
                Realisation.oo (matchTE (TEnew, TE', path), phi)
              end
	  and matchSE(SE, SE', path): realisation =
	    let
	      fun dostr (strid, E) phi : realisation =
		(case SE.lookup SE' strid of
		   SOME E' =>  let val Enew = Realisation.on_Env phi E
		               in  Realisation.oo (matchEnv (Enew, E', strid::path), phi)  end
		 | NONE =>  fail (MISSINGSTR (StrId.implode_longstrid(rev path, strid))))
	    in
	      SE.Fold dostr Realisation.Id SE 
	    end
	  and matchTE (TE, TE', path) : realisation =
	      let
	       fun do_ty (tycon, tystr) phi : realisation=
	      (case TE.lookup TE' tycon of
 	         SOME tystr' =>
                   (*** As Sigma is type explicit, Definition sec. 5.8 we know that for all
                    *** flexible type names t (type names bound by T of Sigma) there exist a tycon,
                    *** such that TE(tycon) = (t, VE) for some VE.  However, it is possible that
                    *** there exists a t' and tycon', s.t. TE(tycon') = (t', VE') for some VE'
                    *** _and_ t' is not flexible (not in T of Sigma).                          ***)
                    let val theta = TyStr.to_theta tystr
                    in
                      (case TypeFcn.to_TyName theta of
                         SOME t =>
                           let val theta' = TyStr.to_theta tystr'
                             fun err f =  (* f is CONFLICTINGEQUALITY or S_CONFLICTINGARITY *)
                                 fail (f (TyCon.implode_LongTyCon (rev path,tycon), (t,theta') ))
                           in
                             if TyName.Set.member t T then
                               (*** definition page 33 sec 5.6 Type Realisation ***)
                               if TyName.arity t = TypeFcn.arity theta'
                                 then
                                   if implies (TyName.equality t, TypeFcn.admits_equality theta')
                                     then Realisation.oo (phi, Realisation.singleton (t,theta'))
                                   else err CONFLICTINGEQUALITY
                               else   err S_CONFLICTINGARITY
                             else (* t is rigid *)
                               phi
                           end
		       | NONE =>  phi )
	            end
	       | NONE =>  fail (MISSINGTYPE (TyCon.implode_LongTyCon(rev path, tycon)))  )
	     in
	       TE.Fold do_ty Realisation.Id TE
	     end

	  fun elim_VEs_in_TE TE = 
	    TE.Fold (fn (tycon, tystr) => fn te =>
		     let val tystr' = TyStr.from_theta_and_VE(TyStr.to_theta tystr,VE.empty)
		     in TE.plus(TE.singleton(tycon,tystr'),te)
		     end) TE.empty TE 
	  fun elim_VEs_in_E E =
	    let val (SE, TE, _) = E.un E
	    in E.mk(SE.map elim_VEs_in_E SE, elim_VEs_in_TE TE, VE.empty)
	    end
	in (* the VE's are not needed for matching [But why remove them? - Rowan 05feb02] *)
	  matchEnv (elim_VEs_in_E E, elim_VEs_in_E E', [])
	end (* sigMatchRea *)


    (**** Enrichment : definition page 34 sec. 5.11 *)
    (*check_enrichment is used by both Sigma.match and Phi.match below.*)

    local
          fun kind (VE.LONGVAR _)   = "var  "
            | kind (VE.LONGCON _)   = "con  "
            | kind (VE.LONGEXCON _) = "excon"
          fun sigma(VE.LONGVAR sigm)   =  sigm
            | sigma(VE.LONGCON sigm)   =  sigm
            | sigma(VE.LONGEXCON tau)  = StatObject.TypeScheme.from_Type tau

	  fun enrichesE (E, E', path) : unit =   (*The path passed around is for error messages.*)
	      let val (SE,TE,VE) = E.un E
		  val (SE',TE',VE') = E.un E'
	      in
		  enrichesSE (SE,SE',path) ;
		  enrichesTE (TE,TE',path) ;
		  enrichesVE (VE,VE',path) 
	      end
	  and enrichesSE (SE, SE', path) : unit =
	        SE.apply
		  (fn (strid, S') =>
		         (case SE.lookup SE strid of
			    SOME S => enrichesE (S, S', strid::path)
			  | NONE => fail (MISSINGSTR
					  (StrId.implode_longstrid(rev path, strid)))))
		    SE'
	  and enrichesTE (TE, TE', path) : unit =
	        TE.apply
		  (fn (tycon, tystr') =>
		         (case TE.lookup TE tycon of
			    SOME tystr => enrichesTyStr (tystr, tystr', path, tycon)
			  | NONE => fail (MISSINGTYPE
					  (TyCon.implode_LongTyCon (rev path, tycon)))))
		     TE'
	  and enrichesTyStr (tystr, tystr', path, tycon) : unit =
	         (*the tycon, like the path, is for a potential error message*)
	        let val (theta, VE)  = TyStr.to_theta_and_VE tystr
		    val (theta',VE') = TyStr.to_theta_and_VE tystr'
		in
		  if not (TypeFcn.eq (theta, theta')) then
		    fail (S_RIGIDTYCLASH
			  (TyCon.implode_LongTyCon (rev path, tycon)))
		  else if not (VE.is_empty VE' orelse VE.eq (VE, VE'))
			 then fail (S_CONFLICTING_DOMCE
				    (TyCon.implode_LongTyCon (rev path, tycon)))
		       else ()
		end
	  and enrichesVE (VE, VE', path) : unit =
	        VE.apply
		  (fn (id, varenvrng') =>
		        (case VE.lookup VE id of
			   SOME varenvrng  =>
			     if enriches_sigma_is (varenvrng, varenvrng') then ()
			     else fail (NOTYENRICHMENT
                                        {qualid = (rev path, id),
                                         str_sigma = sigma varenvrng,
                                         str_vce  = kind  varenvrng,
                                         sig_sigma = sigma varenvrng',
                                         sig_vce  = kind  varenvrng'})
			 | NONE => fail (MISSINGVAR (rev path, id))))
		      VE'
          (*enriches_sigma_is: (sigma1,is1) enriches (sigma2,is2),
	   iff sigma1 generalises sigma2 and (is1 = is2 or is2 = v), §5.5, sml'96.
	   First the case that is1 = is2:*)
	  and enriches_sigma_is (VE.LONGVAR sigma, VE.LONGVAR sigma') =
	        TypeScheme.generalises_TypeScheme (sigma, sigma')
	    | enriches_sigma_is (VE.LONGCON sigma, VE.LONGCON sigma') =
		TypeScheme.generalises_TypeScheme (sigma, sigma')
	    | enriches_sigma_is (VE.LONGEXCON tau, VE.LONGEXCON tau') =
		Type.eq (tau, tau')
	                  (*Then the case that is2 = v:*)
	    | enriches_sigma_is (VE.LONGCON sigma, VE.LONGVAR sigma') =
	        TypeScheme.generalises_TypeScheme (sigma, sigma')
	    | enriches_sigma_is (VE.LONGEXCON tau, VE.LONGVAR sigma') =
		TypeScheme.generalises_TypeScheme
		      (TypeScheme.from_Type tau, sigma')
	    | enriches_sigma_is _ = false
      in
	fun check_enrichment (S,S') : unit = enrichesE (S, S', [])
      end (*local*)

    (* sigMatchRea_rE will raise exception No_match for some (but not all) errors.
       Result refines tphi.   *)
    fun sigMatchRea_rE (SIGMA{T, E, rT, rE}, rE', tphi: realisation) : rRea =
	let
	  fun matchEnv (rE, rE', path) : rRea =   (* The path is for error messages.*)
              let val (rSE, rTE, rRE, _, _) = rEnv.unEnv rE  
                  val (rSE', rTE', rRE', _, _) = rEnv.unEnv rE'
                  val rphi = matchSE (rSE, rSE', path)
              in     (* can ignore TE - each tycon has a refinement in RE *)
                rRea.plus (matchRE (rRE, rRE', path)) rphi
              end
	  and matchSE(rSE, rSE', path): rRea =
	    let
	      fun dostr ((strid, E), rphi) : rRea =
		(case rEnv.lookupSE (rSE', strid) of
		   SOME E' => rRea.plus (matchEnv (E, E', strid::path)) rphi
		 | NONE =>  fail (MISSINGSTR (StrId.implode_longstrid(rev path, strid))))
	    in
	      rEnv.SEFold dostr rRea.Id rSE
	    end
	  and matchRE (rRE, rRE', path) : rRea =
	      let                                 (* Below "found_error" is from ElabDec. *)
               val _ = out_debug (fn () => "At top of matchRE.")
	       fun do_sort ((tycon, sortfcn), rphi) : rRea =
                 (out_debug (fn () => "At top of do_sort.");
		  case rEnv.lookupRE (rRE', tycon) of
 	            SOME sortfcn' =>
                      (out_debug (fn () => "Before SortFcn_to_Sortname.");
                       case RO.SortFcn_to_SortName sortfcn  of
                         SOME sn =>
                          let val t = (SortName.tyname sn)  in  
                           if TyName.Set.member t T then
                             if SortName.arity sn = RO.arity_SortFcn sortfcn'  then
                               if TypeFcn.eq (Realisation.on_TyName tphi t, 
                                              RO.TypeFcn_of_SortFcn sortfcn')  then
                                  if (out_debug (fn () => "Before variance check\n");
                                      RO.varianceSortFcn sortfcn' =
                                      ListPair.map RO.ExplicitTyVar.join_variance
						     (SortName.variance sn, 
						      RO.varianceSortFcn sortfcn'))
				  then
                                      rRea.plus rphi (rRea.singleton (sn, sortfcn'))
				  else  fail (VARIANCE tycon)
                               else fail (INCOMPATIBLE_REALISATION
                                            (TyCon.implode_LongTyCon (rev path,tycon)) )
                             else fail (S_CONFLICTINGARITY 
                                          (TyCon.implode_LongTyCon (rev path,tycon), 
                                           (t, RO.TypeFcn_of_SortFcn sortfcn') ) )
                           else (out_debug (fn () => "t not in T");
                                 rphi)
                          end
		       | NONE => (out_debug (fn () => "NONE case for SortFcn_to_SortName");
		                  rphi) )
	       | NONE =>  if !(found_error())
                            then  rRea.Id (* Caused by an elaboration error. *)
                            else  fail (MISSINGTYPE (TyCon.implode_LongTyCon(rev path, tycon)))  )
	     in
	       rEnv.REFold do_sort rRea.Id rRE
	     end
	in
	  matchEnv (rE, rE', [])
	end (* sigMatchRea_rE *)

    fun check_enrichment_r rT rTfull phi rphi rE1 rE2 : unit = 
	if !(found_error()) then () else

	let   (* Check that the realizers of a RL satisfy the required inclusions. *)
          val () = debug_push (fn () => ["check_enrichment"])
          val rTplusfull = rEnv.T_plus_T (rTfull, rT)

          val conjSNfull = rEnv.conjSortNameT rTplusfull
	  fun sn_to_sfcn sn = rRea.on_SortName conjSNfull rphi sn
	  fun sns_to_phiconj_conjphi (sn1, sn2) =
	      let val (svars, srt1) = RO.applySortFcnVars (sn_to_sfcn sn1)
                  val srtvars = map RO.mkSortSortVar svars
		  val srt2 = RO.applySortFcn (sn_to_sfcn sn2, srtvars)
                  val conjphi = RO.mkSortConj (SOME o conjSNfull) (srt1, srt2)
                  val sn_conj = conjSNfull (sn1, sn2)
                  val phiconj = RO.applySortFcn (sn_to_sfcn sn_conj, srtvars)
                  val () = debug_print_must (fn () => ["\nsns_to_phiconj_conjphi"])
                  val () = prtree_debug (fn () => RO.layoutSort phiconj)
                  val () = prtree_debug (fn () => RO.layoutSort conjphi)
	      in  (phiconj, conjphi)
	      end

          fun sn_layout sn = PP.layout_list (fn sn2 => StringTree.LEAF (SortName.pr_SortName ("", sn2))) 
                                            (SortName.sortNameConjuncts sn)

	  fun checkSNs sn1 sn2 =
           (debug_push (fn () => ["checkSNs", SortName.pr_SortName_noprefix sn1] 
                                 @ debug_layout (sn_layout sn1)
                                 @ [SortName.pr_SortName_noprefix sn2]
                                 @ debug_layout (sn_layout sn2));
	    if not (SortName.eq (sn1, sn2)) then
                let val (srt_phiconj, srt_conjphi) = sns_to_phiconj_conjphi (sn1, sn2)
                in
                    if not (RO.subSort conjSNfull (srt_phiconj, srt_conjphi)) then
                        fail (SUBSORT (srt_phiconj, srt_conjphi))
                    else if not (RO.subSort conjSNfull (srt_conjphi, srt_phiconj)) then
                        fail (SUBSORT (srt_conjphi, srt_phiconj))
	            else ()
                end
            else ()
            ) before debug_pop (fn () => [])


	  fun check_t t = let val (SOME tystr) = rEnv.lookupT (rTplusfull, t)
			      val RL = rEnv.RL_of_TyStr tystr
			      val sns = rEnv.Rdom (rEnv.R_of_TyStr tystr)
			  in
			      app (fn sn1 => app (checkSNs sn1) sns) sns
			  end handle General.Match =>
				     print ("BUG: type name not found while checking "
                                            ^ "signature enrichment:" ^ TyName.pr_TyName t ^ "\n")
          val _ = out_debug (fn () => "\nrT = ")
          val _ = prtree_debug (fn () => rEnv.layoutT rT)
(*          val _ = out_debug (fn () => "\n\nrTfull = ")
          val _ = prtree_debug (fn () => rEnv.layoutT rTfull) *)
          val _ = out_debug (fn () => "\n\nrphi = ")
          val _ = prtree_debug (fn () => rEnv.Rea.layout rphi)


          val phiDomT = (Realisation.dom phi)
          val () = assert (rTdomContainsT rTplusfull phiDomT "check_enrichment_r")

  	  val () = TyName.Set.apply check_t phiDomT
           
          (*--------------------------------------------------------------------*)
          val conjSN = rEnv.conjSortNameT rTfull
          fun kind (rEnv.LONGVAR _)   = "var  "
            | kind (rEnv.LONGCON _)   = "con  "
            | kind (rEnv.LONGEXCON _) = "excon"
          fun sigma(rEnv.LONGVAR sigm)   =  sigm
            | sigma(rEnv.LONGCON sigm)   =  sigm
            | sigma(rEnv.LONGEXCON tau)  = RefObject.Sort_in_SortScheme tau

	  fun enrichesE (rE1, rE2, path) : unit = (*The path passed around is for error messages.*)
	      let val (rSE1, rTE1, rRE1, rVE1, _) = rEnv.unEnv rE1
		  val (rSE2, rTE2, rRE2, rVE2, _) = rEnv.unEnv rE2
	      in
		  enrichesSE (rSE1, rSE2, path) ;
		  enrichesTE (rTE1, rTE2, path) ;
		  enrichesRE (rRE1, rRE2, path) ;
		  enrichesVE (rVE1, rVE2, path) 
	      end
	  and enrichesSE (rSE1, rSE2, path) : unit =
	        rEnv.SEFold
		  (fn ((strid, rE2), ()) =>
		         (case rEnv.lookupSE (rSE1, strid) of
			    SOME rE1 => enrichesE (rE1, rE2, strid::path)
			  | NONE => fail (MISSINGSTR
					  (StrId.implode_longstrid(rev path, strid)))))
                    ()
		    rSE2
	  and enrichesTE (rTE1, rTE2, path) : unit =
	        rEnv.TEFold
		  (fn ((tycon, sortfcn2), ()) =>
		         (case rEnv.lookupTE (rTE1, tycon) of
			    SOME sortfcn1 => enrichesTyStr (sortfcn1, sortfcn2, path, tycon)
			  | NONE => fail (MISSINGTYPE
					  (TyCon.implode_LongTyCon (rev path, tycon)))))
                   ()
		   rTE2
	  and enrichesTyStr (sortfcn1, sortfcn2, path, tycon) : unit =
              if not (RO.equal_SortFcn conjSN (sortfcn1, sortfcn2)) then
		    fail (S_RIGIDTYCLASH
			  (TyCon.implode_LongTyCon (rev path, tycon)))
               else ()
              (* Also, somewhere need to check that all refinements of
                 a datatype are datasort declarations.  Perhaps during matching?  *)

	         (*the tycon, like the path, is for a potential error message*)
(*	        let val (theta, VE)  = TyStr.to_theta_and_VE tystr
		    val (theta',VE') = TyStr.to_theta_and_VE tystr'
		in
		  if not (TypeFcn.eq (theta, theta')) then
		    fail (S_RIGIDTYCLASH
			  (TyCon.implode_LongTyCon (rev path, tycon)))
		  else if not (VE.is_empty VE' orelse VE.eq (VE, VE'))
			 then fail (S_CONFLICTING_DOMCE
				    (TyCon.implode_LongTyCon (rev path, tycon)))
		       else ()
		end*)

	  and enrichesRE (rRE1, rRE2, path) : unit =
	        rEnv.REFold
		  (fn ((tycon, sortfcn2), ()) =>
		         (case rEnv.lookupRE (rRE1, tycon) of
			    SOME sortfcn1 => enrichesTyStr (sortfcn1, sortfcn2, path, tycon)
			  | NONE => fail (MISSINGTYPE
					  (TyCon.implode_LongTyCon (rev path, tycon)))))
                   ()
		   rRE2

	  and enrichesVE (rVE1, rVE2, path) : unit =
	        rEnv.VEFold
		  (fn ((id, varenvrng2), ()) =>
		        (case rEnv.lookupVE (rVE1, id) of
			   SOME varenvrng1  =>
			     if enriches_sigma_is (varenvrng1, varenvrng2) then ()
			     else fail (NOSORTENRICHMENT
                                        {qualid = (rev path, id),
                                         str_sigma = sigma varenvrng1,
                                         str_vce  = kind  varenvrng1,
                                         sig_sigma = sigma varenvrng2,
                                         sig_vce  = kind  varenvrng2})
			 | NONE => fail (MISSINGVAR (rev path, id))))
		  ()
		  rVE2
          and sortSchemeGeneralizes (ssch1, ssch2) =               
              RO.isInstance conjSN (#2 (RO.instance_vars ssch2)) ssch1

          (*enriches_sigma_is: (sigma1,is1) enriches (sigma2,is2),
	   iff sigma1 generalises sigma2 and (is1 = is2 or is2 = v), §5.5, sml'96.
	   First the case that is1 = is2:*)
	  and enriches_sigma_is (rEnv.LONGVAR sigma, rEnv.LONGVAR sigma') =
	        sortSchemeGeneralizes (sigma, sigma')
	    | enriches_sigma_is (rEnv.LONGCON sigma, rEnv.LONGCON sigma') =
		 sortSchemeGeneralizes (sigma, sigma')
	    | enriches_sigma_is (rEnv.LONGEXCON tau, rEnv.LONGEXCON tau') =
		RO.subSort (rEnv.conjSortNameT rTfull) (tau, tau')
	                  (*Then the case that is2 = v:*)
	    | enriches_sigma_is (rEnv.LONGCON sigma, rEnv.LONGVAR sigma') =
	         sortSchemeGeneralizes (sigma, sigma')
	    | enriches_sigma_is (rEnv.LONGEXCON tau, rEnv.LONGVAR sigma') =
		 sortSchemeGeneralizes (RO.Sort_in_SortScheme tau, sigma')
	    | enriches_sigma_is _ = false
	  
	in 
	    enrichesE (rE1, rE2, [])
            before debug_pop (fn () => [])
        end  (* End of check_enrichment_r *)


			    (*signature*)

    structure Sigma = struct
      val triv = SIGMA {T = TyName.Set.empty, E = E.empty, rT = rEnv.emptyT, rE = rEnv.emptyE}
      val bogus = triv
      fun to_T_E_rT_rE (SIGMA {T, E, rT, rE}) = (T, E, rT, rE) 
      fun from_T_E_rT_rE (T,E,rT,rE) = 
         (assert (rTdomContainsT rT T "from_T_E_rT_rE"); 
          assert (env_refines "from_T_E_rT_rE" rE E); 
	  SIGMA {T=T, E=E, rT=rT, rE=rE} )
      val to_E = #2 o to_T_E_rT_rE
      val to_rE = #4 o to_T_E_rT_rE
      val tyvars = E.tyvars o to_E
      val tyvars' = E.tyvars' o to_E
      fun tynames (SIGMA {T, E, rT, rE}) = TyName.Set.difference (E.tynames E) T

      fun flex_sortnames (SIGMA {T, E, rT, rE}) = 
          TyName.Set.fold (fn t => fn b => (rEnv.Rdom (rEnv.R_of_TyStr (case rEnv.lookupT (rT, t)
                                                                          of SOME ts => ts ))
                                            handle Match => [] ) @ b)  (* HACK! See below. - RMD *)
                          [] T
      (* FIX: check ElabTopdec.found_err to avoid crashing due to invalid sorts.  *)
      fun instance' rTfull (sigma as SIGMA {T, E, rT, rE}) : TyName.Set.Set * Env * rT * rEnv =
          let val () = assert (rTdomContainsT rT T "instance'")
	      val (T', phi) = Realisation.renaming' T
              fun tn_to_tn t = case TypeFcn.to_TyName (Realisation.on_TyName phi t) 
                                 of SOME tnew => tnew
              val sortnames = SortName.Set.fromList (flex_sortnames sigma)
              val rphi = rRea.renaming phi sortnames
              val conjSN = rEnv.conjSortNameT rTfull
              val rTout = rEnv.Tmap (fn (t, tystr) => (tn_to_tn t, tystr)) 
                                    (rRea.on_TyNameEnv conjSN rphi rT)
	      val Eout = Realisation.on_Env phi E
	      val rEout = rRea.on_Env conjSN rphi rE
	      val () = assert (rTdomContainsT rTout T' "instance':rTout")
              val () = assert (env_refines "instance':rEout" rEout Eout)
          in  (T', Eout, rTout, rEout)
          end

      fun instance rTfull sigma : Env * rT * rEnv =
          let val (_, E, rT, rE) = instance' rTfull sigma  
          in  (E, rT, rE)  end
(*
      fun on' (phi : realisation, Sigma as SIGMA {T, E, rT, rE}) : Sig * realisation =
	    (*        ^ renaming of bound names *)
	    let
	      (*only clashing bound names need to be renamed to preserve 
	       T intersection ( Supp phi union Yield phi) = EmptySet
	       realisations, Definition v4, page 33 sec. 5.7*)
              val _ = print "WARNING: function ModuleStatObject.on' does not handle sorts.\n"
	      val T_free = tynames Sigma
	      val T_free' = Realisation.on_TyName_set phi T_free
	      val T_clashes = TyName.Set.intersect T T_free'
	      val phi_rename = Realisation.renaming T_clashes
	      val phi_restricted = Realisation.restrict T_free phi
	      val T' = Realisation.on_TyName_set phi_rename T
	      val E' = Realisation.on_Env (Realisation.oo (phi_restricted, phi_rename)) E
	    in
	      (SIGMA {T = T', E = E', rT = rT, rE = rE}, phi_rename)  (* FIX - Rowan 14jan02 *)
	    end
*)
(*
      fun on (phi, Sigma : Sig) : Sig = 
	    if Realisation.is_Id phi then Sigma else
	      #1 (on' (phi, Sigma))
*)
      (* rename_Sig - unused unless flag set *)
      fun rename_Sig rTfull (sigma as SIGMA{T,E, rT,rE}) =
	let val phi = Realisation.renaming T
            val sortnames = SortName.Set.fromList (flex_sortnames sigma)
            val rphi = rRea.renaming phi sortnames
            val conjSN = rEnv.conjSortNameT rTfull
	in SIGMA{T=Realisation.on_TyName_set phi T,
		 E=Realisation.on_Env phi E,
                 rT = rRea.on_TyNameEnv conjSN rphi rT, 
		 rE = rRea.on_Env conjSN rphi rE}
	end

      fun layout (SIGMA {T, E, rT, rE}) =
	      let val Ttree = TyName.Set.layoutSet {start="(", sep=",", finish=")"} TyName.layout T
	      in StringTree.NODE {start="SIGMA{", finish="}", indent=0,
			  children=[Ttree, E.layout E, rEnv.layoutT rT, rEnv.layoutEnv rE], 
                          childsep=StringTree.NOSEP}
	      end

      fun match (rTfull, Sig as SIGMA {T, E, rT, rE}, E', rE') : Env * rEnv =
	(* E' matches Sigma if there exists E'' s.t. Sigma >= E'' and
	 * E' enriches E''. match will raise No_match if there is no match.
	 * otherwise match will return the match E''.*)
	    let val phi = sigMatchRea (Sig,E')
	        val E'' = Realisation.on_Env phi E
           	val _ = check_enrichment (E',E'') ;       (*check_enrichment will raise No_match,
			  			            if E' does not enrich E''*)
(*                val _ = prtree_debug (fn () => layout Sig) *)
                val _ = prtree_debug (fn () => rEnv.layoutEnv rE')
                val _ = prtree_debug (fn () => Realisation.layout phi)
                val rphi = sigMatchRea_rE (Sig, rE', phi)  (* phi is for refinement correctness *)
                val rE'' = rRea.on_Env (rEnv.conjSortNameT rTfull) rphi rE
                val () = assert (rTdomContainsT rT T "match")
                val _ = check_enrichment_r rT rTfull phi rphi rE' rE''
	    in
	      (E'', rE'')
	    end

      (* match' is used to implement opaque signature matching. We
       * return the `transparent env', E_t, the `opaque env', E_o, and
       * a realisation, phi, mapping abstract type names of the opaque
       * env into type functions, such that phi(E_o) = E_t. *)
      fun match' (rTfull, Sig, E', rE') : Env * TyName.Set.Set * Env * rT * rEnv * realisation =
	let val Sig = if !doRename then rename_Sig rTfull Sig else Sig   (* no need for this? - RD *)
            val SIGMA{T, E, rT, rE} = Sig
	    val phi = sigMatchRea (Sig,E')  (* sigMatchRea will raise No_match,
					     * if no realisation can be found. *)
	    val E'' = Realisation.on_Env phi E
  	    val _ = check_enrichment (E',E'') ;       (* check_enrichment will raise No_match,
			 		               * if E' does not enrich E''*)
            val _ = out_debug (fn () => "Before sigMatchRea_rE in match'")
(*            val _ = prtree_debug (fn () => layout Sig) *)
            val _ = prtree_debug (fn () => rEnv.layoutEnv rE')
            val _ = prtree_debug (fn () => Realisation.layout phi)
            val rphi = sigMatchRea_rE (Sig, rE', phi)  (* phi is for refinement correctness *)
            val rE'' = rRea.on_Env (rEnv.conjSortNameT rTfull) rphi rE
            val () = assert (rTdomContainsT rT T "match'")
            val _ = check_enrichment_r rT rTfull phi rphi rE' rE''
	in
	  (E'', T, E, rT, rE, phi)
	end

      (* assumption: NO tynames in Sig1 and Sig2 may be marked generative. *)
(*      fun eq (SIGMA{T=T1,E=E1, ...}, Sig2 as SIGMA{T,...}) = (* FIX - Rowan 14jan02 *)
	if TyName.Set.size T1 <> TyName.Set.size T then false
	else (* First rename bound names of Sig2 and then match E2 against E1 and
	      * then check for equality of E1 and E2. *)
	  let val SIGMA{T=T2,E=E2, ...} = rename_Sig Sig2
	      val T1 = TyName.Set.list T1
	      val T2 = TyName.Set.list T2
	      val _ = mark_names T1
	      val _ = mark_names T2
	      val _ = E_match(E2,E1)
	      val _ = unmark_names T1
	      val _ = unmark_names T2
	  in E_eq(E1,E2)
	  end
*)
		  
    end (*Sigma*)


                         (*functor signature - T'E' here should be "Sigma" *)
    datatype FunSig = FUNSIG of {T : TyName.Set.Set, E : Env, rT : rT, rE : rEnv, T'E' : Sig}

    structure Phi = struct
      fun from_T_E_rT_rE_Sigma (T, E, rT, rE, T'E') = 
         (assert (rTdomContainsT rT T "from_T_E_rT_rE_Sigma"); 
          assert (env_refines "from_T_E_rT_rE" rE E); 
          FUNSIG {T = T, E = E, rT = rT, rE = rE, T'E' = T'E'} )

      fun to_T_E_rT_rE_Sigma (FUNSIG {T, E, rT, rE, T'E'}) = (T, E, rT, rE, T'E')
      fun tynames (FUNSIG {T, E, T'E' = SIGMA {T = T', E = E', ...}, ...}) =
	    TyName.Set.union 
	      (TyName.Set.union (E.tynames E) T)
	      (TyName.Set.difference (E.tynames E') (TyName.Set.union T T'))
      (* sortnames? *)
      fun tyvars Phi = 
	    let val (_, E, _, _, Sig) = to_T_E_rT_rE_Sigma Phi
	    in
	      TyVar.unionTyVarSet (E.tyvars E, Sigma.tyvars Sig)
	    end
      fun tyvars' Phi = 
	    let val (_, E, _, _, Sig) = to_T_E_rT_rE_Sigma Phi
	    in
	      E.tyvars' E @ Sigma.tyvars' Sig
	    end
(*      fun on (phi, funsig' as FUNSIG {T, E, rT, rE, T'E'}) : FunSig =
	    if Realisation.is_Id phi then funsig' else
	      let val (SIGMA {T = T1, E = E1, rT = rT1, rE = rE1}, phi_rename) =
		        Sigma.on' (phi, SIGMA {T = T, E = E, rT = rT, rE = rE})  
		  val Sigma1' = Sigma.on (Realisation.oo (phi, phi_rename), T'E')
	      in
		FUNSIG {T = T1, E = E1, rT = rT1, rE = rE1, T'E' = Sigma1'}
	      end
*)
(* Removed this - no longer being used, so no point in adding sorts to it. *)
      (*take care, match will raise No_match if there is no match*)
(*      fun match (FUNSIG {T=T, E=E, rT=rT, rE=rE, 
                         T'E'=Sig' as SIGMA {T=T',E=E', rT=rT', rE=rE'}}, E0) : Sig  =
	    let val phi = sigMatchRea(SIGMA{T=T, E=E, rT=rT, rE=rE}, E0)
	                    (*sigMatchRea will raise No_match,
			     if no realisation can be found*)
	        val E'' = Realisation.on_Env phi E  (* need to realise rE - rowan 14jan02 *)
	    in
	      check_enrichment (E0,E'') ;       (*check_enrichment will raise No_match,
						 if E0 does not enrich E''*)
	      let val phi_rename = Realisation.renaming T' (* FIX: rename rT *)
	      in
		Sigma.on (Realisation.oo (phi_rename, phi), Sig')
	      end
	    end
*)

      (*take care, match_via will raise No_match if there is no match*)
      fun match_via (FUNSIG {T, E, rT, rE, T'E'=Sig2 as SIGMA {T=T2,E=E2,rT=rT2,rE=rE2}}, 
                     Earg, rTfull, rEarg ) 
              : Sig * realisation * realisation =
	    let val Sig = SIGMA {T=T,E=E,rT=rT,rE=rE}
		val () = assert (rTdomContainsT rT T "match_via")
                val phi = sigMatchRea(Sig, Earg)       (* sigMatchRea may raise No_match *)
	        val E3 = Realisation.on_Env phi E
	        val _ = check_enrichment (Earg,E3) ;   (* check_enrichment will raise No_match if
                                                          Earg does not enrich E3. *)
                val rphi = sigMatchRea_rE (Sig, rEarg, phi) (* phi is for refinement correctness *)
                val rE3 = rRea.on_Env (rEnv.conjSortNameT rTfull) rphi rE
                val _ = check_enrichment_r rT rTfull phi rphi rEarg rE3
              
		val phi_rename = Realisation.renaming T2
		val phi3 = Realisation.oo (phi_rename, phi)

		val Tout = Realisation.on_TyName_set phi3 T2
		val Eout = Realisation.on_Env phi3 E2

		(* Copied from instance' above. T'=> _, phi=>phi_rename, sigma=>Sig2, rT=>rT2?,
			  rphi=>rphi_rename *)
		val sortnames = SortName.Set.fromList (Sigma.flex_sortnames Sig2)
                (* val _ = print ("\nSortNames= " ^ Int.toString (SortName.Set.size sortnames) ^ "\n") *)
		val rphi_rename = rRea.renaming phi_rename sortnames
                (* val _ = print "\nPhi.match_via: rphi= \n"
                   val _ = prtree (rEnv.Rea.layout rphi_rename) *)

		val conjSN = rEnv.conjSortNameT rTfull
		val rphi3 = rRea.oo conjSN rphi_rename rphi

		fun tn_to_tn t = case TypeFcn.to_TyName (Realisation.on_TyName phi_rename t) 
				   of SOME tnew => tnew
		val rTout = rEnv.Tmap (fn (t, tystr) => (tn_to_tn t, tystr))
				      (rRea.on_TyNameEnv conjSN rphi3 rT2) (* WAS: rphi_rename *)
		val rEout = rRea.on_Env conjSN rphi3 rE2

	    in
		(SIGMA{T=Tout, E=Eout, rT=rTout, rE=rEout}, phi, phi_rename)  (* The elaborator *)
            end  (* must maintain the invariant (Supp(phi') U Yield(phi')) \cap (T of Sig') = 0 *)
	                                                      (* -- I believe it does -- Martin *)

      (* Used only by compilation manager, so sorts are not correct - Rowan *)
(*      local
	fun rename_FunSig (FUNSIG{T, E, rT, rE, T'E'}) =
	  let val phi = Realisation.renaming T
	  in FUNSIG{T=Realisation.on_TyName_set phi T,
		    E=Realisation.on_Env phi E,
                    rT=rT, rE=rE,
		    T'E'=Sigma.on(phi,T'E')}
	  end
      in
	fun eq(FUNSIG{T=T1,E=E1,T'E'=Sig1', ...}, funsig2 as FUNSIG{T,...}) = 
	  if TyName.Set.size T1 <> TyName.Set.size T then false
	  else 
	    let val FUNSIG{T=T2,E=E2,T'E'=Sig2', ...} = rename_FunSig funsig2
	        val T1 = TyName.Set.list T1
		val T2 = TyName.Set.list T2
		val _ = mark_names T1
		val _ = mark_names T2
		val _ = E_match(E2,E1)
		val _ = unmark_names T1
		val _ = unmark_names T2
	  in E_eq(E1,E2) andalso Sigma.eq(Sig1',Sig2')
	  end
      end
*)

      fun layout (FUNSIG{T, E, rT, rE, T'E'}) =
	    let
	      val Ttree = TyName.Set.layoutSet {start="(", sep=",", finish=")"} TyName.layout T
	      val body = StringTree.NODE {start="Phi(", finish=")", indent=1,
				  children=[E.layout E, rEnv.layoutT rT,
                                            rEnv.layoutEnv rE, Sigma.layout T'E'], 
				  childsep=StringTree.RIGHT ", "}
	    in 
	      StringTree.NODE {start="", finish="", indent=1,
		       children=[Ttree,body],
		       childsep=StringTree.NOSEP}
	    end
    end (*Phi*)

  end (*ModuleStatObject*)


