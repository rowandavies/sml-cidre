(* Elaborator for Core Language Declarations*)

functor ElabDec(
               
               (* These were added to unify things that seemed to need it *)
                structure Ident : IDENT               

               (* These structures were already there *)
                structure FinMap : FINMAP
                structure Report: REPORT
                structure PP: PRETTYPRINT
                  where type Report = Report.Report

                structure StatObject : STATOBJECT
                  where type strid = Ident.strid (* yes? *)

                structure ParseInfo : PARSE_INFO

                structure ElabInfo : ELAB_INFO
                  where type ParseInfo.ParseInfo = ParseInfo.ParseInfo
                  where type ParseInfo.SourceInfo = ParseInfo.SourceInfo
                  where type ErrorInfo.Type  = StatObject.Type
                  where type ErrorInfo.TyVar = StatObject.TyVar
                  where type ErrorInfo.TypeScheme = StatObject.TypeScheme
                  where type OverloadingInfo.RecType = StatObject.RecType
                  where type OverloadingInfo.TyVar = StatObject.TyVar
                  where type TypeInfo.Type    = StatObject.Type
                  where type TypeInfo.TyVar   = StatObject.TyVar
                  where type TypeInfo.realisation = StatObject.realisation
                  where type ErrorInfo.lab = StatObject.lab
                  where type TypeInfo.TyName.TyName = StatObject.TyName.TyName
                  where type ErrorInfo.TyName = StatObject.TyName.TyName

                  where type TypeInfo.id = Ident.id
                  where type ErrorInfo.id = Ident.id

                  where type TypeInfo.longid = Ident.longid (* yes? *)
                  where type ErrorInfo.longid = Ident.longid (* yes? *)

                  where type TypeInfo.strid = Ident.strid (* yes? *)
                  where type ErrorInfo.strid = Ident.strid (* yes? *)
                  (* where type id       = ElabInfo.ErrorInfo.id *)


                structure IG : DEC_GRAMMAR
                  where type info = ParseInfo.ParseInfo

                  where type Lab.lab = ElabInfo.ErrorInfo.lab
                  where type Ident.id = Ident.id
                  where type Ident.longid = Ident.longid
                  where type Ident.strid = Ident.strid
                  where type StrId.longstrid = ElabInfo.ErrorInfo.longstrid
                  where type TyCon.tycon = ElabInfo.ErrorInfo.tycon
                  where type TyCon.longtycon = ElabInfo.ErrorInfo.longtycon
                  where type TyVar.SyntaxTyVar = StatObject.ExplicitTyVar
                  where type SCon.scon = StatObject.scon

                structure OG : DEC_GRAMMAR
                  where type info = ElabInfo.ElabInfo

                  where type Lab.lab = ElabInfo.ErrorInfo.lab
                  where type Ident.id = Ident.id
                  where type Ident.longid = Ident.longid
                  where type Ident.strid = Ident.strid
                  where type StrId.longstrid = ElabInfo.ErrorInfo.longstrid
                  where type TyCon.tycon = ElabInfo.ErrorInfo.tycon
                  where type TyCon.longtycon = ElabInfo.ErrorInfo.longtycon
                  where type TyVar.SyntaxTyVar = StatObject.ExplicitTyVar
                  where type SCon.scon = StatObject.scon
                  where type ty = IG.ty (* yes? *)

                structure Environments : ENVIRONMENTS
                  where type longid      = Ident.longid
                  where type longtycon   = IG.longtycon
                  where type longstrid   = IG.longstrid
                  where type strid       = IG.StrId.strid
                  (* where type strid    = ElabInfo.TypeInfo.strid *)
                  where type tycon       = IG.tycon
                  (* where type tycon    = ElabInfo.TypeInfo.tycon *)
                  where type valbind     = IG.valbind
                  where type id          = Ident.id
                  (* where type id       = ElabInfo.ErrorInfo.id *)
                  where type pat         = IG.pat
                  where type ExplicitTyVar = IG.tyvar
                  where type ExplicitTyVarEnv = ElabInfo.TypeInfo.ExplicitTyVarEnv 
                  where type ty          = IG.ty
                  (* where type TyName      = ElabInfo.ErrorInfo.TyName *)
                  (* where type TyName   = ElabInfo.TypeInfo.TyName *)
                  where type TyName   = StatObject.TyName.TyName
                  where type TyEnv       = ElabInfo.TypeInfo.TyEnv
                  where type TypeScheme  = StatObject.TypeScheme
                  where type Type        = StatObject.Type
                  where type TyVar       = StatObject.TyVar
                  where type TypeFcn     = StatObject.TypeFcn
                  where type realisation = StatObject.realisation
                  where type Substitution = StatObject.Substitution
                  where type level       = StatObject.level

(* sharing type StatObject.TyName.tycon = Environments.TyName.tycon *)

                  (* Already commented out *)
                  (* sharing StatObject.TyName = Environments.TyName *)


                structure Flags: FLAGS
                structure Crash: CRASH

               ) : ELABDEC =
  struct


    structure ListHacks =
      struct

        fun member x [] = false
          | member x (y::ys) = x=y orelse member x ys

        fun union(set1, set2) =
          set1 @ List.filter (fn x => not(member x set1)) set2

        fun intersect(set1, set2) =
          List.filter (fn x => member x set1) set2

        fun minus(set1, set2) =
          List.filter (fn x => not(member x set2)) set1

      end

    fun impossible s = Crash.impossible ("ElabDec." ^ s)
    fun noSome NONE s = impossible s
      | noSome (SOME x) s = x
    fun map_opt f (SOME x) = SOME (f x)
      | map_opt f NONE = NONE
    (*uniq [1,2,1,3] = [2,1,3]*)
    fun uniq [] = []
      | uniq (x::xs) = if List.exists (fn x' => x=x') xs then uniq xs
                       else x::uniq xs

    (*import from StatObject:*)
    structure Level        = StatObject.Level
    structure TyVar        = StatObject.TyVar
    structure TyName       = StatObject.TyName
         type Type         = StatObject.Type
         type TyVar        = StatObject.TyVar
         type RecType      = StatObject.RecType
         type TypeScheme   = StatObject.TypeScheme
    structure Type         = StatObject.Type
    structure TypeScheme   = StatObject.TypeScheme
         type Substitution = StatObject.Substitution
    structure Substitution = StatObject.Substitution
    structure TypeFcn      = StatObject.TypeFcn 
    structure Realisation  = StatObject.Realisation
            
    (*import from Environments:*)
    type VarEnv            = Environments.VarEnv
    type TyEnv             = Environments.TyEnv
    type StrEnv            = Environments.StrEnv
    type Env               = Environments.Env
    type Context           = Environments.Context
    type constructor_map   = Environments.constructor_map
    type TyGoals           = Environments.TyGoals
    structure VE           = Environments.VE
    structure TyStr        = Environments.TyStr
    structure TG           = Environments.TG
    structure TE           = Environments.TE
    structure SE           = Environments.SE
    structure E            = Environments.E
    structure C            = Environments.C
    structure constructor_map = Environments.constructor_map

    (*import from other modules:*)
    structure ErrorInfo = ElabInfo.ErrorInfo
    structure OverloadingInfo = ElabInfo.OverloadingInfo
    structure TypeInfo = ElabInfo.TypeInfo
    structure Ident = IG.Ident
    type ParseInfo  = ParseInfo.ParseInfo
    type ElabInfo = ElabInfo.ElabInfo
    type TyName = TyName.TyName


    (*info*)

    (*okConv ParseInfo = the ParseInfo converted to ElabInfo with
     empty Error and Type fields:*)

    val okConv = ElabInfo.from_ParseInfo

    val found_error = ref false   (* set when error found in ElabDec or ElabTopdec *)
    val _ = Flags.add_flag ("elaboration_error_found",
                             "Elaboration error found?", found_error)

    fun plus_ErrorInfo i e =
        (found_error := true; 
         ElabInfo.plus_ErrorInfo i e)        

    (*errorConv (ParseInfo, ErrorInfo) = the ParseInfo converted
     to ElabInfo with the error info ErrorInfo. The assumption
     (forced by this type conversion) is that there can only be
     one error tag per info:*)

    fun errorConv (i : ParseInfo, e : ErrorInfo.ErrorInfo) : ElabInfo =
          plus_ErrorInfo (okConv i) e

    (*preOverloadingConv: as okConv, except it includes some
     OverloadingInfo:*)

    fun preOverloadingConv (i : ParseInfo, oi : OverloadingInfo.OverloadingInfo)
          : ElabInfo = ElabInfo.plus_OverloadingInfo (okConv i) oi 

    fun lookupIdError (i : ParseInfo, longid : Ident.longid) : ElabInfo =
          errorConv (i, ErrorInfo.LOOKUP_LONGID longid)

    fun lookupTyConError (i : ParseInfo, longtycon : Environments.longtycon)
          : ElabInfo = errorConv (i, ErrorInfo.LOOKUP_LONGTYCON longtycon)

    fun repeatedIdsError (i : ParseInfo, rids: ErrorInfo.RepeatedId list)
          : ElabInfo = errorConv (i, ErrorInfo.REPEATED_IDS rids)

    (*infixes*)
    val on = Substitution.on     infixr on
    val onC = C.on     infixr onC
    val onVE = VE.on     infixr onVE
    infixr oo    val op oo = Substitution.oo

    fun C_plus_E a = C.plus_E a   infixr C_plus_E
    fun C_plus_TE a = C.plus_TE a  infixr C_plus_TE 
    fun C_plus_VE_and_TE a = C.plus_VE_and_TE a   infixr C_plus_VE_and_TE

    (*types needed for the signature ELABDEC*)
    type PreElabDec  = IG.dec  
    type PostElabDec = OG.dec
    type PreElabTy   = IG.ty
    type PostElabTy  = OG.ty
    type id = Ident.id

    type PreElabDatBind = IG.datbind
    type PostElabDatBind = OG.datbind

    fun pr (msg : string, t : StringTree.t) : unit =
          Report.print (Report.decorate (msg, PP.reportStringTree t))

    fun pr_st st = (PP.outputTree(print,st,100); print "\n")

    fun debug_pr_msg (msg: string): unit =
          if !Flags.DEBUG_ELABDEC then TextIO.output(TextIO.stdOut,msg) else ()

    fun getRepeatedElements equal ls =
          let
            fun NoOfOccurences x [] = 0
              | NoOfOccurences x (y::ys) = 
                  if equal (x, y) then 1 + NoOfOccurences x ys
                  else NoOfOccurences x ys
          in
            List.filter (fn e => (NoOfOccurences e ls) > 1) ls
          end
    fun isEmptyTyVarList x = case x of nil => true | _ => false
    fun memberTyVarList x xs = List.exists (fn y => TyVar.eq (x,y)) xs

    local
      fun count _ [] _ = NONE
        | count p (x::xs) n = if p x then SOME n
                              else count p xs (n + 1)
    in
      fun index p l = count p l 0
    end

    fun where' list elem =
      case index (fn a => a=elem) list 
        of SOME n => n
         | NONE => impossible "where'"

    (* Used by elab_ty0 to determine whether to allow intersections. *)
    val elab_ty_allow_intersect = ref false 

    (* Hooks needed by the compiler:

          o see signature TYPEINFO

     *)

    local open ElabInfo.TypeInfo
    in
     (* MEMO: no duplication checks here (or anywhere else!) *)
      fun addTypeInfo_CON (ElabInfo, C, instances, longid) =
            let
              val (_, con) = Ident.decompose longid
              val cons = C.lookup_fellow_constructors C longid
            in
              ElabInfo.plus_TypeInfo ElabInfo
                (CON_INFO {numCons=length cons,
                           index=where' cons con,instances=instances,
                           longid=longid})
            end

      fun addTypeInfo_EXCON (ElabInfo, tau, longid : Ident.longid) =
            (*The excon carries a value if the type tau is
             functional, and doesn't if it's `exn'.*)
            ElabInfo.plus_TypeInfo ElabInfo
              (EXCON_INFO {Type=tau, longid=longid})

      fun addTypeInfo_EXBIND (ElabInfo, typeOpt) = (*martin*)
            ElabInfo.plus_TypeInfo ElabInfo (EXBIND_INFO {TypeOpt=typeOpt})

      fun addTypeInfo_LAB (ElabInfo, index) =
            ElabInfo.plus_TypeInfo ElabInfo (LAB_INFO {index=index})

      fun addTypeInfo_RECORD_ATPAT (ElabInfo, Type) =
            ElabInfo.plus_TypeInfo ElabInfo (RECORD_ATPAT_INFO {Type=Type})

      fun addTypeInfo_EXP (ElabInfo, tau) =
            ElabInfo.plus_TypeInfo ElabInfo (EXP_INFO {Type=tau})

      fun addTypeInfo_MATCH (ElabInfo, tau) =
            ElabInfo.plus_TypeInfo ElabInfo (MATCH_INFO {Type=tau})
        
      fun addTypeInfo_VAR (ElabInfo, instances) =
            ElabInfo.plus_TypeInfo ElabInfo (VAR_INFO {instances=instances})

      fun addTypeInfo_VAR_PAT (ElabInfo, tau) =
            ElabInfo.plus_TypeInfo ElabInfo (VAR_PAT_INFO {tyvars=[], Type=tau})

(*
      fun addTypeInfo_VAR_PAT_2 (ElabInfo, tau) =
            ElabInfo.plus_TypeInfo ElabInfo (VAR_PAT_INFO {tyvars=[], Type=tau})
*)
      fun addTypeInfo_TYENV (ElabInfo, TE : TyEnv) =
            ElabInfo.plus_TypeInfo ElabInfo (TYENV_INFO TE)

      fun addTypeInfo_ABSTYPE (ElabInfo, (TE : TyEnv, rea : realisation)) =
            ElabInfo.plus_TypeInfo ElabInfo (ABSTYPE_INFO (TE,rea))

      fun addTypeInfo_PLAINvalbind (ElabInfo, tau) =
            ElabInfo.plus_TypeInfo ElabInfo
              (PLAINvalbind_INFO {Type=tau, tyvars=[], Uenv=C.U_empty})

      (* Copied from Kit version 1.5 *)
      fun addTypeInfo_DATBIND(datbind: OG.datbind,TE:Environments.TyEnv) =
	(* insert type names for the type constructors in the datatype binding info
	 * --- cannot be done locally (in elab_datbind) since the type names
	 * are changed when maximising equality for DATATYPEdec and ABSTYPE_dec !!
	 * The type name are also changed by the ABS function !!
	 * That is, allways be careful with adding type info to datatype bindings
	 * and sub-phrases of datatype bindings.
	 *)
	let
	  fun do_datbind (OG.DATBIND(i,tyvars,tycon,conbind,datbind_opt)) =
	    let 
	      open Environments
	      fun NoSome x =
		case x of
		  SOME y => y
		| NONE => Crash.impossible "ElabDec.addTypeInfo_DATBIND"
	      val t = (NoSome o StatObject.TypeFcn.to_TyName 
		       o TyStr.to_theta o NoSome o (TE.lookup TE)) tycon
(*	      val _ = pr("do_datbind: t = ",StatObject.layoutTyName t) *)
	    in
	      OG.DATBIND(ElabInfo.plus_TypeInfo i (DATBIND_INFO{TyName=t}),
		      tyvars,tycon,conbind,
		      case datbind_opt of 
			NONE => NONE
		      | SOME datbind => SOME (do_datbind datbind))
	    end
	in
	  do_datbind datbind
	end

    end


   (*Type_bogus () = when we get elaboration errors we often need to return a
    bogus type.  It's best if that's just a fresh type variable:*)

    val Type_bogus = Type.fresh_normal

    val allow_repeat = true and disallow_repeat = false

    fun Unify(tau, tau', i): Substitution * ElabInfo =
      case Type.unify (tau, tau')
        of Type.UnifyOk => (Substitution.Id, okConv i)  (* substitutions are dummies *)
         | Type.UnifyFail =>
             (Substitution.Id,
              errorConv(i, ErrorInfo.UNIFICATION(tau, tau'))
             )
         | Type.UnifyRankError(tv,tn) => 
             (Substitution.Id,
              errorConv(i, ErrorInfo.UNIFICATION_RANK(tau, tau', tv, tn))
             )

    fun UnifyWithTexts(text,tau,text', tau', i): Substitution * ElabInfo =
      case Type.unify(tau, tau')
        of Type.UnifyOk => (Substitution.Id, okConv i)
         | Type.UnifyFail =>
             (Substitution.Id,
              errorConv(i, ErrorInfo.UNIFICATION_TEXT(text,tau,text', tau'))
             )
         | Type.UnifyRankError(tv,tn) =>
             (Substitution.Id,
              errorConv(i, ErrorInfo.UNIFICATION_RANK(tau, tau', tv, tn))
             )

   (* Traversal of patterns to build a VE. We only do this for `rec'
      bindings. Arguably, we should reject records and so on right here,
      but the caller isn't set up to deal with that (and ignoring them
      would give unbound var errors for actual occuring vars), so we work for
      all patterns. We reject illegal pattern forms in the `rec' post-pass. *)

   (*MEMO: these should all be returning sets. *)

    (********
    Find the variables bound in a valbind
    *********
    There is no checking for multiply declared variables
    ********)

    fun dom_vb(C: Context, vb: IG.valbind): id list =
      case vb
        of IG.PLAINvalbind(_, pat, _, vb_opt) =>
             C.dom_pat (C, pat)
             @ (case vb_opt of
                  SOME vb => dom_vb (C, vb)
                | NONE => nil)

         | IG.RECvalbind(_, vb) =>
             dom_vb(C, vb)

    (*initial_TE datbind = the TE to be used initially in
     elaborating a datbind. We determine the correct equality
     attributes when we maximise equality:*)

    fun initial_TE (IG.DATBIND(_, explicittyvars, tycon, _, NONE)) =
          TE.init explicittyvars tycon
      | initial_TE (IG.DATBIND(_, explicittyvars, tycon, _, SOME datbind)) =
          TE.plus (TE.init explicittyvars tycon, initial_TE datbind)

    (* datasort_TE is determined by finding the tystr containing the first vconstr *)
    fun datasort_TE (C, IG.DATBIND(_, tyvars, tycon, conbind, datbind_opt)) =
        let
           val IG.CONBIND(_, IG.OP_OPT(long_id, _), _, _) = conbind
           val tystr = 
	     case C.lookup_longid C long_id
               of SOME (VE.LONGCON tysch) => 
                  let
		    val (_, ty) = TypeScheme.to_TyVars_and_Type tysch
                    val ty' = case Type.to_FunType ty
				of NONE => ty
                                 | SOME fty => 
                                   case Type.un_FunType fty
                                      of SOME(_,res_ty) => res_ty
                                       | NONE => impossible "datasort_TE(1)"

		    val ct = case Type.to_ConsType ty'
			       of SOME ct => ct
                                | NONE => impossible "datasort_TE(2)"
                             
                    val SOME (_, tyname) = Type.un_ConsType ct
                    val tycon = (TyName.tycon tyname)
                    val (strids, id) = Ident.decompose long_id

                    val longtycon = IG.TyCon.implode_LongTyCon (strids, tycon)
                    exception Restrict
		  in
                    case C.lookup_longtycon C longtycon  of
                         SOME tystr => tystr
		       | NONE =>  (* Caused by datatype replication.  Try via constructors. *)
			  let val other_ids = C.lookup_fellow_constructors C long_id
                              val SOME E = (E.lookup_strids (C.to_E C) strids)
			      val consVE = VE.restrict (E.to_VE E, id::other_ids) 
				           handle _ => raise Restrict
			  in
			      TyStr.from_theta_and_VE (TypeFcn.from_TyName tyname, consVE)
			  end handle Restrict => 
			      TyStr.from_theta_and_VE 
			      (TypeFcn.from_TyName 
			       (TyName.freshTyName
                                {tycon=tycon, arity=List.length tyvars, equality=false}), 
			       VE.bogus)
		  end

                | _ => TyStr.from_theta_and_VE 
		            (TypeFcn.from_TyName 
                             (TyName.freshTyName
                                {tycon=tycon, arity=List.length tyvars, equality=false}), 
                             VE.bogus)
	   val TE' = case datbind_opt of NONE => TE.empty 
                                       | SOME datbind => datasort_TE (C, datbind)
	in
           TE.plus (TE.singleton (tycon, tystr), TE')
	end


    (* addLabelIndexInfo: given a Type and a PATROW, populate the info
     * fields with the index of each label (needed by the
     * compiler). The index info is recoreded during overloading
     * resolvation.  The reason that the correct indexes are recorded
     * later is that only when the type is resolved (so that record
     * variables have been turned into record types without record
     * variables) do we know the correct index: consider fn {2 = x,
     * ...} => x; here we do not know the index of 2 until we have
     * found out what ... stands for (e.g., if it stands for { 1 = _ }
     * the index of 2 should be 1 but if it stands for { 3 = _ } the
     * index of 2 should be 0). *)

    fun addLabelIndexInfo(Type,patrow) =
      let
        val recType = noSome (Type.to_RecType Type) "addLabelIndexInfo"

        val labtys = Type.RecType.to_list recType
        val sortedLabs = map (#1) labtys
        fun f(OG.PATROW(i, lab, pat, patrow_opt)) =
          (case ElabInfo.to_ErrorInfo i 
             of NONE => 
               let val index = where' sortedLabs lab
               in OG.PATROW (ElabInfo.plus_TypeInfo i (TypeInfo.LAB_INFO {index=index}),
                             lab, pat, map_opt f patrow_opt)
               end
              | SOME _ => OG.PATROW(i, lab, pat, patrow_opt)
               )
          | f(OG.DOTDOTDOT i) = OG.DOTDOTDOT i
      in
        f patrow
      end


    (* Insert type information in valbind; the generic_tyvars are
     * those tyvars that were generalised when VE was closed (by
     * C.close). ME 1998-11-18 *)
   
    fun insert_type_info_in_valbind (generic_tyvars : TyVar list, 
                                     VE : VarEnv, Uenv : Environments.ExplicitTyVarEnv, 
                                     valbind: OG.valbind) : OG.valbind =
      let 
        fun lookup_id id = VE.lookup VE id

        fun do_valbind (vb : OG.valbind) : OG.valbind =
          case vb 
            of OG.PLAINvalbind(i, pat, exp, vb_opt) =>
              let val i' = case ElabInfo.to_TypeInfo i 
                             of SOME (TypeInfo.PLAINvalbind_INFO{tyvars=[],Type, Uenv=_}) =>
                               ElabInfo.plus_TypeInfo i 
                               (TypeInfo.PLAINvalbind_INFO{tyvars=generic_tyvars, Type=Type, Uenv=Uenv})
                              | _ => impossible "ElabDec.do_valbind: wrong type info"
                  val vb_opt' = case vb_opt
                                  of SOME vb => SOME(do_valbind vb)
                                   | NONE => NONE
              in OG.PLAINvalbind(i', do_pat pat, exp, vb_opt')
              end
             | OG.RECvalbind(i, vb) => OG.RECvalbind(i,do_valbind vb)

        and do_pat pat =
          case pat 
            of OG.ATPATpat(i, atpat) => OG.ATPATpat(i,do_atpat atpat)
             | OG.CONSpat(i, longid_op, atpat) => OG.CONSpat(i, longid_op, do_atpat atpat)
             | OG.TYPEDpat(i, pat, ty) => OG.TYPEDpat(i, do_pat pat, ty)
             | OG.SORTEDpat(i, pat, tys) => OG.SORTEDpat(i, do_pat pat, tys)
             | OG.LAYEREDpat(i, id_op as OG.OP_OPT(id, withOp),ty_opt, pat) =>
              let val i' =
                    case VE.lookup VE id
                      of SOME(VE.LONGVAR sigma) => 
                        let val (tyvars, Type) = TypeScheme.to_TyVars_and_Type sigma
                        in ElabInfo.plus_TypeInfo i (TypeInfo.VAR_PAT_INFO{tyvars=tyvars,Type=Type})
                        end
                       | _ => i
              in OG.LAYEREDpat(i', id_op, ty_opt, do_pat pat)
              end

             | OG.UNRES_INFIXpat _ => impossible "do_pat(UNRES_INFIX)"

        and do_atpat atpat =
          case atpat 
            of OG.WILDCARDatpat _ => atpat
             | OG.SCONatpat _ => atpat
             | OG.LONGIDatpat(i, longid_op as OG.OP_OPT(longid,withOp)) =>
              let val i' = 
                    case Ident.decompose longid
                      of ([],id) => 
                        (case VE.lookup VE id
                           of SOME(VE.LONGVAR sigma) => 
                             let val (tyvars, Type) = TypeScheme.to_TyVars_and_Type sigma
                             in ElabInfo.plus_TypeInfo i (TypeInfo.VAR_PAT_INFO{tyvars=tyvars,Type=Type})
                             end
                            | _ => i)
                       | _ => i
              in OG.LONGIDatpat(i', longid_op)
              end
             | OG.RECORDatpat(i, patrowOpt) => OG.RECORDatpat (i, map_opt do_patrow patrowOpt)
             | OG.PARatpat(i, pat) => OG.PARatpat(i, do_pat pat)

        and do_patrow patrow = 
          case patrow 
            of OG.DOTDOTDOT _ => patrow
             | OG.PATROW(i, l, pat, patrowOpt) => OG.PATROW(i, l, do_pat pat, map_opt do_patrow patrowOpt)
      in
        do_valbind valbind
      end
      
    
    (********************************************************)
    (*      Elaboration (type checking)                     *)
    (********************************************************)


    (****** atomic expressions ******)

    fun elab_atexp (C : Context, atexp : IG.atexp)
      : (Substitution * Type * OG.atexp) =

        case atexp of

          (* special constants *)                               (*rule 1*)
          IG.SCONatexp(i, scon) =>
            (* Some special constants are overloaded; thus, we must
             * record some overloading info in the case a special constant
             * can denote one of a set of type names. 
             *)
            let val {type_scon, overloading} = Type.of_scon scon
                val i_out =
                  case overloading
                    of NONE => okConv i
                     | SOME tv => preOverloadingConv (i, OverloadingInfo.UNRESOLVED_IDENT tv)
            in (Substitution.Id, type_scon, OG.SCONatexp (i_out, scon))
            end
            

          (* identifiers - variables or constructors *)         (*rule 2*)
        | IG.IDENTatexp(i, IG.OP_OPT(longid, withOp)) =>
            (case C.lookup_longid C longid of

               (* Variable *)                                   
              SOME(VE.LONGVAR sigma) =>
                 let val (instance, instances) = TypeScheme.instance' sigma
                      
                   (*if Type.overloaded_tyvars instances yields [], then there
                    are no overloaded tyvars in the type.  If there is exactly
                    one overloaded tyvar, longid may be a primitive
                    (e.g., + : 'a * 'a -> 'a, where 'a is overloaded), and this
                    tyvar must later be resolved.  If there is more than one
                    overloaded tyvar in the type, longid cannot be a primitive,
                    and the overloading info will never be needed (e.g.,
                    fun ~+ ((x,y),(v,w)) = (x+v,y+w))*)
                   val out_i =
                         addTypeInfo_VAR
                           ((case List.filter TyVar.is_overloaded (Type.tyvars instance) of
                               [tyvar] => preOverloadingConv
                                            (i, OverloadingInfo.UNRESOLVED_IDENT tyvar)
                             | _ => okConv i),
                            instances)
                 in  
                   (Substitution.Id, instance,
                    OG.IDENTatexp (out_i, OG.OP_OPT (longid, withOp)))
                 end

              (* Constructor *)
            | SOME(VE.LONGCON sigma) =>
                let val (tau,instances) = (TypeScheme.instance' sigma)
                in
                  (Substitution.Id, tau,
                   OG.IDENTatexp(addTypeInfo_CON(okConv i, C, instances, longid),
                                 OG.OP_OPT(longid, withOp)
                                 )
                  )
                end

             (* Exception constructor *)
           | SOME(VE.LONGEXCON tau) =>
                (Substitution.Id,
                 tau,
                 OG.IDENTatexp(addTypeInfo_EXCON(okConv i, tau, longid),
                               OG.OP_OPT(longid, withOp)
                               )
                 )

             (* Not found in current context *)
           | NONE =>
               (Substitution.Id, Type_bogus (),
                OG.IDENTatexp(lookupIdError (i, longid),
                              OG.OP_OPT(Ident.bogus, withOp)
                              )
                )
          )

          (* record expression *)                               (*rule 3*)
        | IG.RECORDatexp(i, NONE) =>
            (Substitution.Id, Type.Unit, OG.RECORDatexp(okConv i,NONE)) 

          (* record expression *)
        | IG.RECORDatexp(i, SOME exprow) =>
            let
              val (S, rho, out_exprow) = elab_exprow(C,exprow)
            in
              (S, Type.from_RecType rho,
               OG.RECORDatexp (okConv i, SOME out_exprow)) 
            end 

          (* let expression *)                                  (*rule 4*)
        | IG.LETatexp(i, dec, exp) => 
            let
              val (S1, T, E, out_dec)   = elab_dec(C, dec)
              val (S2, tau, out_exp) = elab_exp((S1 onC C) C_plus_E E, exp)
              val out_i = case TyName.Set.list 
                             (TyName.Set.intersect 
                              (Type.tynames tau) (TyName.Set.fromList T))
                             of [] => okConv i
                              | tynames => errorConv (i, ErrorInfo.DATATYPES_ESCAPE_SCOPE tynames)
            in
              (S2 oo S1, tau, OG.LETatexp (out_i, out_dec, out_exp))
            end

          (* parenthesised expression *)
        | IG.PARatexp(i, exp) =>                                (*rule 5*)
            let val (S, tau, out_exp) = elab_exp(C,exp)
            in (S, tau, OG.PARatexp(okConv i,out_exp)) end

    (******** expression rows ********)

    and elab_exprow (C : Context, exprow : IG.exprow) :
        (Substitution * RecType * OG.exprow) =

        case exprow of 

          (* Expression row *)                                  (*rule 6*)
          IG.EXPROW(i, lab, exp, NONE) =>
            let
              val (S, tau, out_exp) = elab_exp(C, exp)
              val rho = Type.RecType.add_field (lab,tau) Type.RecType.empty
            in
              (S, rho, OG.EXPROW(okConv i,lab,out_exp,NONE))
            end

          (* Expression row *)
        | IG.EXPROW(i, lab, exp, SOME exprow) =>
            let
              val (S1, tau, out_exp   ) = elab_exp(C, exp)
              val (S2, rho, out_exprow) = elab_exprow(S1 onC C,exprow)
            in
              if (ListHacks.member lab (Type.RecType.sorted_labs rho)) then
                (S2, rho, 
                 OG.EXPROW(repeatedIdsError(i, [ErrorInfo.LAB_RID lab]),
                           lab, out_exp, SOME out_exprow))
              else
                (S2 oo S1, Type.RecType.add_field (lab,S2 on tau) rho,
                 OG.EXPROW(okConv i,lab,out_exp,SOME out_exprow))
            end

    (******** expressions ********)

    and elab_exp(C : Context, exp : IG.exp) : 
        (Substitution * Type * OG.exp) =
      let
        val _ =
          if !Flags.DEBUG_ELABDEC then
            pr("elab_exp: ", IG.layoutExp exp)
          else ()

        val (S, ty, exp') = elab_exp'(C, exp)

        val _ =
          if !Flags.DEBUG_ELABDEC then
            pr("giving:   ", Type.layout ty)
          else ()
      in
        (S, ty, exp')
      end

    and elab_exp'(C, exp) =
      case exp

           (* Atomic expression *)                              (*rule 7*)
        of IG.ATEXPexp(i, atexp) =>
             let
               val (S, tau, out_atexp) = elab_atexp(C, atexp)
             in
               (S, tau, OG.ATEXPexp(addTypeInfo_EXP(okConv i, tau),out_atexp))
             end

           (* Application expression *)                         (*rule 8*)
         | IG.APPexp(i, exp, atexp) => 
             let
               val (S1, tau1, out_exp)   = elab_exp(C, exp)
               val (S2, tau2, out_atexp) = elab_atexp(S1 onC C, atexp)
               val new   = Type.fresh_normal ()
               val arrow = Type.mk_Arrow(tau2,new) 
               val (S3, i') = UnifyWithTexts("operand suggests operator type",arrow, 
                                             "but I found operator type",S2 on tau1, i)
               val tau = S3 on new
             in
               (S3 oo S2 oo S1, tau,
                OG.APPexp(addTypeInfo_EXP(i', tau), out_exp, out_atexp))
             end

           (* Typed expression *)                               (*rule 9*)
         | IG.TYPEDexp(i, exp, ty) =>
             let val (S1, tau, out_exp) = elab_exp(C, exp)
             in case elab_ty(S1 onC C, ty)
                  of (SOME tau', out_ty) =>
                    let val (S2, i') = UnifyWithTexts("type of expression",tau,"disagrees with your type constraint", tau', i)
                        val tau'' = S2 on tau'
                    in (S2 oo S1, tau'', OG.TYPEDexp(addTypeInfo_EXP(i',tau''), out_exp, out_ty))
                    end
                   | (NONE, out_ty) => (S1, tau, OG.TYPEDexp(okConv i, out_exp, out_ty))
             end

           (* Sorted expression *)                               (*rule 9*)
         | IG.SORTEDexp(i, exp, ty::tys) =>
             let val (S1, tau, out_exp) = elab_exp(C, exp)
                 val (tau_opt, out_ty) = elab_srt(S1 onC C, ty)
		 fun do_elab ty = elab_srt (S1 onC C, ty)
		 val pairs = map do_elab tys
		 val out_tys = map #2 pairs
		 val tau_opts = map #1 pairs
		 fun find_different (SOME tau1) (SOME tau2::tail) = 
 		     if Type.eq (tau1, tau2) then find_different (SOME tau1) tail
		     else SOME (tau1, tau2)
		   | find_different _ _ = NONE
				 
             in case find_different tau_opt tau_opts of
		    SOME (tau1, tau2) =>
		      (S1, tau, OG.SORTEDexp(errorConv (i, ErrorInfo.INCOMPATIBLE_TY(tau1, tau2)),
					     out_exp, out_ty::out_tys))
		  | NONE => 
		 case tau_opt
                  of SOME tau' =>
                    let val (S2, i') = UnifyWithTexts("type of expression",tau,
						      "disagrees with your sort constraint", tau', i)
                        val tau'' = S2 on tau'
                    in (S2 oo S1, tau'', OG.SORTEDexp(addTypeInfo_EXP(i',tau''), out_exp, 
						      out_ty::out_tys))
                    end
                   | NONE => (S1, tau, OG.SORTEDexp(okConv i, out_exp, out_ty::out_tys))
             end

           (* Handle exception *)                               (*rule 10*)
         | IG.HANDLEexp(i, exp, match) =>
             let
               val (S1, tau1, out_exp)   = elab_exp(C, exp)
               val (S2, tau2, out_match) = elab_match(S1 onC C, match)
               val matchTy = Type.mk_Arrow(Type.Exn, tau1)
               val (S3, i') = UnifyWithTexts("handled expression suggests handler type", matchTy, 
                                             "but I found handler type", tau2, i)
               val tau3 = (S3 oo S2) on tau1
             in
               (S3 oo S2 oo S1, tau3,
                OG.HANDLEexp(addTypeInfo_EXP(i',tau3), out_exp, out_match))
             end

           (* Raise exception *)                                (*rule 11*)
         | IG.RAISEexp(i, exp) =>
             let
               val (S1, tau1, out_exp) = elab_exp(C, exp)
               val exnType = Type.Exn
               val (S2, i')    = UnifyWithTexts("type of expression after 'raise' should be",
                                                exnType, "but I found type", tau1, i)
               val tau = Type.fresh_normal ()
             in
               (S2 oo S1, tau, OG.RAISEexp(addTypeInfo_EXP(i',tau), out_exp))
             end

           (* Function expression *)                            (*rule 12*)
         | IG.FNexp(i, match) => 
             let
               val (S, tau, out_match) = elab_match(C, match)
             in
               (S, tau, OG.FNexp(addTypeInfo_EXP(okConv i,tau), out_match))
             end

         | IG.UNRES_INFIXexp _ =>
             impossible "elab_exp(UNRES_INFIX)"


    (******** matches ********)

    and elab_match (C : Context, match : IG.match) :
        (Substitution * Type * OG.match) =

        case match of

          (* Match *)                                           (*rule 13*)
          IG.MATCH(i, mrule, NONE) =>
            let val (S, tau, out_mrule) = elab_mrule(C,mrule)
            in (S, tau, OG.MATCH(addTypeInfo_MATCH(okConv i,tau),out_mrule,NONE)) end

          (* Match *)
        | IG.MATCH(i, mrule, SOME match') =>
            let
              val (S ,tau ,out_mrule) = elab_mrule(C,mrule)
              val (S',tau',out_match) = elab_match(S onC C,match')
              val (S'', i') = UnifyWithTexts("type of match rule with wildcard", tau',
                                             "type of previous match rules", S' on tau,i)
              val tau'' = S'' on tau'
            in
              (S'' oo S' oo S, tau'',
               OG.MATCH(addTypeInfo_MATCH(i',tau''), out_mrule, SOME out_match))
            end

    (******** match rules ********)

    and elab_mrule (C : Context, mrule : IG.mrule) : 
        (Substitution * Type * OG.mrule) =

        case mrule of

          (* Match rule *)                                      (*rule 14*)
          IG.MRULE(i, pat, exp) =>
            let
              val (S, (VE,tau), out_pat) = elab_pat(C, pat)
              val (S',tau',  out_exp) = 
                     elab_exp(C.plus_VE (S onC C, VE),exp)
              val S'' = S' oo S
              val out_i = okConv i
            in
              (S'', Type.mk_Arrow (S'' on tau,tau'),
               OG.MRULE (out_i ,out_pat,out_exp))
            end

    (******** declarations ********)

    and elab_dec(C : Context, dec : IG.dec) : (Substitution * TyName list * Env * OG.dec) =
        (case dec of   

           (* Value declaration *)                              (*rule 15*)
           
           IG.VALdec (i, ExplicitTyVars, valbind) =>
             let
               val U = ListHacks.union (ExplicitTyVars,
                       ListHacks.minus (Environments.unguarded_valbind (E.to_TG (C.to_E C))
					                               valbind,
					C.to_U C))
               val _ = Level.push ()
	       val newC = C.plus_U (C, U)
	       val Uenv = C.to_Uenv newC  (* needed in RefDec *)
               val (S, VE, out_valbind) =
                     elab_valbind (newC, valbind) 
                                       (* plus_U creates levels for the explicit tyvars in U *)
                     handle E => (Level.pop(); raise E)
               val _ = Level.pop() 

               val (generic_tyvars, VE') = C.close (S onC C, valbind, VE)

(*for debugging
               fun pr_id id = 
                 case C.lookup_longid (C.plus_VE(C,VE')) (Ident.mk_LongId [id])
                   of SOME(VE.LONGVAR sigma) => print (id ^ ": " ^ TypeScheme.string sigma ^ "\n")
                    | _ => ()
*)
               (* Check that VE' is as general as required by TyGoals.  We need this because we 
                  use an instance of the TyGoal in elab_atpat.  - Rowan 3jan02  *)
               exception TG_more_general of TypeScheme * TypeScheme
               fun checkTG (id, VE.LONGVAR sigma) =
                    (case C.lookup_tygoal C id   
                      of NONE => ()
                       | SOME sigma_goal =>
                           if TypeScheme.generalises_TypeScheme (sigma, sigma_goal) then ()
                           else raise TG_more_general (sigma, sigma_goal)  )
                 | checkTG _ = ()

               val out_i = case ListHacks.intersect (ExplicitTyVars, C.to_U C)
                             of [] => ((VE.apply checkTG VE' ; 
                                        okConv i) 
                                    handle TG_more_general (sigma1, sigma2) =>
                                        errorConv(i, ErrorInfo.INCOMPATIBLE_TYSCH (sigma2, sigma1))
                                     | Match => errorConv(i, ErrorInfo.TYGOAL))
                              | explicittyvars => errorConv
                               (i, ErrorInfo.TYVARS_SCOPED_TWICE
                                    (map TyVar.from_ExplicitTyVar explicittyvars))

               val out_valbind = 
		 insert_type_info_in_valbind (generic_tyvars, VE', Uenv, out_valbind)

             (*The side condition ``U n tyvars VE' = {}'' is enforced partly
              by disallowing unification of free explicit tyvars (giving the
              error message

                      (fn x => let val y : 'a = x in y y end) 666 ;
                                       ^^^^^^^^^^
               Type clash,
                  type of left-hand side pattern:     'a
                  type of right-hand side expression: int).

              And partly the side condition is enforced by the restriction
              that the same explicit tyvar may not be scoped at two
              valbinds within each other (Definition 1997, sec. 2.9, last
              bullet).  The latter is checked above by checking whether
              any of `ExplicitTyVars' are in U of C (I hope that does it?)
              24/01/1997 15:38. tho.*)
             in
               (S, [], E.from_VE VE', 
                OG.VALdec (out_i, ExplicitTyVars, out_valbind)) 
             end

           (* valtype-declaration *)
         | IG.VALsdec (i, valsdesc) =>   (* add goal ty (RML) *)
           let val (TG, out_valsdesc) = elab_valsdesc (C, valsdesc)
	   in
             (Substitution.Id, [], E.from_TG TG, OG.VALsdec (okConv i, out_valsdesc))
	   end

         | IG.UNRES_FUNdec _ => impossible "elab_dec(UNRES_FUN)"
           
           (* Type declaration *)                               (*rule 16*)
         | IG.TYPEdec(i, typbind) =>
             let
               (* Note that no substitutions are produced *)
               val (TE, out_typbind) = elab_typbind(C, typbind)
             in
               (Substitution.Id, [], E.from_TE TE,
                OG.TYPEdec(addTypeInfo_TYENV (okConv i, TE), out_typbind))
             end

           (* Sort declaration *)                               (* RML *)
         | IG.SORTdec(i, typbind) =>
             let
               (* Note that no substitutions are produced *)
               val (TE, out_typbind) = elab_srtbind(C, typbind)
             in
               (Substitution.Id, [], E.from_TE TE,
                OG.SORTdec(addTypeInfo_TYENV (okConv i, TE), out_typbind))
             end

           (* Datatype declaration *)                           (*rule 17*)
         | IG.DATATYPEdec(i, datbind) =>
             let
               val TE = initial_TE datbind
               val ((VE1, TE1), out_datbind) = elab_datbind(C C_plus_TE TE, datbind)
               val (VE2, TE2) = Environments.maximise_equality_in_VE_and_TE (VE1, TE1) 
               val T = (TE.fold (fn tystr => fn T => case TypeFcn.to_TyName(TyStr.to_theta tystr)
                                                       of SOME t => t::T
                                                        | NONE => impossible "elab_dec(DATATYPEdec)")
                        [] TE2)
               val _ = debug_pr_msg "elab_dec(DATATYPEdec)"
             in
               (Substitution.Id, T, E.from_VE_and_TE (VE2, TE2),
                OG.DATATYPEdec(addTypeInfo_TYENV (okConv i, TE2),
                               addTypeInfo_DATBIND(out_datbind, TE2))) (*martin*) 
                                                               (*rowan: added back DATBIND_INFO *)
             end

           (*datatype replication*)                             (*rule 18*)
         | IG.DATATYPE_REPLICATIONdec(i, tycon, longtycon) => 
             (case C.lookup_longtycon C longtycon of
                SOME tystr =>
                  let
                    val TE = TE.singleton (tycon, tystr)
                    val (theta, VE) = TyStr.to_theta_and_VE tystr 
                  in
                    (Substitution.Id,[],
                     E.from_VE_and_TE (VE,TE),
                     OG.DATATYPE_REPLICATIONdec
                       (addTypeInfo_TYENV (okConv i, TE), tycon, longtycon))
                  end
              | NONE =>
                  (Substitution.Id,[],
                   E.bogus,
                   OG.DATATYPE_REPLICATIONdec
                     (lookupTyConError (i,longtycon), tycon, longtycon)))

           (* datasort declarations (RML) *)
	 | IG.DATASORTdec(i, datbind) =>
	   let
             val TE = datasort_TE (C, datbind)   (* uses val constr's to match sort to type *)
             val keep = !elab_ty_allow_intersect
             val _ = elab_ty_allow_intersect := true
             val out_datbind = elab_datbind_s(C C_plus_TE TE, datbind)  (* checks types, constrs *)
             val _ = elab_ty_allow_intersect := keep
           in
	       (Substitution.Id, [],
		E.from_TE TE,
		OG.DATASORTdec(okConv i, out_datbind))             
           end

           (* Abstype declaration *)                            (*rule 19*)
         | IG.ABSTYPEdec(i, datbind, dec) =>
             let
               val TE = initial_TE datbind
               val ((VE1, TE1), out_datbind) = elab_datbind(C C_plus_TE TE, datbind)
               val (VE2, TE2) = Environments.maximise_equality_in_VE_and_TE
                                  (VE1, TE1)
               val (S, T, E, out_dec) = 
		   elab_dec(C C_plus_VE_and_TE (VE2,TE2),dec)
               val (T',E',phi) = Environments.ABS (TE2, E)
                 (* the realisation returned maps abstract type 
                  * names to type names for the datbind. *)
               val out_i = addTypeInfo_ABSTYPE (okConv i, (TE2, phi))
             in
               (S,T @ T', E.erase_TG E',
                OG.ABSTYPEdec(out_i, out_datbind, out_dec))
             end

           (* Exception declaration *)                          (*rule 20*)
         | IG.EXCEPTIONdec(i, exbind) =>
             let
               val (VE, out_exbind) = elab_exbind (C, exbind)
             in
               (Substitution.Id, [],
                E.from_VE VE,
                OG.EXCEPTIONdec(okConv i, out_exbind))
             end

           (* Local declaration *)                              (*rule 21*)
         | IG.LOCALdec(i, dec1, dec2) =>
             let
               val (S1, T1, E1, out_dec1) = elab_dec(C.erase_TG C, dec1)
               val (S2, T2, E2, out_dec2) = elab_dec((S1 onC C) C_plus_E E1, dec2)
             in
               (S2 oo S1, T1 @ T2, E.erase_TG E2, OG.LOCALdec(okConv i,out_dec1,out_dec2))
             end

           (* Open declaration *)                               (*rule 22*)
         | IG.OPENdec(i, list) =>
             let
               fun process(E0, list)
                   : Env * Environments.longstrid OG.WithInfo list =
                 case list
                   of IG.WITH_INFO(i, longstrid) :: rest =>
                        (case C.lookup_longstrid C longstrid of
                           SOME E =>
                                let val (E', rest') = process(E0, rest)
                                in
                                  (E.plus (E, E'),
                                   OG.WITH_INFO (okConv i, longstrid) :: rest')
                                end

                            | NONE =>   (* Lookup failure: process rest of
                                           list and build env regardless. *)
                                let
                                  val (E', rest') = process(E0, rest)
                                  val ei = ErrorInfo.LOOKUP_LONGSTRID longstrid
                                in
                                  (E', OG.WITH_INFO(errorConv(i, ei), longstrid)
                                       :: rest'
                                  )
                                end
                        )

                    | nil => (E.empty, nil)

               val (E', list') = process(E.empty, list)
               val i' = ElabInfo.plus_TypeInfo (okConv i) 
                 (TypeInfo.OPEN_INFO
                  let val (SE,TE,VE) = E.un E'
                  in (EqSet.list (SE.dom SE), EqSet.list (TE.dom TE), EqSet.list (VE.dom VE))
                  end)
             in
               (Substitution.Id, [], E', OG.OPENdec(i', list'))
             end

         | IG.INFIXdec(i, prec, ids) =>    (* infix -- no rule in Definition *)
             (Substitution.Id, [],
              E.from_VE VE.empty,
              OG.INFIXdec(okConv i, prec, ids))

         | IG.INFIXRdec(i, prec, ids) =>   (* infixr -- no rule in Definition *)
             (Substitution.Id, [],
              E.from_VE VE.empty,
              OG.INFIXRdec(okConv i, prec, ids))

         | IG.NONFIXdec(i, ids) =>         (* nonfix -- no rule in Definition *)
             (Substitution.Id, [],
              E.from_VE VE.empty,
              OG.NONFIXdec(okConv i, ids))

           (* Empty declaration *)                              (*rule 23*)
         | IG.EMPTYdec(i) =>
             (Substitution.Id, [], E.from_VE VE.empty, OG.EMPTYdec(okConv i))

(*          | IG.SEQdec(i, dec1 as (VALsdec _), dec2 as (VALdec _)) => *)

           (* Sequential declaration *)                         (*rule 24*)
         | IG.SEQdec(i, dec1, dec2) =>
             let
               val (S1, T1, E1, out_dec1) = elab_dec(C, dec1)
               val (S2, T2, E2, out_dec2) = elab_dec((S1 onC C) C_plus_E E1, dec2)
               val E1' = E.on (S2, E1)
               (* check that tygoals from E1 are met in E2? *)
             in
               (S2 oo S1, T1 @ T2,
                E.plus (E1',E2),
                OG.SEQdec(okConv i,out_dec1,out_dec2)) 
             end)

    (****** value bindings - Definition, p. ? ******)

    and elab_valbind(C : Context, valbind : IG.valbind)
          : (Substitution * VarEnv * OG.valbind) =

        case valbind of

        (* Simple value binding *)                              (*rule 25*)
        IG.PLAINvalbind(i, pat, exp, valbind_opt) =>
          let
            val (S0, (VE,tau), out_pat) = elab_pat(C, pat)
            val (S1, tau1, out_exp) = elab_exp(S0 onC (C.erase_TG C), exp)

            val (S2, i') = UnifyWithTexts("type of left-hand side pattern",(S1 oo S0) on tau,
                                          "type of right-hand side expression", tau1, i)

            (* if there was a unification error in the line above, change the right source
               info field of i' to become the right end of exp : *)
            val i' = case ElabInfo.to_ErrorInfo i' of
                    NONE => i'
                    | SOME _ => ElabInfo.retractRight (i', OG.get_info_exp out_exp)

            val (S3, VE', valbind_opt') =
              case valbind_opt
                of SOME valbind =>
                     let
                       val (S, VE, vb) =
                         elab_valbind((S2 oo S1 oo S0) onC C, valbind)
                     in
                       (S, VE, SOME vb)
                     end

                 | NONE =>
                     (Substitution.Id, VE.empty, NONE)
            val intdom = EqSet.intersect (VE.dom VE) (VE.dom VE')
          in 
            if EqSet.isEmpty intdom then
              (case List.filter IG.is_'true'_'nil'_etc
                      (EqSet.list (VE.dom VE)) of
                 [] =>
                   (S3 oo S2 oo S1 oo S0, 
                    VE.plus ((S3 oo S2 oo S1 oo S0) onVE VE, VE'),
                    OG.PLAINvalbind
                    (addTypeInfo_PLAINvalbind(i', tau1),
                     out_pat, out_exp, valbind_opt'))
               | ids =>
                   (S3, VE',
                    OG.PLAINvalbind
                      (plus_ErrorInfo i'
                         (ErrorInfo.REBINDING_TRUE_NIL_ETC ids),
                       out_pat, out_exp, valbind_opt')))
            else
              (S3, VE',
               OG.PLAINvalbind((case ElabInfo.to_ErrorInfo i' of
                                  NONE => 
                                   plus_ErrorInfo i' 
                                    (ErrorInfo.REPEATED_IDS 
                                     (map ErrorInfo.ID_RID (EqSet.list intdom)))
                                | SOME _ => i'),
                               out_pat, out_exp, valbind_opt'))
          end
                                                                (*rule 26*)
                                (* Recursive value binding. Rather tricky
                                   because we have to plant error info after
                                   the second pass. Make that `very tricky.' *)
      | IG.RECvalbind(i, valbind) => 
          let
                                (* Function to unify the occurrence of a
                                   variable in two VE's. The result is a
                                   substitution and an ErrorInfo tag. *)

            fun processID(i, VE, VE', id): Substitution * ElabInfo =
                  (case (VE.lookup VE id, VE.lookup VE' id) of
                     (SOME (VE.LONGVAR sigma1), SOME (VE.LONGVAR sigma2)) =>
                       let val (_, tau1) = TypeScheme.to_TyVars_and_Type sigma1
                           val (_, tau2) = TypeScheme.to_TyVars_and_Type sigma2
                       in (case Type.unify(tau1, tau2) of
                             Type.UnifyOk => (Substitution.Id, i)   (* substitutions are dummies *)
                           | Type.UnifyFail => (Substitution.Id,
                                                plus_ErrorInfo i
                                                (ErrorInfo.UNIFICATION(tau1, tau2)))
                           | Type.UnifyRankError(tv,tn) => (Substitution.Id,
                                                            plus_ErrorInfo i
                                                            (ErrorInfo.UNIFICATION(tau1, tau2))))
                       end
                    | _ => impossible "processID")

                                (* Traverse the out_valbind, doing a
                                   unification (and adding ErrorInfo if reqd.)
                                   and giving a subst. at each stage. The
                                   ErrorInfo goes into the pattern...
                                   ...somewhere... *)

            fun traverseRecValbind(VE, VE', vb): Substitution * OG.valbind =
              case vb
                of OG.PLAINvalbind(i, pat, exp, vb_opt) =>
                     let val (S, pat') = traverseRecPat(VE, VE', pat)
                         val (S', vb_opt') =
                           case vb_opt
                             of SOME vb =>
                                let val (S', vb') = traverseRecValbind (S onVE VE, S onVE VE', vb)
                                in (S' oo S, SOME vb')
                                end
                              | NONE => (S, NONE)
                     in (S', OG.PLAINvalbind(i, pat', exp, vb_opt'))
                     end

                 | OG.RECvalbind(i, vb) =>
                     let
                       val (S, vb') = traverseRecValbind(VE, VE', vb)
                     in
                       (S, OG.RECvalbind(i, vb'))
                     end

            and traverseRecPat(VE, VE', pat): Substitution * OG.pat =
              case pat
                of OG.ATPATpat(i, atpat) =>
                     let
                       val (S, atpat') = traverseRecAtpat(VE, VE', atpat)
                     in
                       (S, OG.ATPATpat(i, atpat'))
                     end

                 | OG.CONSpat(i, id, atpat) =>
                     let val (S, atpat') = traverseRecAtpat(VE, VE', atpat)
                     in (S, OG.CONSpat(i, id, atpat'))
                     end

                 | OG.TYPEDpat(i, pat, ty) =>
                     let
                       val (S, pat') = traverseRecPat(VE, VE', pat)
                     in
                       (S, OG.TYPEDpat(i, pat', ty))
                     end

                 | OG.SORTEDpat(i, pat, tys) =>
                     let
                       val (S, pat') = traverseRecPat(VE, VE', pat)
                     in
                       (S, OG.SORTEDpat(i, pat', tys))
                     end

                 | OG.LAYEREDpat(i, id as OG.OP_OPT(id', withOp),
                                 ty_opt, pat
                                ) =>
                     let
                       val (S, pat') = traverseRecPat(VE, VE', pat)
                       val (S', i') = processID(i, S onVE VE, S onVE VE', id')
                     in
                       (S oo S', OG.LAYEREDpat(i', id, ty_opt, pat'))
                     end
            
                 | OG.UNRES_INFIXpat _ =>
                     impossible "traverseRecPat(UNRES_INFIX)"

            and traverseRecAtpat(VE, VE', atpat): Substitution * OG.atpat =
              case atpat
                of OG.WILDCARDatpat _ => (Substitution.Id, atpat)

                 | OG.SCONatpat _ => (Substitution.Id, atpat)

                 | OG.LONGIDatpat(i, longid_op as OG.OP_OPT(longid, withOp)) =>
                     (case C.lookup_longid C longid of
                        SOME (VE.LONGCON _) => (Substitution.Id, atpat)
                      | SOME (VE.LONGEXCON _) => (Substitution.Id, atpat)
                      | _ => (case Ident.decompose longid of
                                ([], id) =>
                                  let val (S, i') = processID(i, VE, VE', id)
                                  in (S, OG.LONGIDatpat(i', longid_op))
                                  end
                              | _ => impossible "traverseRecAtpat(longid)"))

                 | OG.RECORDatpat(i, patrowOpt) =>
                     (case patrowOpt of 
                        NONE => (Substitution.Id,atpat)
                      | SOME patrow => 
                          let 
                            val (S, patrow') =
                              traverseRecPatrow (VE, VE', patrow)
                          in
                            (S, OG.RECORDatpat(i, SOME patrow'))
                          end)

                 | OG.PARatpat(i, pat) =>
                     let
                       val (S, pat') = traverseRecPat(VE, VE', pat)
                     in
                       (S, OG.PARatpat(i, pat'))
                     end

            and traverseRecPatrow(VE, VE', patrow): Substitution * OG.patrow =
              case patrow of 
                OG.DOTDOTDOT i => (Substitution.Id, patrow)
              | OG.PATROW(i, l, pat, patrowOpt) =>
                  let 
                    val (S, pat') = traverseRecPat(VE, VE', pat)
                    val (S', patrowOpt') =
                      (case patrowOpt of 
                        NONE => (Substitution.Id, NONE)
                      | SOME patrow => 
                          let 
                            val (S'', patrow') = traverseRecPatrow(VE, VE', patrow)
                          in
                            (S'', SOME patrow')
                          end)
                  in
                    (S' oo S, OG.PATROW(i, l, pat', patrowOpt'))
                  end


            (* set up a value environment, VE, for the recursively declared values *)

            val domain_list = dom_vb(C, valbind) 

            fun TypeScheme_fresh () = TypeScheme.from_Type (Type.fresh_normal ())

            fun setup (id, VE) =
                  VE.plus (VE, VE.singleton_var (id, TypeScheme_fresh ()))

            val VE = foldl setup VE.empty domain_list
                                (* VE now maps each rec identifier to 'a. *)

                                (* Proceed with type checking. The ErrorInfo
                                   tags for the rec identifiers will be
                                   untouched (I hope, since we might assign
                                   them further on). *)

            val (S, VE', valbind') =
                  elab_valbind (C.plus_VE (C, VE), valbind)

                                (* Post-pass, to patch up the rec identifiers
                                   and plant unification error tags: *)

            val (S', valbind'') =
                  traverseRecValbind (S onVE VE, VE', valbind')

            val VE'' = S' onVE VE'

            val out_i = okConv i
          in
            if !Flags.DEBUG_ELABDEC then
              pr ("RECvalbind: ", StringTree.NODE {start="{", finish="}", indent=0,
                                           children=[VE.layout VE''],
                                           childsep=StringTree.NOSEP})
            else () ;
            (S' oo S, VE'', OG.RECvalbind (out_i, valbind''))
          end

    (* RML sort goal.  Required during elaboration to synchronize polymorphism.   *)
    and elab_valsdesc (C, IG.VALSDESC (i, id, ty::tys, valsdesc_opt)) : TyGoals * OG.valsdesc =
	  let val (TG2, out_valsdesc_opt) = elab_valsdesc_opt (C, valsdesc_opt)
              val _ = Level.push()
	      val explicittyvars = 
                  ListHacks.minus (EqSet.list (Environments.ExplicitTyVarsTy ty), C.to_U C)
	      val C2 = C.plus_U(C, explicittyvars)
              val (tau_opt, out_ty) = elab_srt(C2, ty)
                                      handle E => (Level.pop(); raise E)
              fun do_elab ty = elab_srt (C2, ty)
                                      handle E => (Level.pop(); raise E)
              val pairs = map do_elab tys
              val out_tys = map #2 pairs
              val tau_opts = map #1 pairs
              fun find_different (SOME tau1) (SOME tau2::tail) = 
 		   if Type.eq (tau1, tau2) then find_different (SOME tau1) tail
		   else SOME (tau1, tau2)
		| find_different _ _ = NONE

              val out_i = case find_different tau_opt tau_opts of
			      NONE => okConv i
			    | SOME (tau1, tau2) => 
			      errorConv (i, ErrorInfo.INCOMPATIBLE_TY(tau1, tau2))
              val _ = Level.pop()
              val TG = 
                  (case tau_opt of 
                     NONE =>  TG2
                   | SOME tau =>  TG.add0(id, TypeScheme.close_overload tau, TG2) )
	  in
	    (TG, OG.VALSDESC (out_i, id, out_ty::out_tys, out_valsdesc_opt))
	  end

    and elab_valsdesc_opt (C, NONE) = (TG.empty, NONE)
      | elab_valsdesc_opt (C, SOME valsdesc) = 
          let val (TG, out_valsdesc) = elab_valsdesc (C, valsdesc)
          in   (TG, SOME out_valsdesc)   end
        

    and elab_srtbind (C, typbind) = 
	let val keep = !elab_ty_allow_intersect
	    val _ = elab_ty_allow_intersect := true
	    val res = elab_typbind (C, typbind)
	    val _ = elab_ty_allow_intersect := keep
	in
	    res
	end

    (******* type bindings *******)

    and elab_typbind (C : Context, typbind : IG.typbind) : (TyEnv * OG.typbind) =

      case typbind of

        (* Type binding *)                                      (*rule 27*)
        IG.TYPBIND(i, ExplicitTyVars, tycon, ty, typbind_opt) =>
          let
            val _ = Level.push()

            val (TyVars,C') = C.plus_U'(C, ExplicitTyVars)

            val tyvarsRepeated = getRepeatedElements (op =) ExplicitTyVars
            val tyvarsNotInTyVarList =
              List.filter (fn tv => not (ListHacks.member tv ExplicitTyVars)) 
              (IG.getExplicitTyVarsTy ty)

          in case elab_ty(C', ty)
               of (SOME tau, out_ty) =>
                 let val _ = Level.pop()
                     val typeFcn = TypeFcn.from_TyVars_and_Type (TyVars, tau)
                     val tystr = TyStr.from_theta_and_VE(typeFcn, VE.empty)
                     val (TE, out_typbind_opt) = elab_typbind_opt(C, typbind_opt)
                 in
                   if not(isEmptyTyVarList(tyvarsNotInTyVarList)) then
                     (TE, OG.TYPBIND(errorConv(i, ErrorInfo.TYVARS_NOT_IN_TYVARSEQ 
                                               (map TyVar.from_ExplicitTyVar tyvarsNotInTyVarList)),
                                     ExplicitTyVars, tycon, out_ty, out_typbind_opt))
                   else
                     if (EqSet.member tycon (TE.dom TE)) then 
                       (TE.plus (TE.singleton(tycon, tystr), TE),
                        OG.TYPBIND(repeatedIdsError(i, [ErrorInfo.TYCON_RID tycon]),
                                   ExplicitTyVars, tycon, out_ty, out_typbind_opt))
                     else
                       case tyvarsRepeated
                         of [] =>
                           (TE.plus (TE.singleton(tycon, tystr), TE),
                            OG.TYPBIND(okConv i, ExplicitTyVars, tycon, out_ty, out_typbind_opt))
                          | _ => 
                           (TE, OG.TYPBIND(repeatedIdsError(i, map ErrorInfo.TYVAR_RID 
                                                            (map TyVar.from_ExplicitTyVar tyvarsRepeated)),
                                           ExplicitTyVars, tycon, out_ty, out_typbind_opt))
                 end
                | (NONE, out_ty) =>
                 let val _ = Level.pop()
                     val (TE, out_typbind_opt) = elab_typbind_opt(C, typbind_opt)
                 in (TE, OG.TYPBIND(okConv i, ExplicitTyVars, tycon, out_ty, out_typbind_opt))
                 end
          end

    and elab_typbind_opt (C : Context, typbind_opt : IG.typbind option)
      : (TyEnv * OG.typbind option) =

      case typbind_opt of

        SOME(typbind) =>
          let
            val (TE, out_typbind) = elab_typbind(C, typbind)
          in
            (TE, SOME out_typbind)
          end

      | NONE =>
          (TE.empty, NONE)

    (******* datatype bindings *******)

    and elab_datbind (C : Context, datbind : IG.datbind)
      : ((VarEnv * TyEnv) * OG.datbind) =

      case datbind of
        (* Datatype binding *)                                  (*rule 28*)
        IG.DATBIND(i, ExplicitTyVars, tycon, conbind, datbind_opt) =>
          let
            val _ = Level.push()

            val (TyVars, C') = C.plus_U'(C, ExplicitTyVars)

            val tyvarsRepeated = getRepeatedElements (op =) ExplicitTyVars
            val tyvarsNotInTyVarList =
              List.filter 
                (fn tv => not (ListHacks.member tv ExplicitTyVars))
                (IG.getExplicitTyVarsConbind conbind)
            val (typeFcn, _) = 
              case C.lookup_tycon C tycon of
                SOME(tystr) => TyStr.to_theta_and_VE(tystr)
              | NONE => impossible "datbind(1)"

            val tyname =
              case TypeFcn.to_TyName typeFcn of
                SOME(tyname) => tyname
              | NONE => impossible "datbind(2)"

            val tau_list =
              map Type.from_TyVar TyVars

            val tau =
              Type.from_ConsType (Type.mk_ConsType (tau_list, tyname))

            val (constructor_map : constructor_map,
                 out_conbind) = elab_conbind (C', tau, disallow_repeat, conbind)
            val _ = Level.pop()
            val VE = constructor_map.to_VE constructor_map

            val VE_closed = VE.close VE

            val tystr = TyStr.from_theta_and_VE (TypeFcn.from_TyName tyname, VE_closed)

            val ((VE', TE'), out_datbind_opt) = elab_datbind_opt (C, datbind_opt)
            val out_i =
                  if IG.TyCon.is_'true'_'nil'_etc tycon then
                    errorConv (i, ErrorInfo.REBINDING_TRUE_NIL_ETC [])
                  else if IG.TyCon.is_'it' tycon then
                    errorConv (i, ErrorInfo.REBINDING_IT)
                  else if not (isEmptyTyVarList tyvarsNotInTyVarList) then
                    errorConv (i, ErrorInfo.TYVARS_NOT_IN_TYVARSEQ 
                               (map TyVar.from_ExplicitTyVar
                                  tyvarsNotInTyVarList))
                  else
                    let val repeated_ids_errorinfos =
                      map ErrorInfo.TYVAR_RID (map TyVar.from_ExplicitTyVar tyvarsRepeated)
                      @ map ErrorInfo.ID_RID
                          (EqSet.list (EqSet.intersect (VE.dom VE') (VE.dom VE)))
                      @ (if EqSet.member tycon (TE.dom TE')
                         then [ErrorInfo.TYCON_RID tycon] else [])
                    in
                      case repeated_ids_errorinfos
                        of [] => okConv i
                         | _ => repeatedIdsError (i, repeated_ids_errorinfos)
                    end 
          in
            ( (VE.plus  (VE_closed, VE'),
               TE.plus (TE.singleton (tycon, tystr), TE')),
              OG.DATBIND(out_i, ExplicitTyVars, tycon,
                         out_conbind, out_datbind_opt) )
          end

    and elab_datbind_opt (C : Context, datbind_opt : IG.datbind option)
      : ((VarEnv * TyEnv) * OG.datbind option) =

      case datbind_opt of

        SOME(datbind) =>
          let
            val ((VE, TE), out_datbind) = elab_datbind(C, datbind)
          in
            ((VE, TE), SOME out_datbind)
          end

       | NONE =>
          ((VE.empty, TE.empty), NONE)


    (******* datasort bindings - just check compatibility of types  *******)

    and elab_datbind_s (C : Context, datbind : IG.datbind) : OG.datbind =
      case datbind of
        IG.DATBIND(i, ExplicitTyVars, tycon, conbind, datbind_opt) =>
          let
            val _ = Level.push()
            val (TyVars, C') = C.plus_U'(C, ExplicitTyVars)
            val tyvarsRepeated = getRepeatedElements (op =) ExplicitTyVars
            val tyvarsNotInTyVarList =
              List.filter 
                (fn tv => not (ListHacks.member tv ExplicitTyVars))
                (IG.getExplicitTyVarsConbind conbind)
            val (typeFcn, VEdataty) = 
              case C.lookup_tycon C tycon of
                SOME(tystr) => TyStr.to_theta_and_VE(tystr)
              | NONE => impossible "datbind(1)"
            val tyname =
              case TypeFcn.to_TyName typeFcn of
                SOME(tyname) => tyname
              | NONE => impossible "datbind(2)"
            val tau_list =
              map Type.from_TyVar TyVars
            val tau =
              Type.from_ConsType (Type.mk_ConsType (tau_list, tyname))
            val (constructor_map : constructor_map,
                 out_conbind) = elab_conbind (C', tau, allow_repeat, conbind)
            val _ = Level.pop()
            val VE = constructor_map.to_VE constructor_map
            val VE_closed = VE.close VE

            fun mismatches_dataty (id, VE.LONGCON tysch) err_opt = 
                (case err_opt 
                   of SOME _ => err_opt
                    | NONE =>              
                     case VE.lookup VEdataty id
		       of SOME (VE.LONGCON tysch') =>	 
                          if TypeScheme.eq (tysch, tysch') then   (* This equality depends on tv order.  OK? *)
                            NONE 
                          else SOME (ErrorInfo.INCOMPATIBLE_TYSCH (tysch', tysch))
                        | _ => SOME (ErrorInfo.LOOKUP_LONGID (Ident.idToLongId id)))
              | mismatches_dataty _ _ = Crash.impossible "ElabDec.mismatches_dataty"
            val IG.CONBIND(_, IG.OP_OPT(first_longid, _), _, _) = conbind  (* for errors below *) 
            val out_i =
                  if VE.is_empty VEdataty then         (* empty VE => valcons not found *)
                    errorConv (i, ErrorInfo.LOOKUP_LONGID first_longid)
                  else if not (isEmptyTyVarList tyvarsNotInTyVarList) then
                    errorConv (i, ErrorInfo.TYVARS_NOT_IN_TYVARSEQ 
                               (map TyVar.from_ExplicitTyVar
                                  tyvarsNotInTyVarList))
                  else case (VE.Fold mismatches_dataty NONE VE_closed) 
                         of SOME err => 
                              errorConv (i, err)
                          | NONE =>
                    case map ErrorInfo.TYVAR_RID (map TyVar.from_ExplicitTyVar tyvarsRepeated)
                      of [] => okConv i
                       | _ => repeatedIdsError (i, map ErrorInfo.TYVAR_RID 
                                                       (map TyVar.from_ExplicitTyVar tyvarsRepeated))
                 
            val out_datbind_opt = case datbind_opt of NONE => NONE
                                                    | SOME datbind=> SOME (elab_datbind_s (C, datbind))
          in
            OG.DATBIND(out_i, ExplicitTyVars, tycon, out_conbind, out_datbind_opt)
          end


    (****** constructor bindings *****)

    and elab_conbind (C : Context, tau : Type, can_repeat : bool, conbind : IG.conbind)
      : (constructor_map * OG.conbind) =

      (*I deviate from the definition in letting elab_conbind return
       a mapping from id's (constructors) to TypeScheme's instead of
       a VE, because a VE also contains fellow constructors for each
       constructor, and the fellow constructors cannot be determined
       before the whole conbind has been processed. I let the caller
       of elab_conbind (elab_datbind) turn this map into a VE proper
       19/01/1997 17:02. tho.*)

      case conbind of

        (* Constructor binding *)                               (*rule 29*)
        IG.CONBIND(i, IG.OP_OPT(longcon, withOp), SOME ty, conbind_opt) =>
          let val (constructor_map, out_conbind_opt) = 
	             elab_conbind_opt (C, tau, can_repeat, conbind_opt)
              val (strid, con) = Ident.decompose longcon
              fun result out_ty = 
  		    OG.CONBIND (out_i_for_conbind con constructor_map can_repeat strid i,
				OG.OP_OPT (longcon, withOp),
				SOME out_ty, out_conbind_opt)
          in case elab_ty (C, ty)
               of (SOME tau', out_ty) =>
                 let val arrow = TypeScheme.from_Type (Type.mk_Arrow (tau', tau))
                 in (constructor_map.add con arrow constructor_map, result out_ty)
                 end
                | (NONE, out_ty) => (constructor_map, result out_ty)
          end

      | IG.CONBIND(i, IG.OP_OPT(longcon, withOp), NONE, conbind_opt) =>
          let val (constructor_map, out_conbind_opt) = 
	        elab_conbind_opt (C, tau, can_repeat, conbind_opt)
              val (strid, con) = Ident.decompose longcon
          in
            (constructor_map.add con (TypeScheme.from_Type tau) constructor_map,
             OG.CONBIND (out_i_for_conbind con constructor_map can_repeat strid i, 
                         OG.OP_OPT(longcon, withOp), NONE, out_conbind_opt))
          end

    and out_i_for_conbind con constructor_map can_repeat strid i = 
          if can_repeat then okConv i  (* all errors caught on datatype *)
          else if constructor_map.in_dom con constructor_map then 
            repeatedIdsError (i, [ErrorInfo.CON_RID con])
          else if IG.is_'true'_'nil'_etc con then 
            errorConv (i, ErrorInfo.REBINDING_TRUE_NIL_ETC [con])
          else if IG.is_'it' con then errorConv (i, ErrorInfo.REBINDING_IT)
          else case strid of [] => okConv i
	                   | _ => errorConv(i, ErrorInfo.QUALIFIED_CON)

    and elab_conbind_opt (C : Context, tau : Type, can_repeat : bool, 
			  conbind_opt : IG.conbind option)
      : (constructor_map * OG.conbind option) =

      case conbind_opt 

        of SOME conbind =>
          let val (constructor_map, out_conbind) = elab_conbind (C, tau, can_repeat, conbind)
          in (constructor_map, SOME out_conbind)
          end

         | NONE => (constructor_map.empty, NONE)

    (****** exception bindings *****)

    and elab_exbind (C : Context, exbind : IG.exbind)
      : (VarEnv * OG.exbind) =

      case exbind of

        (* Exception binding *)                                 (*rule 30*)
        IG.EXBIND(i, IG.OP_OPT(excon, withOp), SOME ty, rest) =>
          let val (VE_rest, out_rest) = elab_exbind_opt (C, rest)
          in case elab_ty (C, ty)
               of (SOME tau, out_ty) =>
                 let val exnTy = Type.mk_Arrow (tau, Type.Exn)
                     val VE_this = VE.singleton_excon (excon, exnTy)
                 in 
                   (VE.plus  (VE_this, VE_rest),
                    OG.EXBIND (out_i_for_exbind excon VE_rest i (SOME tau), 
                               OG.OP_OPT(excon, withOp), SOME out_ty, out_rest))
                 end
                | (NONE, out_ty) => 
                 (VE_rest, OG.EXBIND(okConv i, OG.OP_OPT(excon, withOp), SOME out_ty, out_rest))
          end

      | IG.EXBIND(i, IG.OP_OPT(excon, withOp), NONE, rest) =>
          let
            val VE_this = VE.singleton_excon (excon, Type.Exn)
            val (VE_rest, out_rest) = elab_exbind_opt (C, rest)
          in
            (VE.plus  (VE_this, VE_rest),
             OG.EXBIND (out_i_for_exbind excon VE_rest i NONE, 
                        OG.OP_OPT(excon, withOp), NONE, out_rest))
          end

        (* Exception binding *)                                 (*rule 31*)
      | IG.EXEQUAL(i, IG.OP_OPT(excon, exconOp),
                      IG.OP_OPT(longid, longidOp), rest) =>
          (case C.lookup_longid C longid of
             SOME (VE.LONGEXCON tau) =>
               let
                 val VE_this = VE.singleton_excon (excon, tau)
                 val (VE_rest, out_rest) = elab_exbind_opt (C, rest)
               in 
                 (VE.plus  (VE_this, VE_rest),
                  OG.EXEQUAL (out_i_for_exbind excon VE_rest i NONE,
                              OG.OP_OPT(excon, exconOp),
                              OG.OP_OPT(longid, longidOp), out_rest))
               end
           | _ => (*Carry on, building an error node.*)
               let val (VE_rest, out_rest) = elab_exbind_opt (C, rest)
               in
                 (VE_rest, OG.EXEQUAL(lookupIdError(i, longid),
                                      OG.OP_OPT(excon, exconOp),
                                      OG.OP_OPT(longid, longidOp),
                                      out_rest))
               end)

    and out_i_for_exbind excon VE_rest i tau_opt =
          if EqSet.member excon (VE.dom VE_rest)
          then repeatedIdsError (i, [ErrorInfo.EXCON_RID excon])
          else if IG.is_'true'_'nil'_etc excon
               then errorConv (i, ErrorInfo.REBINDING_TRUE_NIL_ETC [excon])
               else if IG.is_'it' excon
                    then errorConv (i, ErrorInfo.REBINDING_IT)
                    else addTypeInfo_EXBIND (okConv i, tau_opt)

    and elab_exbind_opt (C, SOME exbind) =
          let val (VE, out_exbind) = elab_exbind (C, exbind)
          in
            (VE, SOME out_exbind)
          end

      | elab_exbind_opt(C, NONE) = (VE.empty, NONE)

    (****** atomic patterns ******)

    and elab_atpat (C : Context, atpat : IG.atpat) :
        (Substitution * (VarEnv * Type) * OG.atpat) =

        case atpat of

          (* Wildcard *)                                        (*rule 32*)
          IG.WILDCARDatpat i  =>
            (Substitution.Id, 
             (VE.empty, Type.fresh_normal ()),
              OG.WILDCARDatpat(okConv i))

          (* Special constant *)                                (*rule 33*)
        | IG.SCONatpat(i,scon) =>
            (* Some special constants are overloaded; thus, we must
             * record some overloading info in the case a special constant
             * can denote one of a set of type names. 
             *)
            let val {type_scon, overloading} = Type.of_scon scon
                val i_out =
                  case overloading
                    of NONE => okConv i
                     | SOME tv => preOverloadingConv (i, OverloadingInfo.UNRESOLVED_IDENT tv)
            in (Substitution.Id, (VE.empty, type_scon), OG.SCONatpat (i_out, scon))
            end

          (* Long identifier *)                                 (*rule 34*)
        | IG.LONGIDatpat(i, IG.OP_OPT(longid, withOp)) =>
            (case C.lookup_longid C longid of
               SOME(VE.LONGCON sigma) =>          (* rule 36 *)
                    let
                      fun isConsType tau =
                            (case Type.to_ConsType tau of
                               SOME _ => true
                             | NONE => false)
                      val (tau,instances) = (TypeScheme.instance' sigma)
                      val (tau', i') =
                        if isConsType tau then
                          (tau, okConv i)
                        else
                          (Type_bogus (),
                           errorConv(i, ErrorInfo.NOTCONSTYPE tau)  )
                    in
                      (Substitution.Id,
                       (VE.empty, tau'),
                       OG.LONGIDatpat(addTypeInfo_CON(i', C, instances, longid),
                                      OG.OP_OPT(longid, withOp)  )  )
                    end

                | SOME(VE.LONGEXCON tau) =>
                    let
                      val exnType = Type.Exn
                      val (_, i') = UnifyWithTexts
                                      ("expected long excon in pattern to have type",
                                       exnType, "but found it to have type", tau,i)
                    in
                      (Substitution.Id,
                       (VE.empty, exnType),
                       OG.LONGIDatpat(addTypeInfo_EXCON(i',exnType,longid),
                                      OG.OP_OPT(longid, withOp)))
                    end

                | _ =>          (* make new variable environment *) 
                                (* unbound long identifier *)
		  case Ident.decompose longid
		    of (_::_, _) =>
                             (Substitution.Id,
                              (VE.bogus, Type_bogus ()),
                              OG.LONGIDatpat(errorConv(i, ErrorInfo.QUALIFIED_ID longid),
                                             OG.OP_OPT(longid, withOp)  )  )                       
                     | (nil, id) =>
                       let
			 val tau =
		           case C.lookup_tygoal C id 
			     of NONE => Type.fresh_normal ()
                              | SOME sigma => (* Instantiate here: see ElabDec: VALdec *)
                                (if !Flags.DEBUG_ELABDEC then
                                   pr("elab_atpat:id:  ", C.layout C)
                                 else ();
                                 TypeScheme.instance sigma ) 
			 val tau_scheme = TypeScheme.from_Type tau
		       in
			     (Substitution.Id,
                              (VE.singleton_var(id, tau_scheme), tau),
                              OG.LONGIDatpat(addTypeInfo_VAR_PAT(okConv i, tau),
                                             OG.OP_OPT(longid, withOp)  )  )
		       end
            )

          (* Record pattern *)                                  (*rule 36*)
        | IG.RECORDatpat(i, row_opt as NONE) =>
            (Substitution.Id,
             (VE.empty, Type.Unit),
              OG.RECORDatpat(okConv i, NONE))

        | IG.RECORDatpat(i, row_opt as SOME patrow) =>
            let
              val (S, (VE, rho), out_patrow) = elab_patrow(C, patrow)
            in
              (S, 
               (VE,Type.from_RecType rho),
                OG.RECORDatpat(addTypeInfo_RECORD_ATPAT(okConv i, 
                                                        Type.from_RecType rho),
                               SOME(out_patrow))) 
            end

          (* Parenthesised pattern *)                           (*rule 37*)
        | IG.PARatpat(i, pat) =>
            let val (S, (VE,tau), out_pat) = elab_pat(C, pat)
            in (S, (VE,tau), OG.PARatpat(okConv i,out_pat)) end

    (****** pattern rows ******)

    and elab_patrow (C : Context, patrow: IG.patrow)
          : (Substitution * (VarEnv * RecType) * OG.patrow) =
      case patrow of

           (* Pattern row *)                                    (*rule 39*)
           IG.PATROW(i, lab, pat, NONE) =>
             let
               val (S, (VE, tau), out_pat) = elab_pat(C, pat)
             in
               (S, (VE, Type.RecType.add_field (lab, tau) Type.RecType.empty),
                OG.PATROW(okConv i, lab, out_pat, NONE)
               )
             end

         | IG.PATROW(i, lab, pat, SOME patrow) =>
             let
               val (S, (VE, tau), out_pat) = elab_pat(C, pat)
               val (S', (VE', rho), out_patrow) = elab_patrow(C, patrow)
               val intdom = EqSet.intersect (VE.dom VE) (VE.dom VE')
             in
               case (EqSet.isEmpty intdom, 
                     ListHacks.member lab (Type.RecType.sorted_labs rho)) of
                 (true, false) =>
                   (S' oo S,
                    (VE.plus (VE, VE'), Type.RecType.add_field (lab, tau) rho
                     ), OG.PATROW(okConv i, lab, out_pat, SOME out_patrow))
               | (true, true) => 
                   (Substitution.Id,
                    (VE', rho),
                    OG.PATROW(repeatedIdsError(i,[ErrorInfo.LAB_RID lab]),
                              lab, out_pat, SOME out_patrow))
               | (false, false) =>
                   (Substitution.Id,
                    (VE', Type.RecType.add_field (lab, tau) rho),
                    OG.PATROW(repeatedIdsError(i, 
                                  map ErrorInfo.ID_RID (EqSet.list intdom)),
                              lab, out_pat, SOME out_patrow))
               | (false, true) => 
                   (Substitution.Id,
                    (VE', rho),
                    OG.PATROW(repeatedIdsError(i, 
                               (map ErrorInfo.ID_RID (EqSet.list intdom)) @
                               [ErrorInfo.LAB_RID lab]),
                              lab, out_pat, SOME out_patrow))
             end

        | IG.DOTDOTDOT i => (* Flexible record treatment... *)  (*rule 38*)
            let 
              val rho = Type.RecType.dotdotdot ()
            in
              (Substitution.Id,
               (VE.empty, rho),
               OG.DOTDOTDOT(preOverloadingConv(i,
                   OverloadingInfo.UNRESOLVED_DOTDOTDOT rho)))
            end

    (****** patterns - Definition, p. ? ******)

    and elab_pat (C : Context, pat : IG.pat)
      : (Substitution * (VarEnv * Type) * OG.pat) =
      let
        val _ =
          if !Flags.DEBUG_ELABDEC then
            pr("elab_pat: ", IG.layoutPat pat)
          else ()

        val (S, (VE, ty), pat') = elab_pat'(C, pat)

        val _ =
          if !Flags.DEBUG_ELABDEC then
            let 
              val t = StringTree.NODE{start="{", finish="}", indent=0,
                              children=[VE.layout VE,
                                        Type.layout ty
                                       ],
                              childsep=StringTree.RIGHT "; "
                             }
            in
              pr("giving:   ", t)
            end
              else ()
      in
        (S, (VE, ty), pat')
      end

    and elab_pat'(C, pat) =
        case pat of

          (* Atomic pattern *)                                  (*rule 40*)
          IG.ATPATpat(i, atpat) =>
            let val (S, (VE,tau), out_atpat) = elab_atpat(C, atpat)
            in (S, (VE,tau), OG.ATPATpat(okConv i,out_atpat)) end

          (* Constructed pattern *)                             (*rule 41*)
        | IG.CONSpat(i, IG.OP_OPT(longid, withOp), atpat) =>
            let
              val (S, (VE,tau'), out_atpat) = elab_atpat(C, atpat)
            in
              case C.lookup_longid C longid of

                SOME(VE.LONGCON sigma) =>
                  let
                    val new = Type.fresh_normal ()
                    val arrow = Type.mk_Arrow(tau', new) 
                    val (tau1,instances) = (TypeScheme.instance' sigma)
                    val (S1, i') = UnifyWithTexts("argument to long value constructor \
                                                  \in pattern suggests constructor type",
                                                  arrow, 
                                                  "but constructor has type", tau1, i)
                    val tau2 = S1 on new
                  in
                    (S1 oo S, (S1 onVE VE, tau2),
                     OG.CONSpat(addTypeInfo_CON(i', C, instances, longid),
                                OG.OP_OPT(longid, withOp),
                                out_atpat
                                )
                    )
                  end

              | SOME(VE.LONGEXCON tau) =>
                  let
                    val arrow = Type.mk_Arrow(tau',Type.Exn)
                    val (S1, i') = UnifyWithTexts("argument to long \
                          \exception constructor in pattern requires exception \
                          \constructor type ", arrow, 
                          "but the exception constructor has type", tau, i)
                  in
                    (S1 oo S,
                     (S1 onVE VE,Type.Exn),
                     OG.CONSpat(addTypeInfo_EXCON(i',S1 on arrow,longid),
                                OG.OP_OPT(longid, withOp),
                                    out_atpat
                               )
                    )
                  end

              | _ => (* Mark the error. *)
                  (Substitution.Id,
                   (VE, Type_bogus ()),
                   OG.CONSpat(lookupIdError(i, longid),
                              OG.OP_OPT(Ident.bogus, withOp),
                              out_atpat
                             )
                  )
            end

          (* Typed pattern *)                                   (*rule 42*)
        | IG.TYPEDpat(i, pat, ty) =>
            let val (S, (VE,tau), out_pat) = elab_pat(C, pat)
            in case elab_ty(C, ty)
                 of (SOME tau', out_ty) =>
                   let val (S', i') = UnifyWithTexts("pattern has type", tau, "which conflicts \
                                                     \with your type constraint", tau', i)
                       val i'' = addTypeInfo_VAR_PAT(i', tau')  (* rowan 25jul01 *)
                       val S'' = S' oo S
                   in (S'', (S'' onVE VE, S'' on tau), OG.TYPEDpat(i'', out_pat, out_ty))
                   end
                  | (NONE, out_ty) => (S, (VE, tau), OG.TYPEDpat(okConv i, out_pat, out_ty))
            end

        | IG.SORTEDpat(i, pat, ty::tys) =>
             let val (S, (VE,tau), out_pat) = elab_pat(C, pat)

                 val (tau_opt, out_ty) = elab_srt(S onC C, ty)
		 fun do_elab ty = elab_srt (S onC C, ty)
		 val pairs = map do_elab tys
		 val out_tys = map #2 pairs
		 val tau_opts = map #1 pairs
		 fun find_different (SOME tau1) (SOME tau2::tail) = 
 		     if Type.eq (tau1, tau2) then find_different (SOME tau1) tail
		     else SOME (tau1, tau2)
		   | find_different _ _ = NONE
				 
             in case find_different tau_opt tau_opts of
		    SOME (tau1, tau2) =>
                      (S, (VE, tau), 
		       OG.SORTEDpat(errorConv (i, ErrorInfo.INCOMPATIBLE_TY(tau1, tau2)), 
				    out_pat, out_ty::out_tys))
		  | NONE => 

		 case tau_opt
                  of SOME tau' =>
                    let val (S', i') = UnifyWithTexts("pattern has type", tau, "which conflicts \
                                                      \with your type constraint", tau', i)
                        val i'' = addTypeInfo_VAR_PAT(i', tau')  (* rowan 25jul01 *)
                        val S'' = S' oo S
                    in (S'', (S'' onVE VE, S'' on tau), OG.SORTEDpat(i'', out_pat, out_ty::out_tys))
                    end
                   | NONE => (S, (VE, tau), OG.SORTEDpat(okConv i, out_pat, out_ty::out_tys))
	     end

(*      | IG.SORTEDpat(i, pat, [ty]) =>
            let val (S, (VE,tau), out_pat) = elab_pat(C, pat)
            in case elab_srt(C, ty)
                 of (SOME tau', out_ty) =>
                   let val (S', i') = UnifyWithTexts("pattern has type", tau, "which conflicts \
                                                     \with your type constraint", tau', i)
                       val i'' = addTypeInfo_VAR_PAT(i', tau')  (* rowan 25jul01 *)
                       val S'' = S' oo S
                   in (S'', (S'' onVE VE, S'' on tau), OG.SORTEDpat(i'', out_pat, [out_ty]))
                   end
                  | (NONE, out_ty) => (S, (VE, tau), OG.SORTEDpat(okConv i, out_pat, [out_ty]))
            end
*)
          (* Layered pattern *)                                 (*rule 43*)
        | IG.LAYEREDpat(i, IG.OP_OPT(id, withOp), NONE, pat) =>
            let
              val (S, (VE1, tau), out_pat) = elab_pat(C, pat)
              val VE2 = VE.singleton_var(id, TypeScheme.from_Type tau)
              val intdom = EqSet.intersect (VE.dom VE1) (VE.dom VE2)
              val VE3 = VE.plus (VE1, VE2)
            in
              if EqSet.isEmpty intdom then
                (S, (VE3, tau), 
                 OG.LAYEREDpat(addTypeInfo_VAR_PAT(okConv i,tau), 
                               OG.OP_OPT(id, withOp),
                               NONE, out_pat))
              else
                (S, (VE3, tau),
                 OG.LAYEREDpat(repeatedIdsError(i, map ErrorInfo.ID_RID 
                                                   (EqSet.list intdom)),
                               OG.OP_OPT(id, withOp),
                               NONE, out_pat))
            end

        | IG.LAYEREDpat(i, IG.OP_OPT(id, withOp), SOME ty, pat) =>
            let val (S, (VE1, tau), out_pat) = elab_pat(C, pat)
            in case elab_ty(C, ty)
                 of (SOME tau', out_ty) =>
                   let val (S', i') = UnifyWithTexts("pattern has type", tau, 
                                                     "which conflicts with your constraint", tau', i)
                       val i' = addTypeInfo_VAR_PAT(i', tau') (*added, mads*)
                       val S'' = S' oo S
                       val VE2 = VE.singleton_var (id, TypeScheme.from_Type tau)
                       val intdom = EqSet.intersect (VE.dom VE1) (VE.dom VE2)
                       val VE3 = VE.plus (VE1, VE2)
                   in
                     if EqSet.isEmpty intdom then
                       (S'',
                        (S'' onVE VE3, S'' on tau),
                        OG.LAYEREDpat(i', OG.OP_OPT(id, withOp), SOME out_ty, out_pat)
                        )
                     else
                       (S'', 
                        (S'' onVE VE3, S'' on tau),
                        OG.LAYEREDpat(repeatedIdsError(i, map ErrorInfo.ID_RID (EqSet.list intdom)),
                                      OG.OP_OPT(id, withOp), SOME out_ty, out_pat)
                        )
                   end
                  | (NONE, out_ty) => 
                   (S, (VE1, tau), OG.LAYEREDpat(okConv i, OG.OP_OPT(id, withOp), SOME out_ty, out_pat))
            end

        | IG.UNRES_INFIXpat _ =>
            impossible "elab_pat(UNRES_INFIX)"

    (****** types  ******)

                (* elab_ty returns `NONE' if an error occurred when elborating the
                 * type expression. The reason we do things this way is that
                 * errors are dealt with in two different ways depending on the
                 * construct the type expression is part of. *)

(*
    and elab_ty (C : Context, ty : IG.ty) : (Type option * OG.ty) =
	(elab_ty_allow_intersect := false;
         elab_ty0 (C, ty) )
*)

    and elab_srt (C : Context, ty : IG.ty) : (Type option * OG.ty) =
	let val keep = !elab_ty_allow_intersect
	    val _ = elab_ty_allow_intersect := true
	    val res = elab_ty (C, ty)
	    val _ = elab_ty_allow_intersect := keep
	in
	    res
	end

    and elab_ty (C : Context, ty : IG.ty) : (Type option * OG.ty) =

        case ty of

          (* Explicit type variable *)                          (*rule 44*)
          IG.TYVARty(i, ExplicitTyVar) =>
(*          let val ty = C.ExplicitTyVar_lookup C ExplicitTyVar
                         handle Match => Crash.impossible "ElabDec.elab_ty: TYVAR"
            in  (SOME ty, OG.TYVARty(addTypeInfo_EXP(okConv i, ty), ExplicitTyVar))
            end   Swapped back to original code - Rowan 18sep04 *)

            let val ty_opt = SOME (C.ExplicitTyVar_lookup C ExplicitTyVar) 
                             handle Match => NONE
                val ty = case ty_opt of SOME ty => ty | NONE => Type_bogus ()  (* for RefDec *)
            in (ty_opt, OG.TYVARty(addTypeInfo_EXP(okConv i, ty), ExplicitTyVar))
            end

          (* Record type *)                                     (*rule 45*)
        | IG.RECORDty(i, NONE) =>  (SOME Type.Unit, OG.RECORDty (okConv i, NONE))

          (* Record type *)
        | IG.RECORDty(i, SOME tyrow) =>  (* The error has already been reported. *)
           (case elab_tyrow(C, tyrow)
              of (SOME rho, out_tyrow) => (SOME (Type.from_RecType rho), OG.RECORDty(okConv i, SOME out_tyrow))
               | (NONE, out_tyrow) => (NONE, OG.RECORDty(okConv i, SOME out_tyrow)))


        (* Constructed type *)                                  (*rule 46*)
        | IG.CONty(i, ty_list, longtycon) =>
            let
              val res_list = map (fn ty => elab_ty (C, ty)) ty_list
              val tau_opt_list = map #1 res_list
              val out_ty_list = map #2 res_list
              fun unopt_list ([],a) = SOME (rev a)
                | unopt_list (NONE::rest,a) = NONE
                | unopt_list (SOME tau::rest, a) = unopt_list (rest, tau::a)
            in
              case unopt_list (tau_opt_list, [])
                of SOME tau_list =>
                  (case C.lookup_longtycon C longtycon of
                     SOME tystr =>
                       let
                         val (typeFcn, _) = TyStr.to_theta_and_VE tystr
                         val expectedArity = TypeFcn.arity typeFcn
                         val actualArity = length tau_list
                         val _ = debug_pr_msg "elab_ty(CONty)"
                       in
                         if expectedArity = actualArity then
                           (SOME(TypeFcn.apply (typeFcn, tau_list)),
                            OG.CONty (okConv i, out_ty_list, longtycon))
                         else
                           (NONE,
                            OG.CONty (errorConv (i, ErrorInfo.WRONG_ARITY
                                                 {expected=expectedArity,
                                                  actual=actualArity}),
                                      out_ty_list, longtycon))
                       end
                   | NONE => (NONE, OG.CONty(lookupTyConError(i, longtycon), out_ty_list, longtycon)))
                 | NONE => (NONE, OG.CONty(okConv i, out_ty_list, longtycon))  
            end

          (* Intersection type - only allow for sorts?  Use modules/fn-arg/bool-arg to achieve this*)
        | IG.INTERty(i, ty, ty') =>
            (case (elab_ty(C, ty ), elab_ty(C, ty'))
               of ((SOME tau, out_ty), (SOME tau', out_ty')) =>
		  if Type.eq (tau, tau') then
		      if !elab_ty_allow_intersect then
			  (SOME tau, OG.INTERty(okConv i, out_ty, out_ty'))
		      else 
			  (NONE, OG.INTERty(errorConv (i, ErrorInfo.INTERSECTION_IN_TYPE), 
					    out_ty, out_ty'))
                  else 
                    (NONE, OG.INTERty(errorConv (i, ErrorInfo.INCOMPATIBLE_TY(tau, tau')), 
                                      out_ty, out_ty'))
                | ((_, out_ty), (_, out_ty')) => (NONE, OG.INTERty(okConv i, out_ty, out_ty')))
          

          (* Function type *)                                   (*rule 47*)
        | IG.FNty(i, ty, ty') =>
            (case (elab_ty(C, ty ), elab_ty(C, ty'))
               of ((SOME tau, out_ty), (SOME tau', out_ty')) =>
                 (SOME (Type.mk_Arrow (tau, tau')), OG.FNty(okConv i, out_ty, out_ty'))
                | ((_, out_ty), (_, out_ty')) => (NONE, OG.FNty(okConv i, out_ty, out_ty')))

          (* Parenthesised type *)                              (*rule 48*)
        | IG.PARty(i, ty) =>
            let val (tau_opt, out_ty) = elab_ty(C, ty)
            in
              (tau_opt, OG.PARty(okConv i, out_ty))
            end


    (****** type rows ******)

    and elab_tyrow (C : Context, IG.TYROW(i, lab, ty, tyrow_opt)) : (RecType option * OG.tyrow) =
      case (elab_ty(C, ty), elab_tyrow_opt(C, tyrow_opt))
        of ((SOME tau, out_ty), (SOME rho', out_tyrow_opt)) => 
          let
            val (rho, i') = 
              if (ListHacks.member lab (Type.RecType.sorted_labs rho')) then
                (rho', repeatedIdsError(i, [ErrorInfo.LAB_RID lab]))
              else (Type.RecType.add_field (lab,tau) rho', okConv i)
          in (SOME rho, OG.TYROW(i', lab, out_ty, out_tyrow_opt))
          end

         | ((NONE, out_ty), (SOME rho', out_tyrow_opt)) =>
          (SOME rho', OG.TYROW(okConv i, lab, out_ty, out_tyrow_opt))

         | ((_, out_ty), (_, out_tyrow_opt)) => 
          (NONE, OG.TYROW(okConv i, lab, out_ty, out_tyrow_opt))
          
    and elab_tyrow_opt(C, NONE) = (SOME Type.RecType.empty, NONE)
        | elab_tyrow_opt(C, SOME tyrow) =
        case elab_tyrow(C, tyrow)
          of (SOME rho, out_tyrow) => (SOME rho, SOME out_tyrow)
           | (NONE, out_tyrow) => (NONE, SOME out_tyrow)

    and elab_ty_ok ty = 
      case ty of
	IG.TYVARty(i,tyvar) => OG.TYVARty(okConv i, tyvar)
      | IG.RECORDty(i,tyrow_opt) => OG.RECORDty(okConv i, elab_tyrow_opt_ok tyrow_opt)
      | IG.CONty(i,tys,longtycon) => OG.CONty(okConv i, map elab_ty_ok tys,longtycon)
      | IG.INTERty(i,ty,ty') => OG.INTERty(okConv i, elab_ty_ok ty, elab_ty_ok ty')
      | IG.FNty(i,ty,ty') => OG.FNty(okConv i, elab_ty_ok ty, elab_ty_ok ty')
      | IG.PARty(i,ty) => OG.PARty(okConv i, elab_ty_ok ty)

    and elab_tyrow_ok (IG.TYROW(i,lab,ty,tyrow_opt)) : OG.tyrow =
      OG.TYROW(okConv i, lab, elab_ty_ok ty, elab_tyrow_opt_ok tyrow_opt)

    and elab_tyrow_opt_ok NONE = NONE
      | elab_tyrow_opt_ok (SOME tyrow) = SOME (elab_tyrow_ok tyrow)



(**** Overloading resolution ****)  

fun resolve_overloading (S : Substitution, dec : OG.dec): OG.dec =

    (* resolves overloading in dec, by applying S on every recorded
       overloaded type variable --- if repeated application of S 
       yields int or real, we record this information in the info
       field; otherwise overloading cannot be resolved
       and error-info is inserted in the info-field. 
     *)

let
  open OG 

  local
  val tau_to_overloadinginfo_alist =
        [(Type.Int,    OverloadingInfo.RESOLVED_INT),
         (Type.Real,   OverloadingInfo.RESOLVED_REAL),
         (Type.String, OverloadingInfo.RESOLVED_STRING),
         (Type.Char,   OverloadingInfo.RESOLVED_CHAR),
         (Type.Word8,  OverloadingInfo.RESOLVED_WORD8),
         (Type.Word,   OverloadingInfo.RESOLVED_WORD)]
        
  fun tau_to_overloadinginfo tau  =
    case List.find (fn (tau', oi) => Type.eq (tau, tau')) tau_to_overloadinginfo_alist
      of SOME res => SOME(#2 res)
       | NONE => NONE

  fun resolve_tau (default_type, default : OverloadingInfo.OverloadingInfo) tau 
    : OverloadingInfo.OverloadingInfo =
        let val tau' = S on tau
        in
          if !Flags.DEBUG_ELABDEC then
            (pr("res: tv is: ", Type.layout tau);
             pr("res:  S on tv yields type: ", Type.layout tau'))
          else ();
          (case Type.to_TyVar tau' of
             NONE => (case tau_to_overloadinginfo tau'
                        of SOME res => res
                         | NONE => default) 
                 (*TODO 25/06/1997 10:11. tho.
                  I'd rather do an impossible here: If tau' is not a
                  tyvar, it must be one of int, real, string, char, &
                  word; everything else would be a type error.  Well,
                  perhaps it can occur then, namely when there is a
                  type error (they do occur), and since type errors
                  should not make the compiler crash, it is probably
                  best to not do an impossible.  The only thing to do
                  is then to return `default', as unresolved
                  overloading should not result in an error message. *)
           | SOME tv' =>
               if Type.eq (tau', tau)
               then (*tau' is a tyvar, so the overloading is as yet
                     unresolved, & according to the Definition of SML '97, it
                     must be resolved to a default type.  And now someone has remembered
                     to put this resolve into work by unifying the tyvar with
                     the default type:*)
                    (if !Flags.DEBUG_ELABDEC
                     then TextIO.output (TextIO.stdOut, "res: SOME tv\n") else () ;
                     (case Type.unify (default_type, tau') of
                        Type.UnifyOk => ()
                      | _ => () (*impossible "resolve_tau: unify" *) ) ;
                     default)
               else (*repeat application of S:*) resolve_tau (default_type, default) tau')
        end
  in

  (*resolve_tyvar gives `default' overloading info when overloading couldn't be
   resolved.  According to the definition (p. 72), int is the default type
   except for /, but / is not overloaded in this compiler; / always has type
   real * real -> real, as there is only one kind of real.
   25/06/1997 10:30. tho.*)

    fun resolve_tyvar (default_type, default: OverloadingInfo.OverloadingInfo) 
      : TyVar -> OverloadingInfo.OverloadingInfo =
      (resolve_tau (default_type,default)) o Type.from_TyVar

  end (*local*)

  datatype flexresResult = FLEX_RESOLVED | FLEX_NOTRESOLVED
  fun flexrecres (rho : RecType) : flexresResult =
        let
          fun loop typ = 
                let val typ' = S on typ
                in
                  if !Flags.DEBUG_FLEXRECORDS then 
                    (pr("flexrecres: typ = ", Type.layout typ);
                     pr("flexrecres: typ' = ", Type.layout typ'))
                  else ();
                  if Type.eq (typ',typ) then typ else loop typ'
                end
        in
          if Type.contains_row_variable (loop (Type.from_RecType rho))
          then FLEX_NOTRESOLVED else FLEX_RESOLVED
        end

  local
    open TypeInfo 
    infixr on_TypeInfo

    fun S on_TypeInfo (LAB_INFO {index}) = LAB_INFO {index=index}
      | S on_TypeInfo (RECORD_ATPAT_INFO {Type}) = 
            RECORD_ATPAT_INFO {Type=S on Type}
      | S on_TypeInfo (VAR_INFO {instances}) = 
            VAR_INFO {instances=map (fn tau => S on tau) instances}
      | S on_TypeInfo (VAR_PAT_INFO {tyvars,Type}) =
            VAR_PAT_INFO {tyvars=tyvars,Type=S on Type}
      | S on_TypeInfo (CON_INFO {numCons,index,longid,instances}) = 
            CON_INFO {numCons=numCons,index=index,longid=longid,
                      instances= map (fn tau => S on tau) instances}
      | S on_TypeInfo (EXCON_INFO {Type,longid}) = 
            EXCON_INFO {Type=S on Type,longid=longid}
      | S on_TypeInfo (EXBIND_INFO {TypeOpt=NONE}) = EXBIND_INFO {TypeOpt=NONE}
      | S on_TypeInfo (EXBIND_INFO {TypeOpt=SOME Type}) = 
            EXBIND_INFO {TypeOpt=SOME (S on Type)}   
      | S on_TypeInfo (TYENV_INFO TE) = TYENV_INFO TE                  (*no free tyvars here*)
      | S on_TypeInfo (ABSTYPE_INFO (TE,rea)) = ABSTYPE_INFO (TE,rea)  (*no free tyvars here*)
      | S on_TypeInfo (DATBIND_INFO {TyName}) = DATBIND_INFO {TyName=TyName}
      | S on_TypeInfo (EXP_INFO {Type}) = 
            EXP_INFO {Type=S on Type}
      | S on_TypeInfo (MATCH_INFO {Type}) = 
            MATCH_INFO {Type=S on Type}
      | S on_TypeInfo (PLAINvalbind_INFO {tyvars,Type, Uenv}) =
            PLAINvalbind_INFO {tyvars=tyvars, Type=S on Type, Uenv=Uenv}
      | S on_TypeInfo (OPEN_INFO i) = OPEN_INFO i  (*only identifiers*)
      | S on_TypeInfo (INCLUDE_INFO i) = INCLUDE_INFO i  (*only identifiers*)
      | S on_TypeInfo (FUNCTOR_APP_INFO rea) = 
            FUNCTOR_APP_INFO rea   (* type functions are closed *)
      | S on_TypeInfo (FUNBIND_INFO E) = FUNBIND_INFO E (* signatures are closed *)
      | S on_TypeInfo (TRANS_CONSTRAINT_INFO E) =
            TRANS_CONSTRAINT_INFO E (* signatures are closed *)
      | S on_TypeInfo (OPAQUE_CONSTRAINT_INFO E_and_phi) =
            OPAQUE_CONSTRAINT_INFO E_and_phi (* signatures and realisations are closed *)
      | S on_TypeInfo (SIGBIND_INFO _) = impossible "on_TypeInfo.SIGBIND_INFO"
      | S on_TypeInfo (DELAYED_REALISATION _) = impossible "on_TypeInfo.DELAYED_REALISATION"
  in
    fun resolve_i ElabInfo =
          (case ElabInfo.to_TypeInfo ElabInfo of
             SOME typeinfo =>
               ElabInfo.plus_TypeInfo ElabInfo (S on_TypeInfo typeinfo)
           | NONE => ElabInfo)

  end (*local open TypeInfo ...*)

  exception DDD_IS_EMPTY (* raised by handling of ... when ... stands for the empty set of labels *)

  (*resolve_X X: apply resolve_i to all info fields i in X and resolve_tyvar to
   all overloadinginfos on id's in X, and also do something about flex
   records.*)

  fun resolve_atexp (atexp : atexp) : atexp =
      case atexp of
          SCONatexp(i,scon) =>
            (case ElabInfo.to_OverloadingInfo i 
               of NONE => SCONatexp(resolve_i i, scon)
                | SOME (OverloadingInfo.UNRESOLVED_IDENT tyvar) =>
                 SCONatexp (ElabInfo.plus_OverloadingInfo i (resolve_tyvar (Type.Word, OverloadingInfo.RESOLVED_WORD) tyvar), 
                            scon)
                 | SOME _ => impossible "resolve_atexp.SCON")
        | IDENTatexp(i, op_opt) =>
              (case ElabInfo.to_OverloadingInfo i of 
                   NONE => IDENTatexp (resolve_i i, op_opt)
                 | SOME (OverloadingInfo.UNRESOLVED_IDENT tyvar) =>
                     IDENTatexp
                       (ElabInfo.plus_OverloadingInfo i (resolve_tyvar (Type.Int, OverloadingInfo.RESOLVED_INT) tyvar), 
                        op_opt)
                 | SOME _ => impossible "resolve_atexp")
        | RECORDatexp(i, NONE) => RECORDatexp(resolve_i i,NONE)
        | RECORDatexp(i, SOME exprow) =>
              RECORDatexp(resolve_i i, SOME (resolve_exprow exprow))
        | LETatexp(i, dec, exp) =>
              LETatexp(resolve_i i, resolve_dec dec, resolve_exp exp)
        | PARatexp(i, exp) =>
              PARatexp(resolve_i i, resolve_exp exp)
              
  and resolve_exprow (exprow: exprow) : exprow =
      case exprow of 
          EXPROW(i, l, exp, NONE) =>
              EXPROW(resolve_i i, l, resolve_exp exp, NONE)
        | EXPROW(i, l, exp, SOME exprow) =>
              EXPROW(resolve_i i, l, resolve_exp exp, SOME (resolve_exprow exprow))
              
  and resolve_exp (exp: exp) : exp =
      case exp of
          ATEXPexp(i, atexp) => 
              ATEXPexp(resolve_i i, resolve_atexp atexp)
        | APPexp(i, exp, atexp) => 
              APPexp(resolve_i i, resolve_exp exp, resolve_atexp atexp)
        | TYPEDexp(i, exp, ty) =>
              TYPEDexp(resolve_i i, resolve_exp exp, ty)
        | SORTEDexp(i, exp, tys) =>
              SORTEDexp(resolve_i i, resolve_exp exp, tys)
        | HANDLEexp(i, exp, match) =>
              HANDLEexp(resolve_i i, resolve_exp exp, resolve_match match)
        | RAISEexp(i, exp) => 
              RAISEexp(resolve_i i, resolve_exp exp)
        | FNexp(i, match) =>
              FNexp(resolve_i i, resolve_match match)
        | UNRES_INFIXexp _ =>
              impossible "resolve_exp(UNRES_INFIX)"

  and resolve_match (match: match) : match =
      case match of 
          MATCH(i, mrule, NONE) => 
              MATCH(resolve_i i, resolve_mrule mrule, NONE)
        | MATCH(i, mrule, SOME match) =>
              MATCH(resolve_i i, resolve_mrule mrule, SOME (resolve_match match))

  and resolve_mrule (MRULE(i, pat, exp) : mrule) : mrule =
      MRULE(resolve_i i, resolve_pat pat, resolve_exp exp)
      
  and resolve_dec (dec : dec) : dec =
        (case dec of 
           VALdec(i, tyvars, valbind) =>
             VALdec(resolve_i i, tyvars, resolve_valbind valbind)
         | VALsdec(i, valsdesc) => dec
         | UNRES_FUNdec _ => impossible "resolve_dec(UNRES_FUNdec)"
         | TYPEdec _ => dec
         | SORTdec _ => dec
         | DATATYPEdec(i,datbind) => DATATYPEdec(resolve_i i,datbind)
         | DATATYPE_REPLICATIONdec(i, tycon, longtycon) => 
             DATATYPE_REPLICATIONdec(resolve_i i, tycon, longtycon)
         | DATASORTdec(i,datbind) => DATASORTdec(resolve_i i,datbind)
         | ABSTYPEdec(i, datbind, dec) =>
             ABSTYPEdec(resolve_i i, datbind, resolve_dec dec)
         | EXCEPTIONdec(i,exbind) => EXCEPTIONdec(resolve_i i, exbind)
         | LOCALdec(i, dec1, dec2) =>
             LOCALdec(resolve_i i, resolve_dec dec1, resolve_dec dec2)
         | OPENdec _ => dec
         | SEQdec(i, dec1, dec2) =>
             SEQdec(resolve_i i, resolve_dec dec1, resolve_dec dec2)
         | INFIXdec _ => dec
         | INFIXRdec _ => dec
         | NONFIXdec _ => dec
         | EMPTYdec _ => dec)

  and resolve_valbind (valbind : valbind) : valbind =
      case valbind of
          PLAINvalbind(i, pat, exp, NONE) =>
              PLAINvalbind(i, resolve_pat pat, resolve_exp exp, NONE)
        | PLAINvalbind(i, pat, exp, SOME valbind) =>
              PLAINvalbind(i, resolve_pat pat, 
                           resolve_exp exp, SOME (resolve_valbind valbind))
        | RECvalbind(i, valbind) =>
              RECvalbind(i, resolve_valbind valbind)
      
  and resolve_atpat (atpat : atpat) : atpat =
    case atpat of
      WILDCARDatpat _ => atpat
    | SCONatpat(i,scon) =>
        (case ElabInfo.to_OverloadingInfo i 
           of NONE => SCONatpat(resolve_i i, scon)
            | SOME (OverloadingInfo.UNRESOLVED_IDENT tyvar) =>
             SCONatpat (ElabInfo.plus_OverloadingInfo i (resolve_tyvar (Type.Word, OverloadingInfo.RESOLVED_WORD) tyvar), 
                        scon)
            | SOME _ => impossible "resolve_atpat.SCON")

    | LONGIDatpat(i,x) => LONGIDatpat(resolve_i i,x)
    | RECORDatpat(i, NONE) => RECORDatpat(resolve_i i,NONE)
    | RECORDatpat(i, SOME patrow) =>
        let
          val i' = resolve_i i 
        in let  val patrow' = resolve_patrow patrow
           in
             case ElabInfo.to_TypeInfo i' of
               SOME typeinfo =>
                 (case typeinfo of
                    TypeInfo.RECORD_ATPAT_INFO{Type} => 
                      (* Type has been resolved, c.f. i' *)
                      RECORDatpat(i',SOME (addLabelIndexInfo(Type,patrow')))
                  | _ => impossible ("resolve_atpat(RECORDatpat): " ^ 
                                     "wrong typeinfo"))
             | NONE => impossible ("resolve_atpat(RECORDatpat): " ^ 
                                   "no typeinfo")
           end handle DDD_IS_EMPTY => RECORDatpat(i',NONE)
        end
    | PARatpat(i, pat) =>
        PARatpat(resolve_i i, resolve_pat pat)

  and resolve_patrow (patrow : patrow): patrow  =
    case patrow of
      DOTDOTDOT(i) => 
        (case (ElabInfo.to_OverloadingInfo i) of 
           NONE => patrow
         | SOME (OverloadingInfo.UNRESOLVED_DOTDOTDOT rho) =>
             (case flexrecres rho of
                FLEX_RESOLVED => (* old: DOTDOTDOT (ElabInfo.remove_OverloadingInfo i)*)
                   (* expand "..." into a patrow with ordinary wildcards (_) *)
                   let val i0 = ElabInfo.from_ParseInfo(ElabInfo.to_ParseInfo i)
                       fun wild ty = ATPATpat(i0, WILDCARDatpat(ElabInfo.plus_TypeInfo i0 (TypeInfo.VAR_PAT_INFO{tyvars=[], Type= ty})))
                       val labs_and_types = Type.RecType.to_list rho
                       fun f ((lab,ty), acc) = SOME(PATROW(ElabInfo.plus_TypeInfo i0 (TypeInfo.LAB_INFO{index=0}),lab, wild ty, acc))
                   in case foldr f NONE labs_and_types of
                        SOME patrow' => patrow'
                      | NONE => raise DDD_IS_EMPTY (* caller of resolve_patrow should replace SOME(...) by NONE *)
                   end
              | FLEX_NOTRESOLVED =>
                  DOTDOTDOT(plus_ErrorInfo i ErrorInfo.FLEX_REC_NOT_RESOLVED))
         | SOME _ => impossible "resolve_patrow")
    | PATROW(i, lab, pat, NONE) => 
        PATROW(resolve_i i, lab, resolve_pat pat, NONE)
    | PATROW(i, lab, pat, SOME patrow) =>
        PATROW(resolve_i i, lab, resolve_pat pat, SOME (resolve_patrow patrow)
                                                  handle DDD_IS_EMPTY => NONE)

  and resolve_pat (pat : pat) : pat =
    case pat of
      ATPATpat(i, atpat) =>
        ATPATpat(resolve_i i, resolve_atpat atpat)
    | CONSpat(i, longidopt, atpat) =>
        CONSpat(resolve_i i, longidopt, resolve_atpat atpat)
    | TYPEDpat(i, pat, ty) =>
        TYPEDpat(resolve_i i, resolve_pat pat, ty)
    | SORTEDpat(i, pat, tys) =>
        SORTEDpat(resolve_i i, resolve_pat pat, tys)
    | LAYEREDpat(i, idopt, tyopt, pat) =>
        LAYEREDpat(resolve_i i, idopt, tyopt, resolve_pat pat)
    | UNRES_INFIXpat _ =>
        impossible "resolve_pat(UNRES_INFIX)"


in
  resolve_dec dec
end (*fun resolve_overloading (ugly)*)

    (****** Elaborate a declaration and resolve overloading ******)

    val elab_dec : (Context * IG.dec) -> (TyName list * Env * OG.dec) =

      fn (C, dec) =>
        let
          val (S, T, E, out_dec) = elab_dec(C, dec)
          val dec' = resolve_overloading (S, out_dec)
        in
          (T, E, dec')
        end

end; 
