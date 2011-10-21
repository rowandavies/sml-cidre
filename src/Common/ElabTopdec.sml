(*************************************************************)
(* Elaboration of top-level declarations, modules included,  *)
(*   Section 5 of the Definition, v3                         *)
(*************************************************************)

functor ElabTopdec
  (structure PrettyPrint : PRETTYPRINT
   structure Report : REPORT
   structure FinMap : FINMAP
   structure Flags : FLAGS
       where type Report = Report.Report 
   structure Crash : CRASH

     
   structure StrId : STRID
   structure SigId : SIGID
   
   structure StatObject : STATOBJECT
   structure SortName : SORTNAME
       where type TyName = StatObject.TyName
       where type sortcon = StatObject.tycon

       
   structure RefObject : REFOBJECT
       where type Type = StatObject.Type
       where type SortName = SortName.SortName
       where type SortName.Set.Set = SortName.Set.Set
       where type TyVar = StatObject.TyVar
       where type trealisation = StatObject.realisation
       where type TyName = StatObject.TyName

   
   structure ParseInfo : PARSE_INFO
   structure ElabInfo : ELAB_INFO 
     where type ParseInfo.ParseInfo = ParseInfo.ParseInfo
     where type ParseInfo.SourceInfo = ParseInfo.SourceInfo

     where type TypeInfo.realisation = StatObject.realisation
     where type TypeInfo.strid = StrId.strid
     where type TypeInfo.TyName.Set.Set = StatObject.TyName.Set.Set
     where type TypeInfo.TyVar = StatObject.TyVar
     where type TypeInfo.tycon = SortName.sortcon
                  
     where type ErrorInfo.RefineErrorInfo.Report = Report.Report 
     where type ErrorInfo.TyName = StatObject.TyName
     where type ErrorInfo.TypeFcn = StatObject.TypeFcn
     where type ErrorInfo.Type = StatObject.Type
     where type ErrorInfo.TypeScheme = StatObject.TypeScheme
     where type ErrorInfo.strid = StrId.strid
     where type ErrorInfo.sigid = SigId.sigid
     where type ErrorInfo.longstrid = StrId.longstrid

         
   structure IG : TOPDEC_GRAMMAR
     where type tyvar = StatObject.ExplicitTyVar
     where type id = ElabInfo.ErrorInfo.id
     where type funid = ElabInfo.ErrorInfo.funid
     where type longtycon = ElabInfo.ErrorInfo.longtycon
     where type strid = StrId.strid
     where type longstrid = StrId.longstrid 
     where type sigid = SigId.sigid
     where type info = ParseInfo.ParseInfo
     where type tycon = SortName.sortcon
     where type DecGrammar.TyVar.Variance = SortName.Variance
   
         
   structure OG : TOPDEC_GRAMMAR
       where type tycon = SortName.sortcon
       where type funid = IG.funid
       where type strid = IG.strid
       where type sigid = SigId.sigid
       where type longstrid = StrId.longstrid
       where type longtycon = ElabInfo.ErrorInfo.longtycon
       where type longid = IG.longid   
    (*           where type DecGrammar.longid = IG.longid *)
        (*   where type tyvar = StatObject.ExplicitTyVar
       where type DecGrammar.tyvar = StatObject.ExplicitTyVar *)
       where type tyvar = StatObject.ExplicitTyVar (* For SML/NJ 110.0.3 *)
       where type id = IG.id
       where type info = ElabInfo.ElabInfo
   
   structure Environments : ENVIRONMENTS
       where type TyName = StatObject.TyName
       where type TyName.Set.Set = StatObject.TyName.Set.Set
       where type TyVar = StatObject.TyVar
       where type Type = StatObject.Type
       where type TypeScheme = StatObject.TypeScheme
       where type Substitution = StatObject.Substitution
       where type TypeFcn = StatObject.TypeFcn
       where type level = StatObject.level
       where type realisation = StatObject.realisation
       where type id = IG.id
       where type strid = IG.strid
       where type tycon = SortName.sortcon
       where type ExplicitTyVar = StatObject.ExplicitTyVar
       where type longtycon = ElabInfo.ErrorInfo.longtycon
       where type longstrid = IG.longstrid
       where type ty = IG.ty
       where type Env = ElabInfo.TypeInfo.Env

   structure ModuleStatObject: MODULE_STATOBJECT
       where type TyName = StatObject.TyName
       where type TyName.Set.Set = StatObject.TyName.Set.Set
       where type Env = Environments.Env
       where type realisation = StatObject.realisation
   (*     where type .StringTree = PrettyPrint.StringTree *)
       where type SigMatchError = ElabInfo.ErrorInfo.SigMatchError
   
   structure ModuleEnvironments : MODULE_ENVIRONMENTS
       where type TyName = StatObject.TyName
 (*      where type TyName.Set.Set = StatObject.TyName.Set.Set *)
       where type TyVar = StatObject.TyVar
       where type TyStr = Environments.TyStr
       where type Env = Environments.Env
       where type Sig = ModuleStatObject.Sig
       where type FunSig = ModuleStatObject.FunSig
       where type Context = Environments.Context
       where type realisation = StatObject.realisation
       where type id = IG.id
       where type strid = IG.strid
       where type tycon = IG.tycon
       where type longstrid = IG.longstrid
       where type longtycon = ElabInfo.ErrorInfo.longtycon
       where type sigid = IG.sigid
       where type funid = IG.funid
       where type Basis = ElabInfo.TypeInfo.Basis

       where type rEnv = ModuleStatObject.rEnv
       where type rTyNameEnv = ModuleStatObject.rT

       (* where type rEnv.tycon = IG.tycon *)
       (* where type rEnv.longtycon = ElabInfo.ErrorInfo.longtycon *)
       (* where type rEnv.id = IG.id *)
       (* where type rEnv.strid = StrId.strid *)


       where type rEnv.ExplicitTyVar = StatObject.ExplicitTyVar

       where type rEnv.SortName = SortName.SortName
       where type rEnv.SortFcn = RefObject.SortFcn
       where type rEnv.Sort = RefObject.Sort
       where type rEnv.Variance = SortName.Variance
       where type rEnv.SortVar = RefObject.SortVar
       where type rEnv.SortScheme = RefObject.SortScheme



   structure ElabDec : ELABDEC
     where type PreElabDec = IG.dec
     where type PostElabDec = OG.dec
     where type PreElabTy = IG.ty
     where type PostElabTy = OG.ty
     where type PreElabDatBind = IG.DecGrammar.datbind
     where type PostElabDatBind = OG.DecGrammar.datbind
     where type Env = Environments.Env
     where type TyEnv = Environments.TyEnv
     where type Context = Environments.Context
     where type Type = StatObject.Type
     where type TyName = StatObject.TyName

   
   structure RefDec : REFDEC  
       where type Comp.Error = ElabInfo.ErrorInfo.RefineErrorInfo.Error

       where type Context = ModuleEnvironments.rContext
       where type TyNameEnv = ModuleEnvironments.rEnv.TyNameEnv
       where type Env = ModuleEnvironments.rEnv
       where type PostElabDec = OG.dec
       where type PostElabTy = OG.ty
       where type PostElabDatBind = OG.DecGrammar.datbind
       where type Sort = RefObject.Sort
       
  
     ) : ELABTOPDEC  =
  struct

    structure StdList = List
    structure List = Edlib.List

    fun impossible s = Crash.impossible ("ElabTopdec." ^ s)
    fun noSome NONE s = impossible s
      | noSome (SOME x) s = x
    fun quote s = "`" ^ s ^ "'"
    fun is_Some NONE = false
      | is_Some (SOME x) = true
    val StringTree_to_string = PrettyPrint.flatten1
    fun pp_list pp_x [] = ""
      | pp_list pp_x [x] = pp_x x
      | pp_list pp_x [x,x'] = pp_x x ^ " and " ^ pp_x x'
      | pp_list pp_x (x::xs) = pp_x x ^ ", " ^ pp_list pp_x xs

    (*import from StatObject:*)
    structure Level        = StatObject.Level
    structure TyVar        = StatObject.TyVar
         type TyVar        = StatObject.TyVar
    structure TyName       = StatObject.TyName
         type TyName       = TyName.TyName
         type Type         = StatObject.Type
         type TypeScheme   = StatObject.TypeScheme
    structure Type         = StatObject.Type
    structure TypeScheme   = StatObject.TypeScheme
    structure TypeFcn      = StatObject.TypeFcn
            
    (*import from Environments:*)
    type VarEnv            = Environments.VarEnv
    type TyStr             = Environments.TyStr
    type TyEnv             = Environments.TyEnv
    type StrEnv            = Environments.StrEnv
    type Env               = Environments.Env
    type Context           = Environments.Context
    type constructor_map   = Environments.constructor_map
    type realisation       = Environments.realisation
    structure VE           = Environments.VE
    structure TyStr        = Environments.TyStr
    structure TE           = Environments.TE
    structure SE           = Environments.SE
    structure E            = Environments.E
    structure C            = Environments.C
    structure constructor_map = Environments.constructor_map
    structure Realisation  = Environments.Realisation

    (*import from ModuleEnvironments:*)
    type Basis             = ModuleEnvironments.Basis
    type FunEnv            = ModuleEnvironments.FunEnv
    type SigEnv            = ModuleEnvironments.SigEnv
    type prjid             = ModuleEnvironments.prjid
    structure G            = ModuleEnvironments.G
    structure F            = ModuleEnvironments.F
    structure B            = ModuleEnvironments.B

    structure rEnv = ModuleEnvironments.rEnv  (* RefinedEnvironments *)
    type rEnv = rEnv.Env
    type rContext = rEnv.Context
    type rT = rEnv.TyNameEnv
    type rVarEnv = rEnv.VarEnv
    type rStrEnv = rEnv.StrEnv    

    structure rRea = rEnv.Rea

    (*import from ModuleStatObject:*)
    type Sig               = ModuleStatObject.Sig
    type FunSig            = ModuleStatObject.FunSig
    exception No_match     = ModuleStatObject.No_match
    (*may be raised by Sigma.match and Phi.match*)
    structure Sigma        = ModuleStatObject.Sigma
    structure Phi          = ModuleStatObject.Phi
    
    (*import from other modules:*)
    type StringTree        = StringTree.t  (* PrettyPrint.StringTree *)
    type tycon             = IG.tycon
    type strid             = IG.strid

    structure Ident        = IG.DecGrammar.Ident
    structure TyCon        = IG.DecGrammar.TyCon
    structure ErrorInfo    = ElabInfo.ErrorInfo
    structure TypeInfo     = ElabInfo.TypeInfo

    structure ExplTyVar = IG.DecGrammar.TyVar
    structure RO = RefObject  (* Abbreviation *)
    structure REI = ElabInfo.ErrorInfo.RefineErrorInfo
    structure Comp = RefDec.Comp

    type SortName = RO.SortName

    infixr oo                           fun R1 oo R2 = Realisation.oo(R1,R2)
    infixr B_plus_E                     fun B B_plus_E E = B.plus_E(B,E)
    infixr B_plus_B                     fun B B_plus_B B' = B.plus(B,B')
    infixr G_plus_G                     fun G1 G_plus_G G2 = G.plus(G1,G2)
    infixr B_plus_G                     fun B B_plus_G G = B.plus_G(B,G)
    infixr E_plus_E                     fun E1 E_plus_E E2 = E.plus(E1,E2)
    infixr F_plus_F                     fun F1 F_plus_F F2 = F.plus(F1,F2)
    infixr B_plus_F                     fun B B_plus_F F = B.plus_F(B,F)
    infixr SE_plus_SE                   fun SE1 SE_plus_SE SE2 = SE.plus(SE1,SE2)

                                        local val a = op @
                                        in fun op @ x = a x
                                        end
    infixr B_plus_rE                    fun B B_plus_rE rE = B.plus_rE (B, rE)
    infixr @@                           fun rT1 @@ rT2 = rEnv.T_plus_T (rT1, rT2)
    infixr rE_plus_rE                   fun rE1 rE_plus_rE rE2 = rEnv.E_plus_E (rE1, rE2)

    fun B_plus_E_rT_rE (B, E, rT, rE) = B.plus_rE (B.plus_rT (B.plus_E(B,E), rT), rE)
    fun B_from_E_rT_rE (E, rT, rE) = B.plus_rE (B.plus_rT (B.from_E E, rT), rE)

    val rT0 = rEnv.emptyT  (* Abbreviations.  Perhaps using "0" is a bit confusing.  *)
    val rE0 = rEnv.emptyE
    val rTE0 = rEnv.emptyTE
    val rRE0 = rEnv.emptyRE
    val rVE0 = rEnv.emptyVE

    val letCV = RefDec.Comp.letCV

    fun tynames_E E = E.tynames E
    fun tynames_G G = G.tynames G
    fun tynames_F F = F.tynames F
    fun tyvars_Type tau = Type.tyvars tau
    fun tyvars_B' B = B.tyvars' B

      (*the following three types are for the signature:*)
    type PreElabTopdec  = IG.topdec
    type PostElabTopdec = OG.topdec
    type StaticBasis = ModuleEnvironments.Basis

    (* debugging stuff *)
    val debug_width = ref 100
    fun pr_Tree tree = PrettyPrint.outputTree (print, tree, !debug_width)
    fun pr_Env E = pr_Tree (E.layout E)

    val debug_push = rEnv.debug_push
    val debug_pop = rEnv.debug_pop
    val debug_push_must = rEnv.debug_push_must
    val assert = rEnv.assert

    val allowNonStandardSharing = ref true
    val () = Flags.add_flag ("allowNonStandardSharing", "allowNonStandardSharing", 
                             allowNonStandardSharing)

    val skipRefinement = ref false
    val () = Flags.add_flag ("Skip sort checking?", "skipRefinement", skipRefinement)

    (*Error handling stuff*)
    type ParseInfo  = ParseInfo.ParseInfo
    type ElabInfo = ElabInfo.ElabInfo

    val okConv = ElabInfo.from_ParseInfo

    (* This is set when elab errors are found, and reset in the external elab_topdec. *)
    val found_error : bool ref = ElabDec.found_error

    fun plus_ErrorInfo i e =
        (found_error := true; 
         ElabInfo.plus_ErrorInfo i e)        

    fun errorConv (i : ParseInfo, e : ErrorInfo.ErrorInfo) : ElabInfo =
          plus_ErrorInfo (okConv i) e

    fun rErrorConv (i : ParseInfo, errs : Comp.Error list) =  (* doesn't set found_error *) 
        ElabInfo.plus_ErrorInfo (okConv i) (ErrorInfo.REFDEC errs)

    fun rErrorConv1 (i : ParseInfo, err : REI.ErrorInfo) =
        let val ok_i = okConv i in
          ElabInfo.plus_ErrorInfo (ok_i) (ErrorInfo.REFDEC [(ParseInfo.to_SourceInfo i, err)])
        end

    fun typeConv (i : ParseInfo, type_info : TypeInfo.TypeInfo) : ElabInfo =
          ElabInfo.plus_TypeInfo (okConv i) type_info

    fun repeatedIdsError (i : ParseInfo,
                          rids : ErrorInfo.RepeatedId list)
          : ElabInfo =
            errorConv (i, ErrorInfo.REPEATED_IDS rids)

    (*repeaters (op =) [1,2,1,3,4,4] = [1,4].  Used to check
     syntactic restrictions:*)

    fun repeaters eq ys =
          let
            fun occurences x [] = 0
              | occurences x (y::ys) = 
                  (if eq (x,y) then 1 else 0) + occurences x ys
          in
            List.all (fn x => (occurences x ys) > 1) ys
          end

    fun member x xs = List.exists (fn y => x=y) xs


    (*Sigma_match: We rename flexible names in error_result so
     that the result structure returned in the event of an error
     is as general as possible:*)
    fun Sigma_match (i, rTfull, Sigma, E, rT, rE) =
      let val (E, rE) = Sigma.match (rTfull, Sigma, E, rE)
      in  (okConv i, E, rT, rE)  end
      handle No_match reason =>
        let val (E',rT',rE') = Sigma.instance rTfull Sigma
        in  (errorConv (i, ErrorInfo.SIGMATCH_ERROR reason), E', rT', rE')  end

    (* The realisation below maps abstract type names E_opaque_result into what they stand for *)
    fun Sigma_match' (i, rTfull, Sigma, E, rE) =                          (* in E_trans_result *)
      let val (E_trans_result, T, E_opaque_result, rTout, rEout, phi) = 
              Sigma.match' (rTfull, Sigma, E, rE)
      in (okConv i, E_trans_result, T, E_opaque_result, rTout, rEout, phi)
      end handle No_match reason =>
        let val (T',E',rT',rE') = Sigma.instance' rTfull Sigma
        in (errorConv (i, ErrorInfo.SIGMATCH_ERROR reason), E', T', E', rT', rE', Realisation.Id)
        end

    (*initial_TE datdesc = the TE to be used initially in
     elaborating a datdesc. We determine the correct equality
     attributes when we maximise equality:*)
    fun initial_TE (IG.DATDESC (_, explicittyvars, tycon, _, opt)) =
      let val (_, TE) = TE.init' explicittyvars tycon
      in case opt
           of NONE => TE
            | SOME datdesc => (TE.plus(TE,initial_TE datdesc))
      end

    fun initial_rT_rTE_rRE (TE, IG.DATDESC (_, explicittyvars, tycon, _, opt)) =
      if !found_error then (rT0, rTE0, rRE0) else
      let val (SOME tystr) = TE.lookup TE tycon
          val (SOME tyname) = TypeFcn.to_TyName (TyStr.to_theta tystr)
          val sortname = 
            SortName.freshSortName 
              {conjuncts=[tycon], 
               variance= map ExplTyVar.variance explicittyvars, 
               tyname=tyname}
          val sortFcn =
            RO.SortName_in_SortFcn sortname
          val RL = rEnv.singleRL sortname
          val tystr =
            rEnv.mkTyStr(rEnv.emptyCE, rEnv.emptyR, sortname, true, RL)
          val rT = rEnv.singleT(tyname, tystr)
          val rTE = rEnv.singleTE(tycon, sortFcn)
          val rRE = rEnv.singleRE(tycon, sortFcn)
      in case opt
           of NONE => (rT, rTE, rRE)
            | SOME datdesc => let val (rT2, rTE2, rRE2) = initial_rT_rTE_rRE (TE, datdesc)
                              in (rT @@ rT2, rEnv.TE_plus_TE (rTE, rTE2),
                                  rEnv.RE_plus_RE (rRE, rRE2))
                              end
      end

    fun Sigma_instance' a = Sigma.instance' a

    (*wellformed_E is used for the `where type' construct*)
    fun wellformed_E E =
      let val (SE, TE, VE) = E.un E
      in
        wellformed_SE SE andalso wellformed_TE TE
      end
    and wellformed_TE TE =
      TE.fold
      (fn tystr => fn bool => bool andalso wellformed_TyStr tystr)
      true TE
    and wellformed_TyStr tystr =
      let val (theta, VE) = TyStr.to_theta_and_VE tystr
      in
        TypeFcn.is_TyName theta orelse VE.is_empty VE
      end
    and wellformed_SE SE =
      SE.fold (fn E => fn bool => bool andalso wellformed_E E) true SE


 structure StructureSharing =
   struct

    exception Share of ErrorInfo.ErrorInfo
    fun update(a,b,m) = case FinMap.lookup m a
                                  of SOME bs => FinMap.add(a,b::bs,m)
                                   | NONE => FinMap.add(a,[b],m)
    (* We first collect a list of tyname lists which must be identified. *)
    fun collect_TE (flexible : TyName -> bool, path : strid list, TEs, acc) : TyName list list =
      let val tcmap = List.foldL (fn TE => fn acc => 
                         TE.Fold (fn (tycon, tystr) => fn acc => update(tycon,tystr,acc)) acc TE ) 
                         FinMap.empty TEs
          (* Eliminate entries with less than two component, check
           * arities and flexibility of involved tynames. Further,
           * extract tynames from type structures. *)
      in
         FinMap.Fold(fn ((tycon, []),acc) => acc
                      | ((tycon, [tystr]), acc) => acc
                      | ((tycon, tystrs), acc) =>
                  let fun tystr_to_tyname tystr =
                        let val theta = TyStr.to_theta tystr
                        in case TypeFcn.to_TyName theta
                             of NONE => 
                                 if !allowNonStandardSharing then NONE
                                 else raise Share (ErrorInfo.SHARING_TYPE_NOT_TYNAME
                                                   (TyCon.implode_LongTyCon (rev path, tycon),
                                                    theta))
                              | SOME t => SOME t
                        end 
                  in
                      case StdList.mapPartial tystr_to_tyname tystrs of 
                          tynames as (t :: _) =>
                              let val arity = TyName.arity t
                                  val _ = List.apply (fn t => 
                                                      if TyName.arity t = arity then
                                                          if flexible t then ()
                                                          else raise Share (ErrorInfo.SHARING_TYPE_RIGID
                                                                            (TyCon.implode_LongTyCon (rev path, tycon), t))
                                                      else raise Share (ErrorInfo.SHARING_TYPE_ARITY tynames))
                                                     tynames                                      
                              in tynames::acc
                              end
                        | _ => acc  (* No actual sharing, because < 2 are tynames *)
                  end) acc tcmap
      end

    fun collect_E (flexible, path, Es, acc) : TyName list list =
      let val (SEs, TEs) = List.foldL(fn E => fn (SEs, TEs) =>
                                      let val (SE,TE,VE) = E.un E
                                      in (SE::SEs,TE::TEs)
                                      end) ([],[]) Es
          val acc = collect_SE(flexible, path, SEs, acc)
      in collect_TE(flexible, path, TEs, acc)
      end
    and collect_SE (flexible, path, SEs, acc) : TyName list list =
      let val smap = List.foldL (fn SE => fn acc => 
                       SE.Fold (fn (strid, E) => fn acc => update(strid,E,acc)) acc SE ) 
                       FinMap.empty SEs
      in
            FinMap.Fold(fn ((strid, []), acc) => acc            (* Eliminate entries with *)
                         | ((strid, [E]), acc) => acc     (* less than two component. *)
                         | ((strid, Es), acc) => collect_E(flexible,strid::path,Es,acc))
            acc smap
      end
    (* Collapse tynames set if any candidates identifies two such *)
    fun collapse ([], Ts) : TyName.Set.Set list = Ts
      | collapse (T::Ts, Ts') =
      let fun split ([], no, yes) = (no, yes)
            | split (T'::Ts'', no, yes) =
              if TyName.Set.isEmpty(TyName.Set.intersect T' T) then split(Ts'',T'::no, yes)
              else split(Ts'',no, T'::yes)
      in case split(Ts,[],[])
           of (no, []) => collapse(no, T::Ts')
            | (no, yes) => 
             let val Tnew = List.foldL(fn T => fn T' => TyName.Set.union T T') T yes
             in collapse(Tnew::no, Ts')
             end
      end
    (* Find a representative; if everything is allright, T 
     * will have at least one member. *)
      fun find ([], acc) = impossible "IG.SHARINGspec.find"
        | find ([t], acc) = (t, acc)
        | find (t::ts, acc) = if TyName.equality t then (t, ts @ acc)
                              else find (ts,t::acc)
    (* Build up a realisation with plus; this is possible since we
     * know all components are now distinct. [Merge rT (and rE) accordingly.]  *)
    fun build ([], rT1, rE1) : TyName list * realisation * rT * rEnv = 
          ([], Realisation.Id, rT1, rE1)
      | build (T1::Ts1, rT1, rE1) = 
          let val (t, T2) = find(T1,[])
              val phi = Realisation.from_T_and_tyname (TyName.Set.fromList T2, t) 
              val (rT2, rE2) = if !found_error then  (rT1, rE1)
                               else  rEnv.mergeT (rT1, rE1, T2, t)  (* may raise No_match *)
              val (T3, phi3, rT3, rE3) = build (Ts1, rT2, rE2)
          in (T2 @ T3, phi3 oo phi, rT3, rE3)
          end
    fun share (flexible, Es : Env list, rT, rE) : TyName list * realisation * rT * rEnv =
      let val Ts = map TyName.Set.fromList (collect_E(flexible,[],Es,[]))
          val Ts : TyName list list = map TyName.Set.list (collapse(Ts,[]))
      in build (Ts, rT, rE) (* FIX: - rowan *)
      end

   end  (* of structure StructureSharing *)

     val share = StructureSharing.share
     exception Share = StructureSharing.Share

    (* ------------------------------------------------
     * Match object in repository (for recompilation)
     * ------------------------------------------------ *)
(*
    fun match_and_update_repository (prjid_and_funid,T',E') : unit =
      let val N' = map TyName.name (TyName.Set.list T')
          val B' = B.from_E E'
          val obj = (ElabRep.empty_infix_basis,B.empty,[],(ElabRep.empty_opaq_env, TyName.Set.empty), 
                     N',ElabRep.empty_infix_basis,B', ElabRep.empty_opaq_env)
      in case ElabRep.lookup_elab prjid_and_funid
           of SOME (index,(_,_,_,_,N,_,B,_)) =>  (* Names in N already marked generative, 
                                                  * because the object is returned by lookup. *)
             (List.apply Name.mark_gen N';
              B.match(B',B);
              List.apply Name.unmark_gen N';
              List.apply Name.mk_rigid N';
              ElabRep.owr_elab(prjid_and_funid,index,obj))

            | NONE => ElabRep.add_elab(prjid_and_funid,obj)
      end
*)
    (* --------------------------------------------
     *  Checking for respecifications
     * -------------------------------------------- *)

    local
      structure VIdSet = OrderSet(structure Order = struct type T = Ident.id
                                                           val lt = fn x => fn y => Ident.< (x,y)
                                                    end
                                  structure PP = PrettyPrint)
      structure TyConSet = OrderSet(structure Order = struct type T = TyCon.tycon
                                                             val lt = fn x => fn y => TyCon.< (x,y)
                                                      end
                                    structure PP = PrettyPrint)
      structure StrIdSet = OrderSet(structure Order = struct type T = StrId.strid
                                                             val lt = fn x => fn y => StrId.< (x,y)
                                                      end
                                    structure PP = PrettyPrint)
    in                                                      
      type ids = StrIdSet.Set * TyConSet.Set * VIdSet.Set * VIdSet.Set (* last for rVE *)
      val ids_empty = (StrIdSet.empty, TyConSet.empty, VIdSet.empty, VIdSet.empty)
      fun add_ids_strid((strid_set,tycon_set,vid_set, rvid_set), strid) =
        let val ids = (StrIdSet.insert strid strid_set, tycon_set, vid_set, rvid_set)
        in (StrIdSet.member strid strid_set, ids)
        end
      fun add_ids_tycon((strid_set,tycon_set,vid_set,rvid_set), tycon) =
        let val ids = (strid_set, TyConSet.insert tycon tycon_set, vid_set, rvid_set)
        in (TyConSet.member tycon tycon_set, ids)
        end
      fun add_ids_vid((strid_set,tycon_set,vid_set,rvid_set), vid) =
        let val ids = (strid_set, tycon_set, VIdSet.insert vid vid_set,rvid_set)
        in (VIdSet.member vid vid_set, ids)
        end
      fun in_vids ((strid_set,tycon_set,vid_set,rvid_set), vid) =
        VIdSet.member vid vid_set
      fun add_ids_rvid((strid_set,tycon_set,vid_set,rvid_set), rvid) =
        let val ids = (strid_set, tycon_set, vid_set, VIdSet.insert rvid rvid_set)
        in (VIdSet.member rvid rvid_set, ids)
        end
    end

    (* -------------------------
     * Some utilities
     * ------------------------- *)

    fun map_Some_on_2nd (x,y) = (x,SOME y)
    fun map_Some_on_2nd' (x,y,z) = (x,SOME y,z)
    fun map_Some_on_3nd (x,z,y) = (x,z,SOME y)
    fun map_Some_on_5th (x,z,rx,rz,y) = (x,z,rx,rz,SOME y)
      
    fun elab_X_opt (prjid, Y, SOME X) elab_X empty_Z = map_Some_on_2nd (elab_X (prjid, Y, X))
      | elab_X_opt (prjid, Y, NONE) elab_X empty_Z = (empty_Z, NONE)

    fun elab_X_opt' (Y, SOME X) elab_X empty_Z empty_W empty_rZ empty_rW = 
        map_Some_on_5th (elab_X (Y, X))
      | elab_X_opt' (Y, NONE) elab_X empty_Z empty_W  empty_rZ empty_rW = 
        (empty_Z, empty_W, empty_rZ, empty_rW, NONE)

    fun elab_X_opt'' (Y, SOME X, ids) elab_X empty_Z = map_Some_on_2nd' (elab_X (Y, X, ids))
      | elab_X_opt'' (Y, NONE, ids) elab_X empty_Z = (empty_Z, NONE, ids)

    fun Phi_match_via a =  Phi.match_via a
    fun Sigma_to_T_E_rT_rE a = Sigma.to_T_E_rT_rE a
    fun VE_close a = VE.close a
    fun maximise_equality_in_VE_and_TE a = Environments.maximise_equality_in_VE_and_TE a
    fun lookup_longstrid a = B.lookup_longstrid a
    fun lookup_funid a = B.lookup_funid a
    fun lookup_sigid a = B.lookup_sigid a
    fun lookup_longtycon a = E.lookup_longtycon a
    fun lookup_longtycon' a = B.lookup_longtycon a
    fun lookup_longstrid' a = E.lookup_longstrid a
    fun lookup_tycon a = C.lookup_tycon a

    fun lookupE_rlstrid rE lstrid = B.lookup_rlongstrid (B.from_rE rE) lstrid

    (*****************************************************)
    (* Structure Expressions - Definition v3 pages 36-37 *)
    (*****************************************************)
    (* val ref_datdesc :> rContext * datdesc * bool -> (rT * rVE * bool) Comp *)
    fun elab_strexp (B : Basis, strexp : IG.strexp) : (TyName list * Env * rT * rEnv * OG.strexp) =

     ( (*debug_push (fn () => ["elab_strexp"]); *)
      case strexp of
        (* Generative *)                                    (*rule 50*)
        IG.STRUCTstrexp (i, strdec) =>
          let val (T, E, rT, rE, out_strdec) = elab_strdec (B, strdec)
          in
            (T, E, rT, rE, OG.STRUCTstrexp (okConv i, out_strdec))
          end

        (* Structure identifier *)                          (*rule 51*)
      | IG.LONGSTRIDstrexp (i, longstrid) => 
          (case (lookup_longstrid B longstrid, B.lookup_rlongstrid B longstrid, !found_error) of
             (SOME E, SOME rE, _) => ([], E, rT0, rE, OG.LONGSTRIDstrexp (okConv i, longstrid))
           | (SOME E, NONE, true) =>  (* No Sort-Checking after an Elaboration error. *)
               ([], E, rT0, rE0, OG.LONGSTRIDstrexp (okConv i, longstrid))
           | (NONE, _, _) =>
               ([], E.bogus, rT0, rE0,
                OG.LONGSTRIDstrexp
                  (errorConv (i, ErrorInfo.LOOKUP_LONGSTRID longstrid), longstrid)))
                                                            (*rule 52*)
      | IG.TRANSPARENT_CONSTRAINTstrexp (i, strexp, sigexp) =>
          let val (T, E, rT, rE, out_strexp) = elab_strexp (B, strexp)
              val (Sigma, out_sigexp) = elab_sigexp (B, sigexp)
              val rTfull = rEnv.T_of_C (B.to_rC B) @@ rT   (* for conjunctions in Sigma_match *)
              val (out_i, E', rT', rE') = Sigma_match (i, rTfull, Sigma, E, rT, rE)
              val out_i = ElabInfo.plus_TypeInfo out_i (TypeInfo.TRANS_CONSTRAINT_INFO E')
          in     (* rT' should be rT, unless there is an error *)
            (T, E', rT', rE', OG.TRANSPARENT_CONSTRAINTstrexp (out_i, out_strexp, out_sigexp))
          end
                                                            (*rule 53*)
      | IG.OPAQUE_CONSTRAINTstrexp (i, strexp, sigexp) =>
          let val (T, E, rT, rE, out_strexp) = elab_strexp (B, strexp)
              val (Sigma, out_sigexp) = elab_sigexp (B, sigexp)
              val rTfull = rEnv.T_of_C (B.to_rC B) @@ rT   (* for conjunctions in Sigma_match' *)
              val (out_i, E_trans, T', E_opaque, rT', rE_opaque, phi) = 
                  Sigma_match' (i, rTfull, Sigma, E, rE)
              val out_i = ElabInfo.plus_TypeInfo out_i (TypeInfo.OPAQUE_CONSTRAINT_INFO 
                                                          (E_trans,phi) )
          in
            (TyName.Set.list T', E_opaque, rT', rE_opaque, 
             OG.OPAQUE_CONSTRAINTstrexp (out_i, out_strexp, out_sigexp))
          end

        (* Functor application *)                           (*rule 54*)
      | IG.APPstrexp (i, funid, strexp) =>   (* FIX - rowan 22jan02 *)
          let
            val (T, E, rT, rE, out_strexp) = elab_strexp (B, strexp)
          in   (* The following works for the examples I've tried so far, but may not
                  work in every case.  - Rowan 19sep03 *)
            case lookup_funid B funid of
               SOME (prjid, Phi) =>  (* rea_inst is for argument instantiation; rea_gen accounts *)
                (let val rTfull = rEnv.T_of_C (B.to_rC B) @@ rT
                     val (Sig, rea_inst, rea_gen) = (* for generative names in the functor body. *)
                         Phi_match_via (Phi, E, rTfull, rE)
                     val (T2, E2, rT2, rE2) = Sigma_to_T_E_rT_rE Sig
                     val out_i = typeConv(i,TypeInfo.FUNCTOR_APP_INFO {rea_inst=rea_inst,
                                                                       rea_gen=rea_gen, Env=E2})
                 in (T @ (TyName.Set.list T2), E2, rT @@ rT2, rE2 (* FIX? (Added rT2 20jan03) *), 
                     OG.APPstrexp (out_i, funid, out_strexp) ) 
                 end handle No_match reason =>   (* We bind the argument names in error_result *)
                   let         (* so that the argument signature returned is as general as possible. *)
                     val (T1, E1, rT1, rE1, Sig1) = Phi.to_T_E_rT_rE_Sigma Phi
                     val (T2, E2, rT2, rE2) = Sigma.to_T_E_rT_rE Sig1
                     val T3 = TyName.Set.difference (tynames_E E2) (TyName.Set.union T1 T2)
                     val Sig3 = Sigma.from_T_E_rT_rE (T3, E2, rT2, rE2)  (* FIX? *)
                     val out_i = errorConv (i, ErrorInfo.SIGMATCH_ERROR reason)
                     val (Einst, rTinst, rEinst) = Sigma.instance (rT @@ rT1 (* check *)) Sig3
                   in (T @ (TyName.Set.list T2), Einst, rTinst, rEinst, 
                       OG.APPstrexp (out_i, funid, out_strexp)) 
                   end
                 )
             | NONE =>
                 (T, E.bogus, rT, rE0,
                  OG.APPstrexp (errorConv (i, ErrorInfo.LOOKUP_FUNID funid),
                                funid, out_strexp))
          end

        (* Local declaration *)                             (*rule 55*)
      | IG.LETstrexp (i, strdec, strexp) =>
          let val (T1, E1, rT1, rE1, out_strdec) = elab_strdec (B, strdec)
              val (T2, E2, rT2, rE2, out_strexp) = elab_strexp (B_plus_E_rT_rE (B,E1,rT1,rE1), 
                                                                strexp)
          in
            (T1 @ T2, E2, rT1 @@ rT2, rE2, OG.LETstrexp (okConv i, out_strdec, out_strexp))
          end

        (* Dummy stub structure with assumed signature (for sort checking) *)
      | IG.ASSUMEstrexp (i, sigexp) =>
          let val (Sigma, out_sigexp) = elab_sigexp (B, sigexp)
              val (T, E, rT, rE) = Sigma.to_T_E_rT_rE Sigma
           in
            (TyName.Set.list T, E, rT, rE, OG.ASSUMEstrexp (okConv i, out_sigexp))
          end
    ) (* before debug_pop (fn () => []) *)



    (********************************************************)
    (* Structure-level Declarations - Definition v3 page 37 *)
    (********************************************************)

    and elab_strdec (B : Basis, strdec : IG.strdec) : (TyName list * Env * rT * rEnv * OG.strdec) =
      let 
(*        val _ = (* if !Flags.DEBUG_ELABTOPDEC then *)
                rEnv.debug_push (fn () =>["elab_strdec: dec\n", 
                                             PrettyPrint.flatten1 (IG.layoutStrdec strdec)]); *)
(*                      print "\nelab_strdec: B\n";
                      pr_Tree (B.layout B);
                      print "\n")
              else ()
*)
        val res =                       
       (case strdec of
        (* Core declaration *)                              (*rule 56*)
        IG.DECstrdec (i, dec) => 
          let
            val (T, E, out_dec) = ElabDec.elab_dec (B.to_C B, dec)
            val ((rT, rE), rErrors) = if !found_error          (* Check if any errors *)
                                       then ((rEnv.emptyT, rEnv.emptyE), [])
                                       else RefDec.ref_dec (B.to_rC B, out_dec) true
            (* Filter out and print warnings. isWarning should be in RefInfo. *)
            fun isWarning (_, REI.UNMATCHED srt) = true
              | isWarning  _ = false
            val (rWarnings2, rErrors2) = StdList.partition isWarning rErrors
            val () = app (fn warning => Flags.warn (REI.report warning) ) rWarnings2
            val out_i = case rErrors2 of [] => okConv i
                                     | _ => rErrorConv (i, rErrors2)
          in                                               (* catch "structure s : S = ... *)
            (T, E, rT, rE, OG.DECstrdec (out_i, out_dec))
          end

        (* Structure declaration *)                         (*rule 57*)
      | IG.STRUCTUREstrdec (i, strbind) =>
          let
            val (T, SE, rT, rSE, out_strbind) = elab_strbind (B, strbind)
          in
            (T, E.from_SE SE, rT, rEnv.SE_in_E rSE, OG.STRUCTUREstrdec (okConv i, out_strbind))
          end

        (* Local declaration *)                             (*rule 58*)
      | IG.LOCALstrdec (i, strdec1, strdec2) =>
          let
            val (T1, E1, rT1, rE1, out_strdec1) = elab_strdec (B, strdec1)
            val (T2, E2, rT2, rE2, out_strdec2) = elab_strdec (B_plus_E_rT_rE (B, E1, rT1, rE1),
                                                               strdec2)
          in
            (T1 @ T2, E2, rT1@@rT2, rE2, OG.LOCALstrdec (okConv i, out_strdec1, out_strdec2))
          end

        (* Empty declaration *)                             (*rule 59*)
      | IG.EMPTYstrdec i => ([], E.empty, rEnv.emptyT, rEnv.emptyE, OG.EMPTYstrdec (okConv i))

        (* Sequential declaration *)                        (*rule 60*)
      | IG.SEQstrdec (i, strdec1, strdec2) =>
          let
            (* val _ = rEnv.debug_pop (fn () => []) (* Early pop in this case => smaller stacks *) *)
            val (T1, E1, rT1, rE1, out_strdec1) = elab_strdec (B, strdec1)
            val (T2, E2, rT2, rE2, out_strdec2) = elab_strdec (B_plus_E_rT_rE (B,E1,rT1,rE1), 
                                                               strdec2)
          in
            (T1 @ T2,  E1 E_plus_E E2,  rT1 @@ rT2,  rE1 rE_plus_rE rE2,
             OG.SEQstrdec (okConv i, out_strdec1, out_strdec2))
          end)

(*      val _ = case strdec of IG.SEQstrdec _ => () | _ => rEnv.debug_pop (fn () => []) *)
      val _ = if !Flags.DEBUG_ELABTOPDEC then   
                     (print "elab_strdec: dec\n";
                      pr_Tree (IG.layoutStrdec strdec);  (* This code may crash.  *)
(*                      print "\nelab_strdec: B\n";        (* It should check found_error. *) *)
(*                      pr_Tree (B.layout B); *)
                      print "\nelab_strdec: rT\n";
                      pr_Tree (rEnv.layoutT (#3 res));
                      print "\nelab_strdec: rE\n";
                      pr_Tree (rEnv.layoutEnv (#4 res)); 
                      print "\n\n")
              else ()
      in  res  end

    (**********************************************)
    (* Structure Bindings - Definition v3 page 38 *)
    (**********************************************)

    and elab_strbind (B : Basis, strbind : IG.strbind)   (* remove or project TG here *)
      : (TyName list * StrEnv * rT * rEnv.StrEnv * OG.strbind) =
      (case strbind of 
       (* Structure bindings *)                             (*rule 61*)
       IG.STRBIND (i, strid, strexp, strbind_opt) =>
         let
(*           val () = debug_push (fn () => ["elab_strbind"]) *)
           val (T, E, rT, rE, out_strexp) = elab_strexp (B, strexp)
           val (T', SE, rT', rSE', out_strbind_opt) =
                 elab_X_opt' (B, strbind_opt)
                   elab_strbind [] SE.empty rT0 rEnv.emptySE
           val out_i = if EqSet.member strid (SE.dom SE)
                       then repeatedIdsError (i, [ErrorInfo.STRID_RID strid])
                       else okConv i
         in
           (T' @ T, SE.singleton (strid, E) SE_plus_SE SE, rT @@ rT', 
            rEnv.SE_plus_SE (rEnv.singleSE (strid, rE), rSE'),
            OG.STRBIND (out_i, strid, out_strexp, out_strbind_opt))
           (*before debug_pop (fn () =>[]) *)
         end)

    (*****************************************************************)
    (* Signature Expressions, Definition v3 page 38, rules 63 and 64 *)
    (*****************************************************************)

    (*elaborate a sigexp to an E*)
    and elab_sigexp' (B : Basis, sigexp : IG.sigexp) 
        : (TyName list * Env * rT * rEnv * OG.sigexp) =
        let
        val _ = if !Flags.DEBUG_ELABTOPDEC then 
                     (print "elab_sigexp': sigexp = \n";
                      pr_Tree (IG.layoutSigexp sigexp);
(*                      print "\nelab_strdec: B\n";
                      pr_Tree (B.layout B);
*)                      print "\n")
              else ()

        val res =                       

     (case sigexp 
         (* Generative *)                                   (*rule 62*)
        of IG.SIGsigexp (i, spec) =>
           let val (T, E, rT, rE, out_spec, _) = elab_spec (B, spec, ids_empty)
           in
             (T, E, rT, rE, OG.SIGsigexp (okConv i, out_spec))
           end

         (* Signature identifier *)                         (*rule 63*)
       | IG.SIGIDsigexp (i, sigid) =>
           (case lookup_sigid B sigid of
              SOME sigma =>
                let val (T,E,rT,rE) = Sigma_instance' (rEnv.T_of_C (B.to_rC B)) sigma
                                      (* Sigma.to_T_E_rT_rE sigma *)
                in
                  (TyName.Set.list T, E, rT, rE, OG.SIGIDsigexp (okConv i, sigid))
                end
            | NONE =>
                ([], E.bogus, rT0, rE0,
                 OG.SIGIDsigexp (errorConv (i, ErrorInfo.LOOKUP_SIGID sigid), sigid)))

                                                            (*rule 64*)
       | IG.WHERE_TYPEsigexp (i, sigexp, explicittyvars, longtycon, ty) =>  
           let
             val (T, E, rT, rE, out_sigexp) = elab_sigexp' (B, sigexp)
             val _ = Level.push()
             val (alphas, C) = C.plus_U'(B.to_C B,explicittyvars)
             fun return (T, E_result, rT_result, rE_result, out_i, out_ty) =
               (T, E_result, rT_result, rE_result,                              (* 13jan03 Add pop()? *)
                OG.WHERE_TYPEsigexp (out_i, out_sigexp, explicittyvars, longtycon, out_ty))
             fun fail (error_info, out_ty) = return (T,E, rT, rE, errorConv (i, error_info), out_ty)
           in case ElabDec.elab_ty (C, ty)
                of (NONE, out_ty) => return (T, E, rT, rE, okConv i, out_ty) (* error in out_ty *)

                 | (SOME tau, out_ty) =>  
                  let val _ = Level.pop()
                  in case lookup_longtycon E longtycon 
                       of NONE => fail (ErrorInfo.LOOKUP_LONGTYCON longtycon, out_ty)
                        | SOME tystr =>
                         let val (theta, VE) = TyStr.to_theta_and_VE tystr 
                         in case TypeFcn.to_TyName theta 
                              of NONE => fail (ErrorInfo.WHERE_TYPE_NOT_TYNAME (longtycon, theta, tau), out_ty)
                               | SOME t =>
                                if not(TyName.Set.member t (TyName.Set.fromList T)) then
                                  fail (ErrorInfo.WHERE_TYPE_RIGID (longtycon, t), out_ty)
                                else if TyName.arity t <> List.size alphas then
                                  fail (ErrorInfo.WHERE_TYPE_ARITY (alphas, (longtycon, t)), out_ty) 
                                else
                                  let val theta' = TypeFcn.from_TyVars_and_Type (alphas, tau)
                                      val phi = Realisation.singleton (t, theta')
                                        val phi_E = Realisation.on_Env phi E

                                      val rC = rEnv.C_plus_T (B.to_rC B, rT)
                                  in
                                    case rEnv.Lookup_tyname (rC, t) of NONE =>
                                             fail (ErrorInfo.WHERE_TYPE_NOT_WELLFORMED (longtycon, t, tau), out_ty)
                                    | SOME rTystr =>
                                    case rEnv.R_of_TyStr rTystr of rR =>

                                    if length (rEnv.Rdom rR) > 1 then 
                                      fail (ErrorInfo.WHERE_TYPE_MULTIPLE_REFINEMENTS (longtycon, t), out_ty)
                                    else if TyName.equality t andalso not (TypeFcn.admits_equality theta') then
                                      fail (ErrorInfo.WHERE_TYPE_EQTYPE (longtycon, t, tau), out_ty)
                                    else if not (wellformed_E phi_E) then
                                      fail (ErrorInfo.WHERE_TYPE_NOT_WELLFORMED (longtycon, t, tau), out_ty)
                                    else if !found_error then 
                                        return (List.dropAll (fn t' => TyName.eq(t,t')) T, 
                                                phi_E, rT, rE, okConv i, out_ty)
                                    else
                                      let
                                        fun debug_print s =  () (* print (s ^ "\n") *)
                                        val _ = debug_print "HERE1\n"
                                        val TNtoSN = rEnv.TNtoSN rC  
                                        val sn = TNtoSN t
                                        val svs = map RO.SVofTV alphas
                                        val sfcn = RO.mkSortFcn (svs, RO.MLSortOfTy TNtoSN tau)
                                        val rphi = rRea.singleton (sn, sfcn)
                                        val rphi_rE = rRea.on_Env (rEnv.conjSortNameC rC) rphi rE
                                        val rT_less_t = rEnv.restrictT rT (fn t' => TyName.eq (t,t'))
                                        val rphi_rT = rRea.on_TyNameEnv (rEnv.conjSortNameC rC) rphi 
                                                                        rT_less_t
                                        (*val _ = pr_Tree (rEnv.layoutT rphi_rT)
                                          val _ = pr_Tree (rEnv.layoutEnv rphi_rE) *)
                                        val _ = debug_print ""
                                        val _ = map (debug_print o TyName.pr_TyName) T
                                        val _ = debug_print (TyName.pr_TyName t)
                                      in
                                        return (List.dropAll (fn t' => TyName.eq(t,t')) T, 
                                                phi_E, rphi_rT, rphi_rE, okConv i, out_ty)

                                      end
                                  end 
                         end
                  end
           end)
      val _ = if !Flags.DEBUG_ELABTOPDEC then   
                     (print "elab_sigexp: exp\n";
                      pr_Tree (IG.layoutSigexp sigexp);  (* This code may crash.  *)
(*                      print "\nelab_strdec: B\n";        (* It should check found_error. *) *)
(*                      pr_Tree (B.layout B); *)

                      print "\nelab_sigexp: T\n";
                      map (fn tn => print (TyName.pr_TyName tn ^ "\t") ) (#1 res); 
                      print "\n";
                      print "\nelab_sigexp: E\n";
                      pr_Tree (E.layout (#2 res)); 

                      print "\nelab_sigexp: rT\n";
                      pr_Tree (rEnv.layoutT (#3 res));
                      print "\nelab_sigexp: rE\n";
                      pr_Tree (rEnv.layoutEnv (#4 res)); 
                      print "\n\n")
              else ()
      in  res  end



    (*********************************************************)
    (* Signature Expressions, Definition v3 page 38, rule 65 *)
    (*********************************************************)

    (*elaborate a sigexp to a Sigma*)                       (*rule 65*)
    and elab_sigexp (B : Basis, sigexp : IG.sigexp) : (Sig * OG.sigexp) =
          let
            val (T, E, rT, rE, out_sigexp) = elab_sigexp' (B, sigexp)
(*            val T = TyName.Set.difference (tynames_E E) (B.to_T B) *)
          in
            (Sigma.from_T_E_rT_rE (TyName.Set.fromList T, E, rT, rE), out_sigexp)
          end


    (*********************************************************)
    (* Signature Declarations - Definition v3 page  39       *)
    (*********************************************************)

                                                            (*rule 66*)
    and elab_sigdec (B : Basis, sigdec : IG.sigdec) : (SigEnv * OG.sigdec) =
      (case sigdec of

        (* Single declaration *)
        IG.SIGNATUREsigdec (i, sigbind) =>
          let
            val (G, out_sigbind) = elab_sigbind (B, sigbind)
          in
            (G, OG.SIGNATUREsigdec (okConv i, out_sigbind))
          end)


    (**********************************************)
    (* Signature Bindings - Definition v3 page 39 *)
    (**********************************************)

    and elab_sigbind (B : Basis, sigbind : IG.sigbind) : (SigEnv * OG.sigbind) =

      (case sigbind of

        (* Signature bindings *)                            (*rule 67*)
        IG.SIGBIND (i, sigid, sigexp, NONE) =>
          let
            val (sigma, out_sigexp) = elab_sigexp (B, sigexp)
            val G = G.singleton (sigid, sigma)
            val out_i = ElabInfo.plus_TypeInfo (okConv i) (TypeInfo.SIGBIND_INFO (Sigma.tynames sigma))
            val _ = if !Flags.DEBUG_ELABTOPDEC then print "elab_sigbind: END\n" else ()
          in
            (G, OG.SIGBIND (out_i, sigid, out_sigexp, NONE))
          end

        (* Signature bindings *)                            (*rule 67*)
      | IG.SIGBIND (i, sigid, sigexp, SOME sigbind) =>
          let
            val (sigma, out_sigexp) = elab_sigexp (B, sigexp)
            val G1 = G.singleton (sigid, sigma)
            val (G2, out_sigbind) = elab_sigbind (B, sigbind)
            val out_i = if EqSet.member sigid (G.dom G2)
                        then repeatedIdsError (i, [ErrorInfo.SIGID_RID sigid])
                        else ElabInfo.plus_TypeInfo (okConv i) (TypeInfo.SIGBIND_INFO (Sigma.tynames sigma))
          in
            (G1 G_plus_G G2,
             OG.SIGBIND (out_i, sigid, out_sigexp, SOME out_sigbind))
          end)

    (**********************************************)
    (* Specifications - Definition v3 pages 39-40 *)
    (**********************************************)

    and elab_spec (B : Basis, spec : IG.spec, ids: ids) 
        : (TyName list * Env * rT * rEnv * OG.spec * ids) =
           
      (case spec of

        (* Value specification *)                           (*rule 68*)
        IG.VALspec (i, valdesc) =>
          let
            val _ = Level.push ()
            val ((VE, rVE), out_valdesc, ids) = elab_valdesc ((B.to_C B,B.to_rC B), valdesc, ids)
            val _ = Level.pop ()
          in                                   (* rVE is already closed *)
            ([], E.from_VE (VE_close VE), rT0, rEnv.VE_in_E rVE,
             OG.VALspec (okConv i, out_valdesc), ids)
          end

        (* Value sort specification *)  
      | IG.VALsspec (i, valdesc) =>
          let
            val _ = Level.push ()
            val ((VE, rVE), out_valdesc, ids) = 
                  elab_valsdesc ((B.to_C B, B.to_rC B), valdesc, ids)
            val _ = Level.pop ()
          in                  
            ([], E.from_VE (VE_close VE), rT0, rEnv.VE_in_E rVE,  (* rVE is already closed *)
             OG.VALsspec (okConv i, out_valdesc), ids)
          end

        (* Type specification *)                            (*rule 69% *)
      | IG.TYPEspec (i, typdesc) =>
          let val (T, TE, rT, rTE, rRE, out_typdesc, ids) = 
                  elab_typdesc false (B.to_C B, typdesc, ids)
          in
            (T, E.from_TE TE, rT, (rEnv.TE_in_E rTE) rE_plus_rE (rEnv.RE_in_E rRE),
             OG.TYPEspec (okConv i, out_typdesc), ids)
          end

        (* Sort specification *)                            (* RML *)
      | IG.SORTspec (i, sortdesc) => (* impossible "elab_spec: SORTspec not implemented" *)
          let val ((TE, rT, rRE), out_sortdesc, ids) =  elab_sortdesc (B.to_rC B, sortdesc, ids)
              val rE = (rEnv.RE_in_E rRE) rE_plus_rE (rEnv.TE_in_E (rEnv.convertREtoTE rRE))
          in
            ([], E.from_TE TE, rT, rE, OG.SORTspec (okConv i, out_sortdesc), ids)
          end

      | IG.SUBSORTspec (i, spec, longsrtcons1, longsrtcons2) =>
          let
            val (T0, E, rT, rE, out_spec, ids) = elab_spec (B, spec, ids) (* first we elab spec *)

            val rC0 = rEnv.E_in_C rE

            fun lookup [] : SortName list * TyCon.longtycon OG.WithInfo list =  ([], [])
              | lookup (IG.WITH_INFO (i2, longsrtcon) :: tail) = 
                let val (sns_tail, out_tail) = lookup tail
                    fun fail () = (sns_tail, rErrorConv1(i2, REI.LOOKUP_LONGSORTCON longsrtcon))
                    val (sns, out_i2) = 
                        case rEnv.Lookup_longsortcon (rC0, longsrtcon)
                          of NONE => fail ()
                           | SOME sortfcn =>
                             (case RO.SortFcn_to_SortName sortfcn
                                of NONE => fail ()  (* Error type isn't quite right here. *)
                                 | SOME sn => (sn :: sns_tail, okConv i2) )
              in
                (sns, OG.WITH_INFO (out_i2, longsrtcon) :: out_tail)
              end

            val (sns1, out_longsrtcons1) = lookup longsrtcons1
            val (sns2, out_longsrtcons2) = lookup longsrtcons2
            val rTplus = (rEnv.T_of_C (B.to_rC B)) @@ rT

            val conjSN = rEnv.conjSortNameT rTplus

            val (rTout, rEout) = 
              case (sns1, sns2) 
                of (sn1h::sns1t, sn2h::sns2t) =>
                   let
                       val sn1 = foldl conjSN sn1h sns1t
                       val sn2 = foldl conjSN sn2h sns2t
                       val rphi = rEnv.RL_collapse (rTplus, sn1, conjSN (sn1, sn2))
                   in
                       (rRea.on_TyNameEnv conjSN rphi rT, 
                        rRea.on_Env conjSN rphi rE)
                   end

                 | _ => (rT, rE)
          in
              (T0, E, rTout, rEout, 
               OG.SUBSORTspec (okConv i, out_spec, out_longsrtcons1, out_longsrtcons2), 
               ids)
          end


        (* Equality type specification *)
      | IG.EQTYPEspec (i, typdesc) =>                       (*rule 70*)
          let val (T, TE, rT, rTE, rRE, out_typdesc, ids) = 
                  elab_typdesc true (B.to_C B, typdesc, ids)
          in
            (T, E.from_TE TE,  rT, (rEnv.TE_in_E rTE) rE_plus_rE (rEnv.RE_in_E rRE),
             OG.EQTYPEspec (okConv i, out_typdesc), ids)
          end

        (* Datatype specification *)                        (*rule 71*)
      | IG.DATATYPEspec (i, datdesc) =>
          let
            val TE = initial_TE datdesc  
            val ((VE, TE), out_datdesc, ids) = 
                  elab_datdesc (C.plus_TE (B.to_C B, TE), datdesc, ids)
            val (VE, TE) = Environments.maximise_equality_in_VE_and_TE (VE, TE)
            val T = TE.fold(fn tystr => fn T => 
                            case TypeFcn.to_TyName(TyStr.to_theta tystr)
                              of SOME t => t::T
                               | NONE => impossible "IG.DATATYPEspec.theta not tyname") [] TE

            val (rT, rTE, rRE) = initial_rT_rTE_rRE (TE, datdesc)  
            val rE = rEnv.E_plus_E (rEnv.TE_in_E rTE, rEnv.RE_in_E rRE)
            val rCplus = rEnv.C_plus_C (B.to_rC B, rEnv.mkC(rT, rE))
            val ((rT2, rVE2, _), errs) = if !found_error then ((rT, rVE0, true), [])
                                         else ref_datdesc (rCplus, out_datdesc, true) true
            val out_i = case errs of [] => okConv i
                                   | _ => rErrorConv (i, errs)
          in
            (T, E.from_VE_and_TE (VE,TE), rT2, rE rE_plus_rE (rEnv.VE_in_E rVE2), 
             OG.DATATYPEspec (out_i, out_datdesc), ids)
          end

        (* Datasort specification *)       (* copy from ElabDec.sml, elab_datdesc *)
      | IG.DATASORTspec (i, datdesc) => 
        let    (* first, funs to convert so that we can reuse parts of RefDec *)
          fun to_datbind (IG.DATDESC (i, tyvars, tycon, condesc, datdesc_opt)) =
              IG.DecGrammar.DATBIND (i, tyvars, tycon, to_conbind condesc, 
                                     Option.map to_datbind datdesc_opt)
          and to_conbind (IG.CONDESC (i, longid, ty_opt, condesc_opt)) =
              IG.DecGrammar.CONBIND (i, IG.DecGrammar.OP_OPT (longid, false), ty_opt, 
                                     Option.map to_conbind condesc_opt)
          fun to_datdesc (OG.DecGrammar.DATBIND (i, tyvars, tycon, conbind, datbind_opt)) =
              OG.DATDESC (i, tyvars, tycon, to_condesc conbind, 
                                     Option.map to_datdesc datbind_opt)
          and to_condesc (OG.DecGrammar.CONBIND (i, OG.DecGrammar.OP_OPT (longid, _), ty_opt, 
                                                 conbind_opt)) =
              OG.CONDESC (i, longid, ty_opt, Option.map to_condesc conbind_opt)
          val datbind = to_datbind datdesc
          val C = B.to_C B
          val TE = ElabDec.datasort_TE (C, datbind)
          val out_datbind = ElabDec.elab_datbind_s(C.plus_TE (C, TE), datbind)
          val out_datdesc = to_datdesc out_datbind
        in if !found_error then 
             ([], E.from_TE TE, rT0, rE0, OG.DATASORTspec (okConv i, out_datdesc), ids)
           else
             let 
               val ((rT, rE), errs) = 
                   RefDec.ref_datsortbind_complete (B.to_rC B, out_datbind) true
               val out_i = case errs of [] => okConv i
                                      | _ => rErrorConv (i, errs)
             in
               ([], E.from_TE TE, rT, rE, OG.DATASORTspec (out_i, out_datdesc), ids)
             end
        end

        (*Datatype replication specification*)              (*rule 72*)
      | IG.DATATYPE_REPLICATIONspec (i, tycon, longtycon) => 
          (case lookup_longtycon' B longtycon of
             SOME tystr =>
               let val (theta, VE) = TyStr.to_theta_and_VE tystr
                   val TE = TE.singleton (tycon, tystr)

                   val rC = B.to_rC B
                   val SOME srtfcn = rEnv.Lookup_longsortcon (rC, longtycon)
                   val SOME sn = RO.SortFcn_to_SortName srtfcn
                   val tn = SortName.tyname sn
                   val _ = if !Flags.DEBUG_ELABTOPDEC then pr_Tree (rEnv.layoutC rC) else ()
                   val SOME rTystr = rEnv.Lookup_tyname (rC, tn)
                   val rCE = rEnv.CE_of_TyStr rTystr
                   val rVE = rEnv.CE_to_VE rCE
                   val rTE = rEnv.singleTE (tycon, srtfcn)
                   val rRE = rEnv.singleRE (tycon, srtfcn)
                   val rE = rEnv.E_plus_E (rEnv.VE_in_E rVE, (rEnv.E_plus_E (rEnv.TE_in_E rTE,
                                                                              rEnv.RE_in_E rRE )))
                   val (errors,ids) = case add_ids_tycon(ids,tycon)
                                        of (true, ids) => ([ErrorInfo.TYCON_RID tycon], ids)
                                         | (false, ids) => ([], ids) 
                   val (errors, ids) = VE.Fold(fn (vid,_) => fn (errors, ids) =>
                                              (case add_ids_vid(ids, vid)
                                                 of (true,ids) => (ErrorInfo.ID_RID vid :: errors, ids)
                                                  | (false, ids) => (errors, ids))) (errors,ids) VE 
                   val out_i = case errors
                                 of [] => okConv i
                                  | _ => repeatedIdsError(i,errors) 
               in
                 ([], E.from_VE_and_TE (VE,TE), rT0, rE, 
                  OG.DATATYPE_REPLICATIONspec (out_i, tycon, longtycon), ids)
               end
           | NONE =>
               ([], E.bogus, rT0, rE0, 
                OG.DATATYPE_REPLICATIONspec
                  (errorConv (i, ErrorInfo.LOOKUP_LONGTYCON longtycon),
                   tycon, longtycon), ids))

        (* Exception specification *)                       (*rule 73*)
      | IG.EXCEPTIONspec (i, exdesc) =>
          let val ((VE, rVE), out_exdesc, ids) = elab_exdesc ((B.to_C B, B.to_rC B), exdesc, ids) 
          in
            ([], E.from_VE VE, rT0, rEnv.VE_in_E rVE, OG.EXCEPTIONspec (okConv i, out_exdesc), ids)
          end

        (* Structure specification *)                       (*rule 74*)
      | IG.STRUCTUREspec (i, strdesc) =>
          let val (T, SE, rT, rSE, out_strdesc, ids) = elab_strdesc (B, strdesc, ids)
          in
            (T, E.from_SE SE, rT, rEnv.SE_in_E rSE, OG.STRUCTUREspec (okConv i, out_strdesc), ids)
          end

        (* Include specification *)                         (*rule 75*)
      | IG.INCLUDEspec (i, sigexp) =>
          let val (T, E, rT, rE, out_sigexp) = elab_sigexp' (B, sigexp)
              val (SE,TE,_) = E.un E
              val strids = EqSet.list (SE.dom SE)
              val (errors, ids) = List.foldL (fn strid => fn (errors,ids) =>
                                              (case add_ids_strid(ids,strid)
                                                 of (true, ids) => (ErrorInfo.STRID_RID strid :: errors, ids)
                                                  | (false, ids) => (errors, ids))) ([], ids) strids 
              val tycons = EqSet.list (TE.dom TE)
              val (errors, ids) = List.foldL (fn tycon => fn (errors,ids) =>
                                              (case add_ids_tycon(ids,tycon)
                                                 of (true, ids) => (ErrorInfo.TYCON_RID tycon :: errors, ids)
                                                  | (false, ids) => (errors, ids))) (errors, ids) tycons
                                  (* FIX: add rSE, rTE to ids - Rowan 22jan02 *)
              val out_i = case errors
                            of [] => okConv i
                             | _ => repeatedIdsError(i,errors) 
              val out_i = ElabInfo.plus_TypeInfo out_i (TypeInfo.INCLUDE_INFO (strids,tycons))
          in
            (T, E, rT, rE, OG.INCLUDEspec (out_i, out_sigexp), ids)
          end

        (* Empty specification *)                           (*rule 76*)
      | IG.EMPTYspec i => ([], E.empty, rT0, rE0, OG.EMPTYspec (okConv i), ids)

        (* Sequential specification *)                      (*rule 77*)
      | IG.SEQspec (i, spec1, spec2) =>
          let
            val (T1, E1, rT1, rE1, out_spec1, ids) = elab_spec (B, spec1, ids)
            val (T2, E2, rT2, rE2, out_spec2, ids) = 
                  elab_spec (B_plus_E_rT_rE (B,E1,rT1,rE1), spec2, ids)
            exception REF_ERROR of TypeScheme * TypeScheme
             val out_i =     (* This checks that VALsspec's refine the right type *)
              (VE.apply (fn (vid, VE.LONGVAR sigma2) =>  (* apply is done for effect only *)
                                (case VE.lookup (E.to_VE E1) vid
                                  of SOME (VE.LONGVAR sigma1) => 
                                     if TypeScheme.eq (sigma1, sigma2) then ()
                                     else raise REF_ERROR (sigma1, sigma2)
                                   | _ => () )  (* vid not in E1 *)
                          | _ => () )  (* VE2 element is not LONGVAR *)
                        (E.to_VE E2);  
               okConv i )   (* VE.apply completed normally *)
               handle REF_ERROR (sigma1, sigma2) => 
                 errorConv (i, ErrorInfo.INCOMPATIBLE_TYSCH (sigma1, sigma2))
          in
              (T1 @ T2, E.plus (E1,E2), rT1 @@ rT2, rE1 rE_plus_rE rE2, 
              OG.SEQspec (out_i, out_spec1, out_spec2), ids)
          end
                                                            (*rule 78*)
      | IG.SHARING_TYPEspec (i, spec, longtycon_withinfo_s) =>
          let
            val (T0, E, rT, rE, out_spec, ids) = elab_spec (B, spec, ids) (* first we elab spec *)
            val (T, out_longtycon_withinfo_s, error) =        (* Then we lookup the tycons in E *)
              List.foldR
                (fn IG.WITH_INFO (i, longtycon_i) =>
                 fn (T', out_longtycon_withinfo_s, error) =>
                 let val (T'', out_i, error) =
                   case lookup_longtycon E longtycon_i
                     of SOME tystr_i =>
                       let val theta_i = TyStr.to_theta tystr_i  in
                         case TypeFcn.to_TyName theta_i of
                           SOME t_i =>
                             if TyName.Set.member t_i (TyName.Set.fromList T0) then
                               (t_i::T', okConv i, error)
                             else
                               (T', errorConv (i, ErrorInfo.SHARING_TYPE_RIGID(longtycon_i, t_i)),
                                true)
                         | NONE => (T', errorConv (i, ErrorInfo.SHARING_TYPE_NOT_TYNAME
                                                        (longtycon_i, theta_i)), true)
                       end
                      | NONE => (T', errorConv (i, ErrorInfo.LOOKUP_LONGTYCON longtycon_i), true)
                 in
                   (T'', OG.WITH_INFO (out_i, longtycon_i) :: out_longtycon_withinfo_s, error)
                 end)
                ([], [], false) longtycon_withinfo_s
          in 
            if error then (T0, E, rT0, rE0, 
                           OG.SHARING_TYPEspec(okConv i, out_spec, out_longtycon_withinfo_s), ids)
            else let 
               
                      (* Eliminate dublicates; necessary for computing the correct T' and T0' below. *)
                   val T = TyName.Set.list(TyName.Set.fromList T)

                   (* Now, find a representative; if everything is allright, T 
                    * will have at least one member. *)
                   fun find ([], acc) = impossible "IG.SHARING_TYPEspec.find"
                     | find ([t], acc) = (t, acc)
                     | find (t::ts, acc) = if TyName.equality t then (t, ts @ acc)
                                           else find (ts,t::acc)
                   val (t0, T') = find(T, []) 
                   val arity = TyName.arity t0
                   val T0' = List.dropAll (fn t => List.exists (fn t' => TyName.eq(t,t')) T') T0
                   (* Return a failure result *)
                   fun fail i = (T0', E, rT, rE,
                                 OG.SHARING_TYPEspec (i, out_spec, out_longtycon_withinfo_s), ids)
                 in 
                   if List.forAll (fn t => TyName.arity t = arity) T' then
                     let val phi = Realisation.from_T_and_tyname (TyName.Set.fromList T', t0)
                         (* merge tystrs in rT for tynames in T' and adjust rE *)
                         val (rT2, rE2) = if !found_error then  (rT, rE)
                                          else rEnv.mergeT (rT, rE, T', t0) (* or raise No_match *)
                         val S = OG.SHARING_TYPEspec (okConv i, out_spec, out_longtycon_withinfo_s)
                     in (T0', Realisation.on_Env phi E, rT2, rE2, S, ids)
                     end handle rEnv.No_match =>
                             fail (rErrorConv1 (i, REI.SHARING_REFINEMENTS_DIFFER))
                   else fail (errorConv(i, ErrorInfo.SHARING_TYPE_ARITY T))
                 end
          end
      
      | IG.SHARINGspec (i, spec, longstrid_withinfo_s) =>
          let val (T0, E, rT, rE, out_spec, ids) = elab_spec (B, spec, ids)
              val (Es, out_longstrid_withinfo_s) =
                     List.foldR
                     (fn IG.WITH_INFO (i, longstrid) =>
                      (fn (Es, out_longstrid_withinfo_s) =>

                       (case lookup_longstrid' E longstrid of
                          SOME E_i => (E_i::Es,
                                       OG.WITH_INFO (okConv i, longstrid)
                                       ::out_longstrid_withinfo_s)
                        | NONE => (Es, OG.WITH_INFO
                                         (errorConv (i, ErrorInfo.LOOKUP_LONGSTRID longstrid),
                                          longstrid)::out_longstrid_withinfo_s))))
                        ([],[]) longstrid_withinfo_s
              val T0_set = TyName.Set.fromList T0
              fun member t = TyName.Set.member t T0_set
              fun out_this i2 = OG.SHARINGspec (i2, out_spec, out_longstrid_withinfo_s)
              fun fail i2 = (T0, E, rT, rE, (out_this i2), ids) (* result if errors *)
          in let val (T', phi, rT2, rE2) = share (member, Es, rT, rE)
                 val T0' = List.dropAll (fn t => List.exists (fn t' => TyName.eq(t,t')) T') T0 
             in
                 (T0', Realisation.on_Env phi E, rT2, rE2, out_this (okConv i), ids)
             end handle Share error_info => fail (errorConv (i, error_info))
                      | rEnv.No_match => fail (rErrorConv1(i, REI.SHARING_REFINEMENTS_DIFFER))
          end
      ) (*specs*)


    (**********************************************)
    (* Value Descriptions - Definition v3 page 41 *)
    (**********************************************)

    and elab_valdesc ((C : Context, rC : rContext), valdesc : IG.valdesc, ids : ids)
      : ((VarEnv * rVarEnv) * OG.valdesc * ids) =
                                                            (*rule 79 + *)
      case valdesc 
        of IG.VALDESC (i, id, ty, valdesc_opt) =>
          let val (error, ids) = add_ids_vid(ids,id)
              val (in_rvids, ids) = add_ids_rvid(ids,id)  (* add to rvids also, if not there *)
              val explicittyvars = EqSet.list (Environments.ExplicitTyVarsTy ty)
              val C' = C.plus_U(C, explicittyvars)
              val (typ_opt, out_ty) = ElabDec.elab_ty (C', ty)
              val ((VE2, rVE2), out_valdesc_opt, ids) = 
                    elab_X_opt'' ((C,rC), valdesc_opt, ids) elab_valdesc (VE.empty, rEnv.emptyVE)
              val out_VE = case typ_opt of NONE => VE2
                                         | SOME typ =>
                                 (VE.plus (VE.singleton_var (id, TypeScheme.from_Type typ), VE2))
              val (_, rC') = rEnv.C_plus_U' (rC, explicittyvars)
              val (out_rVE, errs) =               (* may as well close the sort here *)
                 if (in_rvids orelse !found_error) then  (rVE2, [])  else
                   letCV (RefDec.ref_ty_covar (rC', out_ty))  (fn sort =>  (* letCV is HOAS *)
                     rEnv.VE_plus_VE (rEnv.singleVarVE (id, RO.Close_Sort sort), rVE2) ) true
              val out_i = 
                if error then
                  repeatedIdsError (i, [ErrorInfo.ID_RID id])
                else if IG.DecGrammar.is_'true'_'nil'_etc id then 
                  errorConv (i, ErrorInfo.SPECIFYING_TRUE_NIL_ETC [id])
                else case errs of [] => okConv i
                                | _ => rErrorConv(i, errs)
          in
            ((out_VE, out_rVE), OG.VALDESC (out_i, id, out_ty, out_valdesc_opt), ids)  
          end

    and elab_valsdesc ((C : Context, rC : rContext), valdesc : IG.valdesc, ids : ids)
      : ((VarEnv * rVarEnv) * OG.valdesc * ids) =
      case valdesc 
        of IG.VALDESC (i, id, ty, valdesc_opt) =>
          let val (in_rvids, ids) = add_ids_rvid(ids,id)  (* add to rvids, if not there *)
               val explicittyvars = EqSet.list (Environments.ExplicitTyVarsTy ty)
              val C' = C.plus_U(C, explicittyvars)
              val (typ_opt, out_ty) = ElabDec.elab_srt (C', ty)
              val (_, rC') = rEnv.C_plus_U' (rC, explicittyvars)
              val (sort, errs) =
                 if !found_error then  (RO.bogusSort, [])  
                 else  RefDec.ref_ty_covar (rC', out_ty) true
              val sort_sch = RO.Close_Sort sort           (* may as well close the sort here *)
              val ((VE2, rVE2), out_valdesc_opt, ids) = 
                    elab_X_opt'' ((C, rC), valdesc_opt, ids) elab_valsdesc (VE.empty, rEnv.emptyVE)
              val out_VE = case typ_opt of NONE => VE2
                                         | SOME typ =>
                                 (VE.plus (VE.singleton_var (id, TypeScheme.from_Type typ), VE2))
              val out_rVE = rEnv.VE_plus_VE (rEnv.singleVarVE (id, sort_sch), rVE2)
              val out_i = 
                if IG.DecGrammar.is_'true'_'nil'_etc id then 
                  errorConv (i, ErrorInfo.SPECIFYING_TRUE_NIL_ETC [id])
                else case errs of [] => okConv i
                                | _ => rErrorConv(i, errs)
          in
            ((out_VE, out_rVE), OG.VALDESC (out_i, id, out_ty, out_valdesc_opt), ids)  
          end

    (*********************************************)
    (* Type Descriptions - Definition v3 page 41 *)
    (*********************************************)

    and elab_typdesc (equality : bool) (C : Context, typdesc : IG.typdesc, ids : ids)
      : (TyName list * TyEnv * rT * rEnv.TyEnv * rEnv.SortEnv * OG.typdesc * ids) =
                                                            (*rule 80*)
       (case typdesc of
         IG.TYPDESC (i, explicittyvars, tycon, typdesc_opt) =>
           let
             val tyvars = map TyVar.from_ExplicitTyVar explicittyvars
             val tyvars_repeated = repeaters (op =) explicittyvars
             val arity = List.size explicittyvars
             val t = TyName.freshTyName {tycon=tycon, arity=arity, equality=equality}
             val theta = TypeFcn.from_TyName t
             val tystr = TyStr.from_theta_and_VE (theta, VE.empty)
             val (error, ids) = add_ids_tycon(ids,tycon)

             val sortname = 
                 SortName.freshSortName 
                   {conjuncts=[tycon], variance= map ExplTyVar.variance explicittyvars, tyname=t}
             val sortFcn = RO.SortName_in_SortFcn sortname

             val rRL = rEnv.singleRL sortname
             val rR = rEnv.singleR (sortname, rEnv.emptyRC)

             val covariant = List.forAll (ExplTyVar.covariant o ExplTyVar.variance) explicittyvars
             val _ = if covariant then (SortName.set_Covariance sortname)
                     else ()
             val rtystr = rEnv.mkTyStr(rEnv.emptyCE, rR, sortname, covariant, rRL)
             val rT = rEnv.singleT(t, rtystr)
             val rTE = rEnv.singleTE(tycon, sortFcn)
             val rRE = rEnv.singleRE(tycon, sortFcn)
             val (T2, TE2, rT2, rTE2, rRE2, out_typdesc_opt, ids) = 
               case typdesc_opt
                 of NONE => ([], TE.empty, rT0, rTE0, rRE0, NONE, ids)
                  | SOME typdesc' => let val (T2,TE2,rT2,rTE2,rRE2, out_typdesc,ids) = 
                                             elab_typdesc equality (C, typdesc', ids)
                                     in (T2,TE2,rT2,rTE2,rRE2, SOME out_typdesc, ids)
                                     end
             val out_i = 
                 if error then  repeatedIdsError (i, [ErrorInfo.TYCON_RID tycon])
                 else case  tyvars_repeated
                        of [] => okConv i
                         | _ => repeatedIdsError (i, map ErrorInfo.TYVAR_RID 
                                                     (map TyVar.from_ExplicitTyVar tyvars_repeated) )
           in
             (t::T2, TE.plus (TE.singleton (tycon, tystr), TE2),
              rT @@ rT2, rEnv.TE_plus_TE (rTE, rTE2), rEnv.RE_plus_RE (rRE, rRE2), 
              OG.TYPDESC (out_i, explicittyvars, tycon, out_typdesc_opt), ids)
           end )

    (*********************************************************)
    (* Sort and SubSort Descriptions - SSML extension        *)
    (*********************************************************)
(*
    and elab_subsortdesc (rC : rContext, IG.SUBSORTDESC (i, longsrtcons1, longsrtcons2)) 
         : SortName list * SortName list * OG.subsortdesc =
        let
          fun lookup [] : SortName list * TyCon.longtycon OG.WithInfo list =  ([], [])
            | lookup (IG.WITH_INFO (i2, longsrtcon) :: tail) = 
              let val (sns_tail, out_tail) = lookup tail
                  fun fail () = (sns_tail, rErrorConv1(i2, REI.LOOKUP_LONGSORTCON longsrtcon))
                  val (sns, out_i2) = 
                      case rEnv.Lookup_longsortcon (rC, longsrtcon)
                        of NONE => fail ()
                         | SOME sortfcn =>
                           (case RO.SortFcn_to_SortName sortfcn
                              of NONE => fail ()  (* Error type isn't quite right here. *)
                               | SOME sn => (sn :: sns_tail, okConv i2) )
              in
                (sns, OG.WITH_INFO (out_i2, longsrtcon) :: out_tail)
              end

          val (sns1, out_longsrtcons1) = lookup longsrtcons1
          val (sns2, out_longsrtcons2) = lookup longsrtcons2
        in
          (sns1, sns2, OG.SUBSORTDESC (okConv i, out_longsrtcons1, out_longsrtcons2))
        end
*)

    and elab_sortdesc (rC : rContext, sortdesc : IG.sortdesc, ids : ids)
      : (TyEnv * rT * rEnv.SortEnv) * OG.sortdesc * ids =
       (case sortdesc of
         IG.SORTDESC (i, explicittyvars, srtcon, isSub, longtycon, sortdesc_opt) =>
           let
             val tyvars = map TyVar.from_ExplicitTyVar explicittyvars
             val tyvars_repeated = repeaters (op =) explicittyvars
             val arity = List.size explicittyvars
             val variance = map ExplTyVar.variance explicittyvars
             val svars = map RO.SVofTV tyvars
             val (error, ids) = add_ids_tycon(ids,srtcon) (* FIX: error not used currently *)

(*             val (super_sns, sub_sns, out_subsortdesc) = elab_subsortdesc (rC, subsortdesc) *)
(*             val sns = sub_sns @ super_sns *)
             fun fail () = (TE.empty, rT0, rRE0, rErrorConv1 (i, REI.LOOKUP_LONGSORTCON longtycon))
             val (TE, rT, rRE, out_i) = 
               if !found_error then  (TE.empty, rT0, rRE0, okConv i)
               else 
               case rEnv.Lookup_longsortcon (rC, longtycon)
                of NONE => fail ()
                 | SOME sortfcn =>
                   (case RO.SortFcn_to_SortName sortfcn
                     of NONE => fail ()  (* Error type isn't quite right here. *)
                      | SOME compat_sn => 
                        let
                            val tyname = SortName.tyname compat_sn
                            val theta = TypeFcn.from_TyName tyname
                            val tystr = TyStr.from_theta_and_VE (theta, VE.empty)
                            val TEnew = TE.singleton (srtcon, tystr)
                            val (rTnew, sn_new) = 
                              if isSub then 
                                rEnv.extend_lattice (rC, tyname, srtcon, variance, [] ,[compat_sn])
                              else
                                rEnv.RL_add_abstract (rC, tyname, srtcon, variance)

                            val rREnew = rEnv.singleRE(srtcon, RO.SortName_in_SortFcn sn_new)
                        in
                           (TEnew, rTnew, rREnew, okConv i)
                        end)

             val rCnew = rEnv.C_plus_T (rC, rT)
             val ((TE2, rT2, rRE2), out_sortdesc_opt, ids) = 
                 elab_X_opt'' (rCnew, sortdesc_opt, ids) elab_sortdesc (TE.empty, rT0, rRE0)
             val out_sortdesc = 
                   OG.SORTDESC (out_i, explicittyvars, srtcon, isSub, longtycon, out_sortdesc_opt)
           in
             ((TE.plus (TE, TE2), rT @@ rT2, rEnv.RE_plus_RE (rRE, rRE2)), out_sortdesc, ids)
           end)


    (*************************************************)
    (* Datatype Descriptions - Definition v3 page 42 *)
    (*************************************************)
                                                            (*rule 81*)
    and elab_datdesc (C : Context, datdesc : IG.datdesc, ids : ids)
      : ((VarEnv * TyEnv) * OG.datdesc * ids) =
        (case datdesc of
          IG.DATDESC (i, explicittyvars, tycon, condesc, datdesc_opt) =>
            let
              val _ = Level.push()

              val (tyvars, C') = C.plus_U'(C, explicittyvars)

              val (theta, _) = TyStr.to_theta_and_VE
                                 (noSome (lookup_tycon C tycon) "datdesc(1)")
              val t = noSome (TypeFcn.to_TyName theta) "datdesc(2)"
              val taus = map Type.from_TyVar tyvars
              val tau = Type.from_ConsType (Type.mk_ConsType (taus, t))
              val (constructor_map, out_condesc, ids) = elab_condesc (C', tau, condesc, ids)
              val VE = constructor_map.to_VE constructor_map
                    
              (*The following lists must be made before closing VE,
               as explicit tyvars are turned into ordinary tyvars when
               closed.*)

              val tyvars_repeated = repeaters (op =) explicittyvars
              val tyvars_not_bound =
                List.all (fn tyvar => not (member tyvar explicittyvars)) 
                (IG.getExplicitTyVarsCondesc condesc)

              val _ = Level.pop()
              val VE_closed = VE.close VE
              val theta = TypeFcn.from_TyName t
              val tystr = TyStr.from_theta_and_VE (theta, VE_closed)

              val (error, ids) = add_ids_tycon(ids,tycon)
              val ((VE', TE'), out_datdesc_opt, ids) = elab_X_opt'' (C, datdesc_opt, ids) 
                                                       elab_datdesc (VE.empty, TE.empty)
            in
              (case (if error then [ErrorInfo.TYCON_RID tycon] else [])
                    @ map ErrorInfo.TYVAR_RID (map TyVar.from_ExplicitTyVar tyvars_repeated)
                 of [] =>
                   (case tyvars_not_bound 
                      of [] => 
                        ( (VE.plus (VE_closed, VE'),
                           TE.plus (TE.singleton (tycon, tystr), TE')) ,
                         OG.DATDESC ((*if TyCon.is_'true'_'nil'_etc tycon then
                                       errorConv (i, ErrorInfo.SPECIFYING_TRUE_NIL_ETC [])
                                      else *) (* Removed this for the basis stub. - Rowan *)
                                     if TyCon.is_'it' tycon then
                                        errorConv (i, ErrorInfo.SPECIFYING_IT)
                                          else okConv i,
                                            explicittyvars, tycon, out_condesc,
                                            out_datdesc_opt), ids )
                       | _ => 
                        ( (VE', TE') ,
                         OG.DATDESC (errorConv(i, ErrorInfo.TYVARS_NOT_IN_TYVARSEQ
                                               (map TyVar.from_ExplicitTyVar tyvars_not_bound)),
                                     explicittyvars, tycon, out_condesc,
                                     out_datdesc_opt), ids ))
                  | repeated_ids_errorinfos => 
                      ( (VE', TE') ,
                       OG.DATDESC (repeatedIdsError (i, repeated_ids_errorinfos),
                                   explicittyvars, tycon, out_condesc,
                                   out_datdesc_opt), ids ))
            end)

    (****************************************************)
    (* Constructor Descriptions - Definition v3 page 42 *)
    (****************************************************)

    and elab_condesc (C : Context, tau : Type, condesc : IG.condesc, ids : ids)
      : (constructor_map * OG.condesc * ids) =

       case condesc of
                                                            (*rule 82*)
         IG.CONDESC (i, longcon, NONE, condesc_opt) =>
           let
             val sigma = TypeScheme.from_Type tau
             val (strid, con) = IG.DecGrammar.Ident.decompose longcon           
             val (error, ids) = add_ids_vid(ids,con)
             val (constructor_map, out_condesc_opt, ids) = elab_condesc_opt(C, tau, condesc_opt, ids)
           in
             (constructor_map.add con sigma constructor_map,
              OG.CONDESC (out_i_for_condesc con constructor_map i error strid, longcon,
                          NONE, out_condesc_opt), ids)
           end

       | IG.CONDESC (i, longcon, SOME ty, condesc_opt) =>
           let val (strid, con) = IG.DecGrammar.Ident.decompose longcon           
               val (error, ids) = add_ids_vid(ids,con)
               val (constructor_map, out_condesc_opt, ids) = elab_condesc_opt (C, tau, condesc_opt, ids)
               fun result out_ty cmap = (cmap, OG.CONDESC (out_i_for_condesc con constructor_map i error strid, longcon,
                                                           SOME out_ty, out_condesc_opt), ids)
           in case ElabDec.elab_ty (C, ty)
                of (SOME tau', out_ty) =>
                  let val arrow = Type.mk_Arrow (tau', tau)
                      val sigma = TypeScheme.from_Type arrow
                      val cmap' = constructor_map.add con sigma constructor_map
                  in result out_ty cmap'
                  end
                 | (NONE, out_ty) => result out_ty constructor_map
           end

    and out_i_for_condesc con constructor_map i error strid = 
          if error then repeatedIdsError (i, [ErrorInfo.CON_RID con])
          (* else if IG.DecGrammar.is_'true'_'nil'_etc con then 
            errorConv (i, ErrorInfo.SPECIFYING_TRUE_NIL_ETC [con]) *) (* Removed: - Rowan *)
          else if IG.DecGrammar.is_'it' con then 
            errorConv (i, ErrorInfo.SPECIFYING_IT)
          else case strid of [] => okConv i
                           | _ => errorConv (i, ErrorInfo.QUALIFIED_CON)

    and elab_condesc_opt
      (C : Context, tau : Type, condesc_opt : IG.condesc option, ids : ids)
      : (constructor_map * OG.condesc option * ids) =
          elab_X_opt'' (C, condesc_opt, ids) 
            (fn (C, condesc, ids) => elab_condesc (C, tau, condesc, ids))
              constructor_map.empty

    (**** REFINEMENTS FOR DATATYPE DESCRIPTIONS - almost of a copy of RefDec.ref_datbind ****)
    and ref_datdesc (C, OG.DATDESC(i, tyvar_list, tycon, condesc, datdesc_opt),
                     covariant_sofar) errflag = 
      let
        val sortfcn = case rEnv.Lookup_sortcon (C, tycon)
                        of SOME sortfcn => sortfcn
                         | NONE => Crash.impossible "ElabTopdec.ref_datdesc(1)"
        val sortname = case RO.SortFcn_to_SortName sortfcn
                         of SOME sortname => sortname
                          | NONE => Crash.impossible "ElabTopdec.ref_datdesc(2)"
        val tyname = SortName.tyname sortname 
        val (sv_list, C1) = rEnv.C_plus_U' (C, tyvar_list)
        val sort_list = map RO.mkSortSortVar sv_list
        val sort = RO.mkSortConsSort
                     (RO.mkConsSort(sort_list, sortname))
        val ((CE, RC, covariant_condesc), errs1) = ref_condesc (C1, sort, condesc) errflag
        val VE = rEnv.CE_to_VE CE

(*        val empty_sn = SortName.freshEmptySortName tyname*)
(*        val R = rEnv.R_plus_R(rEnv.singleR(sortname, RC), rEnv.singleR(empty_sn, rEnv.emptyRC))*)
(*        val RL = rEnv.addRL(rEnv.addRL(rEnv.singleRL sortname, (sortname, empty_sn), empty_sn),*)
(*                           (empty_sn, empty_sn), empty_sn)*)
        val R = rEnv.singleR(sortname, RC)
        val RL = rEnv.singleRL sortname

        val ((T', VE', covariant), errs2) = 
          ref_datdesc_opt(C, datdesc_opt, covariant_sofar andalso covariant_condesc) errflag
        val _ = if covariant then (SortName.set_Covariance sortname)
(*                                   SortName.set_Covariance empty_sn)  *)
                else ()
        val tystr = rEnv.mkTyStr(CE, R, sortname, covariant, RL)
        val outT = rEnv.T_plus_T (rEnv.singleT (tyname, tystr), T')
      in
        ((outT, rEnv.VE_plus_VE (VE, VE'), covariant), errs2 @ errs1)
      end

    and ref_datdesc_opt (C, SOME datdesc, covariant_sofar) errflag =
          ref_datdesc (C, datdesc, covariant_sofar) errflag
      | ref_datdesc_opt (C, NONE, covariant_sofar) errflag =
          Comp.noErr (rEnv.emptyT, rEnv.emptyVE, covariant_sofar)

    (***** REFINEMENTS FOR CONSTRUCTOR DESCRIPTIONS  *****)
    (* bool is whether type has only covariant constructors - used for optimization *)
    and ref_condesc (C, sort, condesc) errflag
          : (rEnv.ConEnv * rEnv.SortCons * bool) * (Comp.Error list) = 
      case condesc of
        OG.CONDESC(i, longcon, ty_opt, condesc_opt) =>         
          let
            fun covariant_sortname sortname = 
                case rEnv.Lookup_tyname (C, SortName.tyname sortname)
                   of SOME tystr => rEnv.covariant_of_TyStr tystr
                    | NONE => Crash.impossible "ElabTopdec.ref_condesc"
            val ((sort1, sortFcn_opt, covariant), errs1) = 
              case ty_opt
                of SOME ty => 
                    letCV (RefDec.ref_ty_covar (C, ty)) (fn sort2 =>
                    (RO.mkSortArrow (sort2, sort), 
                     SOME (RO.mkSortFcn (RO.sortvarsSort sort, sort2)), (* Note order! *)
                     RO.covariant_sort covariant_sortname sort2)  ) errflag
                 | NONE => Comp.noErr (sort, NONE, true)

            val sortScheme = RO.Close_Sort sort1    (* Vars in order of last occurance *)
            val ((CE, RC, covariant_rest), errs2) =
              ref_condesc_opt(C, sort, condesc_opt) errflag
            val con = Ident.id_of_longid longcon
          in
              ((rEnv.CE_plus_CE (rEnv.singleCE(con, sortScheme), CE),
                rEnv.RC_plus_RC (RC, rEnv.singleRC(con, sortFcn_opt)),
                covariant_rest andalso covariant),
               errs2 @ errs1)
          end

    and ref_condesc_opt(C, sort, condesc_opt) errflag : 
        (rEnv.ConEnv * rEnv.SortCons * bool) * (Comp.Error list) =
      case condesc_opt of SOME(condesc) => ref_condesc(C, sort, condesc) errflag
                        | NONE => Comp.noErr (rEnv.emptyCE, rEnv.emptyRC, true)


    (**************************************************)
    (* Exception Descriptions - rule 83             *)
    (**************************************************)

    and elab_exdesc ((C, rC), exdesc : IG.exdesc, ids : ids)
      : ((VarEnv * rVarEnv) * OG.exdesc * ids) =
                                                            (*rule 83*)
       case exdesc of
         IG.EXDESC (i, excon, NONE, exdesc_opt) =>
           let val (error, ids) = add_ids_vid(ids,excon)
               val ((VE, rVE), out_exdesc_opt, ids) = 
                   elab_X_opt'' ((C, rC), exdesc_opt, ids) elab_exdesc (VE.empty, rVE0)
           in
             ((VE.plus (VE.singleton_excon (excon, Type.Exn), VE), 
               rEnv.VE_plus_VE (rEnv.singleVE (excon, rEnv.LONGEXCON RO.SortExn), rVE)),
              OG.EXDESC (out_i_for_exdesc excon VE i error, excon, NONE,
                         out_exdesc_opt), ids)
           end
        | IG.EXDESC (i, excon, SOME ty, exdesc_opt) =>
           let val (error, ids) = add_ids_vid(ids,excon)
               val ((VE, rVE), out_exdesc_opt, ids) = 
                   elab_X_opt'' ((C, rC), exdesc_opt, ids) elab_exdesc (VE.empty, rVE0)
               fun result VE_rVE out_i out_ty = 
                   (VE_rVE, OG.EXDESC (out_i, excon, SOME out_ty, out_exdesc_opt), ids)
           in case ElabDec.elab_ty (C, ty)
                of (SOME tau, out_ty) =>    (* FIX: check found_errors here before calling RefDec.  *)
                  let val tyvars = tyvars_Type tau
                        val arrow = Type.mk_Arrow (tau, Type.Exn)
                      val VE' = VE.plus (VE.singleton_excon (excon, arrow), VE)
                      val (sort1, errs) = RefDec.ref_ty_covar (rC, out_ty) true
                      val sort2 = RO.mkSortArrow (sort1, RO.SortExn)
                      val rVE' = rEnv.VE_plus_VE (rEnv.singleVE (excon, rEnv.LONGEXCON sort2), rVE)
                      val out_i = case (tyvars, errs) 
                                    of ([], []) => out_i_for_exdesc excon VE i error
                                     | (_::_, _) => errorConv(i, ErrorInfo.EXDESC_SIDECONDITION)
                                     | _ => rErrorConv(i, errs)
                  in
                    result (VE', rVE') out_i out_ty
                  end
                 | (NONE, out_ty) => 
                  let val out_i = out_i_for_exdesc excon VE i error
                  in result (VE, rVE) out_i out_ty
                  end
           end

    and out_i_for_exdesc excon VE i error =
      if error then repeatedIdsError (i, [ErrorInfo.EXCON_RID excon])
      else if IG.DecGrammar.is_'true'_'nil'_etc excon then
        errorConv (i, ErrorInfo.SPECIFYING_TRUE_NIL_ETC [excon])
           else if IG.DecGrammar.is_'it' excon then
             errorConv (i, ErrorInfo.SPECIFYING_IT)
                else okConv i

    (***************************************************)
    (* Structure Desctriptions - Definition v3 page 41 *)
    (***************************************************)

    and elab_strdesc (B : Basis, strdesc : IG.strdesc, ids : ids)
      : (TyName list * StrEnv * rT * rStrEnv * OG.strdesc * ids) =
                                                            (*rule 84*)
      (case strdesc of
         IG.STRDESC (i, strid, sigexp, NONE) =>
           let val (T, E, rT, rE, out_sigexp) = elab_sigexp' (B, sigexp)
               val (error,ids) = add_ids_strid(ids,strid)
               val out_i = if error then repeatedIdsError(i, [ErrorInfo.STRID_RID strid])
                           else okConv i
           in
             (T, SE.singleton (strid, E), rT, rEnv.singleSE (strid, rE),
              OG.STRDESC (out_i, strid, out_sigexp, NONE), ids)
           end
       | IG.STRDESC (i, strid, sigexp, SOME strdesc) =>
           let
             val (T, E, rT, rE, out_sigexp) = elab_sigexp' (B, sigexp)
             val (error, ids) = add_ids_strid(ids,strid)
             val (T', SE', rT', rSE', out_strdesc, ids) = elab_strdesc (B, strdesc, ids)
             val out_i = if error then repeatedIdsError (i, [ErrorInfo.STRID_RID strid])
                         else okConv i
           in
             (T @ T', SE.singleton (strid, E) SE_plus_SE SE', 
              rT @@ rT', rEnv.SE_plus_SE (rEnv.singleSE (strid, rE), rSE'),
              OG.STRDESC (out_i, strid, out_sigexp, SOME out_strdesc), ids)
           end)



    (****************************************************)
    (* Functor Declarations - Definition v3 pages 42-43 *)
    (****************************************************)

    and elab_fundec (prjid : prjid, B : Basis, fundec : IG.fundec)
      : (FunEnv * OG.fundec) =
                                                            (*rule 85*)
      (case fundec of
         IG.FUNCTORfundec (i, funbind) =>
           let val (F, out_funbind) = elab_funbind (prjid, B, funbind)
           in
             (F, OG.FUNCTORfundec(okConv i, out_funbind))
           end)

    (********************************************)
    (* Functor Bindings - Definition v3 page 43 *)
    (********************************************)

    and elab_funbind (prjid : prjid, B : Basis, funbind : IG.funbind)
      : (FunEnv * OG.funbind) =
                                                            (*rule 86*)
      (case funbind of
        IG.FUNBIND (i, funid, strid, sigexp, strexp, funbind_opt) =>
          let
            val (T_E, out_sigexp) = elab_sigexp (B, sigexp)
            val (T, E, rT, rE) = Sigma.to_T_E_rT_rE T_E
            val B' = B_plus_E_rT_rE (B, E.from_SE (SE.singleton (strid,E)), 
                                     rT, rEnv.SE_in_E (rEnv.singleSE (strid, rE)))
(*
            fun print_basis B =
              PrettyPrint.outputTree(print,ModuleEnvironments.B.layout B, 100)
            val _ = print_basis B'
*)
            val (T'', E', rT2, rE2, out_strexp) = elab_strexp(B', strexp)
            val T' = TyName.Set.intersect (tynames_E E') (TyName.Set.fromList T'')
            val T'E' = Sigma.from_T_E_rT_rE (T', E', rT2, rE2)
            val (F, out_funbind_opt) = elab_X_opt (prjid, B, funbind_opt) elab_funbind F.empty
            val out_i = if EqSet.member funid (F.dom F)
                        then repeatedIdsError (i, [ErrorInfo.FUNID_RID funid])
                        else ElabInfo.plus_TypeInfo (okConv i) 
                          (TypeInfo.FUNBIND_INFO {argE=E,elabBref=ref B',T=TyName.Set.fromList T'',
                                                  resE=E',opaq_env_opt=NONE})
         val _ = if !Flags.DEBUG_ELABTOPDEC then print "elab_funbind: END\n" else ()
          in
            (F.singleton (funid, (prjid,Phi.from_T_E_rT_rE_Sigma (T, E, rT, rE, T'E'))) F_plus_F F,
             OG.FUNBIND (out_i, funid, strid, out_sigexp, out_strexp,
                         out_funbind_opt))
          end)


    (************************************************************)
    (* Top-level Declarations - Definition 1997, rules 87-89    *)
    (************************************************************)

    and elab_topdec (prjid : prjid, B : Basis, topdec : IG.topdec)    (* we check for free tyvars later *)
          : (Basis * OG.topdec) =
      case topdec 
        of IG.STRtopdec (i, strdec, topdec_opt) =>                                      (* 87 *)
          let val (_, E, rT, rE, out_strdec) = elab_strdec(B, strdec)
              val (B', out_topdec_opt) = elab_topdec_opt(prjid, B_plus_E_rT_rE (B, E, rT, rE), topdec_opt)
              val B'' = (B_from_E_rT_rE (E, rT, rE)) B_plus_B B'
          in (B'', OG.STRtopdec(okConv i, out_strdec, out_topdec_opt))
          end
         | IG.SIGtopdec (i, sigdec, topdec_opt) =>                                      (* 88 *)
          let val (G, out_sigdec) = elab_sigdec(B, sigdec)
              val (B', out_topdec_opt) = elab_topdec_opt(prjid, B B_plus_G G, topdec_opt)
              val B'' = (B.from_G G) B_plus_B B'
              val _ = if !Flags.DEBUG_ELABTOPDEC then print "elab_topdec: END\n" else ()
          in (B'', OG.SIGtopdec(okConv i, out_sigdec, out_topdec_opt))
          end
         | IG.FUNtopdec (i, fundec, topdec_opt) =>                                      (* 89 *)
          let val (F, out_fundec) = elab_fundec(prjid, B, fundec)
              val (B', out_topdec_opt) = elab_topdec_opt(prjid, B B_plus_F F, topdec_opt)
              val B'' = (B.from_F F) B_plus_B B'
          in (B'', OG.FUNtopdec(okConv i, out_fundec, out_topdec_opt))
          end

    and elab_topdec_opt (prjid : prjid, B : Basis, topdec_opt : IG.topdec option) : (Basis * OG.topdec option) =
      case topdec_opt
        of SOME topdec => let val (B', out_topdec) = elab_topdec(prjid, B, topdec)
                          in (B', SOME out_topdec)
                          end
         | NONE => (B.empty, NONE)

    (* Check for free type variables: Free type variables are not
     * allowed in the basis resulting from elaborating a top-level
     * declaration. However, free type variables may occur in the
     * resulting elaborated top-level declaration - namely if an
     * identifier has been `shadowed' in the resulting basis. Since
     * elaboration may succeed in this case, we could go ahead and
     * unify such free type variables with `int', say, to have the
     * property that no topdec's contains free type variables. This we
     * do not do, so we should make sure that all compilation phases
     * deals correctly with free type variables. *)

    fun modify_info_topdec (topdec, i) = case topdec of 
      OG.STRtopdec(_,strdec,topdecopt) => OG.STRtopdec(i,strdec,topdecopt)
    | OG.SIGtopdec(_,sigdec,topdecopt) => OG.SIGtopdec(i,sigdec,topdecopt)
    | OG.FUNtopdec(_,fundec,topdecopt) => OG.FUNtopdec(i,fundec,topdecopt)

    fun elab_topdec' (prjid : prjid, B : Basis, topdec) : Basis * OG.topdec =


      let val res as (B',topdec') = elab_topdec(prjid,B,topdec)
      in case tyvars_B' B'
           of [] => res
            | tyvars => let val i = IG.info_on_topdec topdec 
                            val ids = map #1 tyvars
                            val out_i = errorConv(i, ErrorInfo.UNGENERALISABLE_TYVARS ids)
                        in (B', modify_info_topdec(topdec',out_i))
                        end
      end
 
    val elab_topdec = fn a => (TyName.Rank.reset();
                               found_error := (!skipRefinement);  (* No errors yet, and rBasis is ok *)
                               (*print ("\nElabTopdec: LEVEL = " ^ StatObject.Level.pr(StatObject.Level.current())^"\n");*)
                               let val res = elab_topdec' a
                               in TyName.Rank.reset();
                                 res
                               end)

    (********
    Printing functions
    ********)

    val layoutStaticBasis = B.layout
  end;
