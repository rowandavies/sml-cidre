
signature TOOLS =
  sig
    structure BasicIO: BASIC_IO
    structure Report: REPORT

    structure FinMap: FINMAP
        where type Report = Report.Report
    structure FinMapEq : FINMAPEQ
        where type Report = Report.Report
    structure SortedFinMap: SORTED_FINMAP
        where type Report = Report.Report
    structure IntFinMap : MONO_FINMAP where type dom = int
    structure PrettyPrint: PRETTYPRINT
        where type Report = Report.Report
    structure Flags: FLAGS
        where type Report = Report.Report
            
    structure Timestamp: TIMESTAMP
    structure ListHacks: LIST_HACKS
    structure Crash: CRASH
    structure Timing: TIMING
  end;


functor Tools(): TOOLS =
  struct
    structure BasicIO = BasicIO()
    structure Crash = Crash(structure BasicIO = BasicIO)
    structure Report = Report(structure BasicIO = BasicIO)
    structure Flags = Flags(structure Crash = Crash
			    structure Report = Report)
    structure Timestamp = Timestamp()

    structure PrettyPrint = PrettyPrint(structure Report = Report
					structure Crash = Crash
                                        structure Flags = Flags
				       )
    structure IntFinMap = IntFinMap(structure Report = Report
				    structure PP = PrettyPrint
				   )

    structure FinMap = FinMap(structure Report = Report
			      structure PP = PrettyPrint
			     )

    structure FinMapEq = FinMapEq(structure Report = Report
				  structure PP = PrettyPrint
				    )

    structure SortedFinMap = SortedFinMap(structure Report = Report
					  structure PP = PrettyPrint
					 )

    structure Timing = Timing(structure Flags = Flags
			      structure Crash = Crash)
    structure ListHacks = ListHacks()
  end;



signature ALL_INFO =
  sig
    structure SourceInfo      : SOURCE_INFO
    structure DFInfo          : DF_INFO
    structure ParseInfo       : PARSE_INFO
      where type SourceInfo = SourceInfo.SourceInfo
      where type DFInfo = DFInfo.DFInfo
    structure OverloadingInfo : OVERLOADING_INFO

    structure RefineErrorInfo : REFINE_ERROR_INFO
      where type SourceInfo = SourceInfo.SourceInfo

    structure ElabInfo : ELAB_INFO
      where type ParseInfo.ParseInfo = ParseInfo.ParseInfo
      where type ParseInfo.SourceInfo = ParseInfo.SourceInfo
      where type OverloadingInfo.OverloadingInfo = OverloadingInfo.OverloadingInfo
      where type ErrorInfo.RefineErrorInfo.ErrorInfo = RefineErrorInfo.ErrorInfo

    structure RefInfo : REF_INFO
      where type ElabInfo.ElabInfo = ElabInfo.ElabInfo
      where type ElabInfo.ParseInfo.SourceInfo = RefineErrorInfo.SourceInfo
      where type Comp.Error = ElabInfo.ErrorInfo.RefineErrorInfo.Error

end;



signature BASICS =
  sig
    structure Tools : TOOLS 
    structure StrId : STRID

    structure Ident : IDENT
      where type strid = StrId.strid

    structure InfixBasis : INFIX_BASIS
      where type id = Ident.id
      where type Report = Tools.Report.Report

    structure SCon : SCON

    structure Lab : LAB
    structure TyVar : TYVAR

    structure TyCon : TYCON
      where type strid = StrId.strid

    structure Name : NAME

    structure TyName : TYNAME
      where type tycon = TyCon.tycon
      where type name = Name.name

    structure SortName : SORTNAME
      where type sortcon = TyCon.tycon
      where type TyName  = TyName.TyName
      where type Variance = TyVar.Variance
      where type name = Name.name

    structure StatObject : STATOBJECT
      where type TyName    = TyName.TyName (* Was structure sharing. *)
      where type ExplicitTyVar = TyVar.SyntaxTyVar
      where type strid     = StrId.strid
      where type scon      = SCon.scon
      where type lab       = Lab.lab
      where type tycon = TyCon.tycon

    structure RefObject : REFOBJECT
      where type SortName    = SortName.SortName
      where type SortName.Set.Set = SortName.Set.Set
      where type scon        = SCon.scon
      where type lab         = Lab.lab
      where type TyVar       = StatObject.TyVar
      where type TypeFcn     = StatObject.TypeFcn
      where type Type        = StatObject.Type
      where type TVNames     = StatObject.TVNames
      where type TyName      = TyName.TyName
      where type trealisation = StatObject.realisation
      where type ExplicitTyVar.Variance    = TyVar.Variance
      where type ('a,'b) sortedFinMap = ('a,'b) Tools.SortedFinMap.map

                        (* where type realisation = StatObject.realisation *)
                        
    structure SigId : SIGID
    structure FunId : FUNID

    structure LexBasics: LEX_BASICS
      where type Report = Tools.Report.Report

    structure PreElabDecGrammar: DEC_GRAMMAR
      where type TyVar.Variance = TyVar.Variance
      where type id = Ident.id
      where type longid = Ident.longid
      where type strid = StrId.strid
      where type longstrid = StrId.longstrid
      where type tycon = TyCon.tycon
      where type tyvar = TyVar.SyntaxTyVar
      where type lab = Lab.lab
      where type scon = SCon.scon
      where type longtycon = TyCon.longtycon


    structure Environments : ENVIRONMENTS
      where type Type         = StatObject.Type
      where type TyVar        = StatObject.TyVar
      where type TypeScheme   = StatObject.TypeScheme
      where type TypeFcn      = StatObject.TypeFcn
      where type realisation  = StatObject.realisation
      where type level        = StatObject.level
      where type id           = Ident.id
      where type longid       = Ident.longid
      where type Substitution = StatObject.Substitution
      where type ty           = PreElabDecGrammar.ty
      where type tycon    = TyCon.tycon
      where type longtycon    = TyCon.longtycon
      where type longstrid    = StrId.longstrid
      where type ExplicitTyVar  = TyVar.SyntaxTyVar
      where type strid       = StrId.strid
      where type valbind = PreElabDecGrammar.valbind
      where type pat = PreElabDecGrammar.pat
     
      where type ('a,'b) FinMap.map = ('a,'b) Tools.FinMap.map
      where type TyName       = TyName.TyName
      where type TyName.Set.Set = StatObject.TyName.Set.Set


    structure RefinedEnvironments : REFINED_ENVIRONMENTS
      where type id = Ident.id
      where type longid =  Ident.longid
      where type SortVar = RefObject.SortVar
      where type Sort = RefObject.Sort
      where type Type = StatObject.Type
      where type TyName = TyName.TyName
      where type SortName = SortName.SortName
      where type TypeFcn = StatObject.TypeFcn
      where type SortFcn = RefObject.SortFcn
      where type SortScheme = RefObject.SortScheme
      where type tycon = TyCon.tycon
      where type longtycon =  TyCon.longtycon
      where type ExplicitTyVar  = TyVar.SyntaxTyVar
      where type ExplicitTyVarEnv = Environments.ExplicitTyVarEnv
      where type strid       = StrId.strid
      where type longstrid    = StrId.longstrid
      where type Report = Tools.Report.Report
      (* where type Context = Environments.Context - Refined Contexts are different.*)

    structure ModuleStatObject : MODULE_STATOBJECT
      where type Env = Environments.Env
      where type rEnv = RefinedEnvironments.Env
      where type rT = RefinedEnvironments.TyNameEnv
      where type realisation = StatObject.realisation
      where type strid = StrId.strid
      where type longstrid = StrId.longstrid
      where type longtycon = TyCon.longtycon
      where type Type = StatObject.Type
      where type TypeScheme = StatObject.TypeScheme
      where type TypeFcn = StatObject.TypeFcn
      where type TyVar = StatObject.TyVar
      where type id = Ident.id
      where type TyName = TyName.TyName
      where type TyName.Set.Set = StatObject.TyName.Set.Set

    structure ModuleEnvironments : MODULE_ENVIRONMENTS
      where type realisation = StatObject.realisation
      where type longstrid = StrId.longstrid
      where type tycon = TyCon.tycon
      where type longtycon = TyCon.longtycon
      where type Context = Environments.Context
      where type FunSig = ModuleStatObject.FunSig
      where type TyStr = Environments.TyStr
      where type TyVar = StatObject.TyVar
      where type id = Ident.id
      where type longid = Ident.longid
      where type strid = StrId.strid
      where type sigid = SigId.sigid
      where type funid = FunId.funid
      where type Env = Environments.Env
      where type Sig = ModuleStatObject.Sig
      where type TyName = TyName.TyName


(*      where type rEnv.tycon = TyCon.tycon   (* Now handled in MODULE_ENVIRONMENTS *)
      where type rEnv.strid = StrId.strid     (* - but perhaps more work than it was worth. *)
      where type rEnv.id = Ident.id
      where type rEnv.TyName = StatObject.TyName
      where type rEnv.longtycon = TyCon.longtycon *)

      where type rEnv = ModuleStatObject.rEnv
      where type rTyNameEnv = ModuleStatObject.rT
      where type rEnv.ExplicitTyVar = StatObject.ExplicitTyVar

      where type rEnv.SortName = SortName.SortName
      where type rEnv.Variance = SortName.Variance (* Try this...  *)
      where type rEnv.SortFcn = RefObject.SortFcn
      where type rEnv.Sort = RefObject.Sort
      where type rEnv.SortVar = RefObject.SortVar
      where type rEnv.SortScheme = RefObject.SortScheme
      where type rContext = RefinedEnvironments.Context

      (* sharing ModuleEnvironments.rEnv = RefinedEnvironments    (* Argh - expand this?? *) *)


    structure OpacityEnv : OPACITY_ENV
      where type funid = FunId.funid
      where type TyName.TyName = TyName.TyName

    structure AllInfo : ALL_INFO
      (* where type TypeInfo.strid = StrId.strid *)

      where type SourceInfo.pos = LexBasics.pos
      where type SourceInfo.Report = Tools.Report.Report
      where type OverloadingInfo.RecType = StatObject.RecType
      where type ParseInfo.ParseInfo = PreElabDecGrammar.info (* See ALL_INFO *)
      where type ParseInfo.SourceInfo.Report = Tools.Report.Report
      where type ParseInfo.DFInfo.InfixBasis = InfixBasis.Basis
      where type ParseInfo.SourceInfo.pos = LexBasics.pos

      where type ElabInfo.OverloadingInfo.RecType = StatObject.RecType
      where type ElabInfo.ParseInfo.DFInfo.InfixBasis = InfixBasis.Basis

      where type ElabInfo.TypeInfo.Type = StatObject.Type
      where type ElabInfo.TypeInfo.TyEnv = Environments.TyEnv
      where type ElabInfo.TypeInfo.ExplicitTyVarEnv = Environments.ExplicitTyVarEnv
      where type ElabInfo.TypeInfo.longid = Ident.longid (* yes? *)
      where type ElabInfo.TypeInfo.realisation = StatObject.realisation
      where type ElabInfo.TypeInfo.Env = Environments.Env
      where type ElabInfo.TypeInfo.strid = Ident.strid (* yes? *)
      where type ElabInfo.TypeInfo.id = Ident.id
      where type ElabInfo.TypeInfo.TyName.TyName = TyName.TyName
      where type ElabInfo.TypeInfo.TyName.Set.Set = StatObject.TyName.Set.Set
      where type ElabInfo.TypeInfo.Basis = ModuleEnvironments.Basis
      where type ElabInfo.TypeInfo.tycon = TyCon.tycon
      where type ElabInfo.TypeInfo.TyVar = StatObject.TyVar
 
      where type ElabInfo.ErrorInfo.TyName = TyName.TyName
      where type ElabInfo.ErrorInfo.Type = StatObject.Type
      where type ElabInfo.ErrorInfo.TypeScheme = StatObject.TypeScheme
      where type ElabInfo.ErrorInfo.id = Ident.id
      where type ElabInfo.ErrorInfo.lab = StatObject.lab
      where type ElabInfo.ErrorInfo.longstrid = StrId.longstrid
      where type ElabInfo.ErrorInfo.longid = Ident.longid (* yes? *)
      where type ElabInfo.ErrorInfo.strid = Ident.strid (* yes? *)
      where type ElabInfo.ErrorInfo.longtycon = TyCon.longtycon
      where type ElabInfo.ErrorInfo.TypeFcn = StatObject.TypeFcn
      where type ElabInfo.ErrorInfo.sigid = SigId.sigid
      where type ElabInfo.ErrorInfo.RefineErrorInfo.Report = Tools.Report.Report
      where type ElabInfo.ErrorInfo.funid = FunId.funid
      where type ElabInfo.ErrorInfo.SigMatchError = ModuleStatObject.SigMatchError

      where type RefineErrorInfo.Sort = RefObject.Sort
      where type RefineErrorInfo.Type = StatObject.Type
      where type RefineErrorInfo.SortScheme = RefObject.SortScheme
      where type RefineErrorInfo.longid = Ident.longid
      where type RefineErrorInfo.longsortcon = TyCon.longtycon

(*  Argh.  Fully expanding these would be very tedious.  Instead "where type"s have only been added as needed. 
      sharing AllInfo.RefInfo.REnv = RefinedEnvironments
      sharing AllInfo.RefInfo.RefObject = RefObject
      sharing AllInfo.RefInfo.ElabInfo = AllInfo.ElabInfo
*)

      where type RefInfo.REnv.VarEnv = RefinedEnvironments.VarEnv
      where type RefInfo.REnv.TyNameEnv = RefinedEnvironments.TyNameEnv
      where type RefInfo.REnv.Env = RefinedEnvironments.Env
      where type RefInfo.REnv.Sort = RefObject.Sort 

      where type RefInfo.ElabInfo.TypeInfo.TyName.TyName = TyName.TyName
      where type RefInfo.ElabInfo.TypeInfo.longid = Ident.longid
      where type RefInfo.ElabInfo.TypeInfo.Type    = StatObject.Type
      where type RefInfo.ElabInfo.TypeInfo.TyVar   = StatObject.TyVar
      where type RefInfo.ElabInfo.TypeInfo.ExplicitTyVarEnv = RefinedEnvironments.ExplicitTyVarEnv
                     

    structure Comp : COMP  
      where type Error = AllInfo.ElabInfo.ErrorInfo.RefineErrorInfo.Error 
                         (* Was: AllInfo.SourceInfo.SourceInfo * AllInfo.RefineErrorInfo.ErrorInfo *)
      where type 'a Redo = 'a AllInfo.RefInfo.Comp.Redo
      where type 'a Memo = 'a AllInfo.RefInfo.Comp.Memo

  end;




functor Basics(structure Tools: TOOLS): BASICS =
  struct
    structure Tools = Tools

    structure StrId = StrId(structure Timestamp = Tools.Timestamp
			    structure Crash = Tools.Crash
			   )

    structure Ident = Ident(structure StrId = StrId
			    structure Crash = Tools.Crash
			   )

    structure InfixBasis = InfixBasis
      (structure Ident = Ident
       structure FinMap = Tools.FinMap
       structure Report = Tools.Report
       structure PP = Tools.PrettyPrint)

    structure SigId = SigId()
          and FunId = FunId()
          and TyVar = TyVar(structure Crash = Tools.Crash)
(*          and SortVar = SortVar(structure Crash = Tools.Crash) *)
	  and Lab = Lab()
    	  and SCon = SCon()
    	  and TyCon = TyCon(structure StrId = StrId
			    structure Crash = Tools.Crash
			   )

(*     structure SortCon = SortCon(structure TyCon = TyCon
		  		 structure StrId = StrId
				 structure Crash = Tools.Crash
			        )
*)

    structure Name = Name (structure Crash = Tools.Crash)

    structure TyName = TyName(structure TyCon = TyCon
			      structure IntFinMap = Tools.IntFinMap
			      structure Crash = Tools.Crash
			      structure Name = Name
			      structure Flags = Tools.Flags
			      structure PrettyPrint = Tools.PrettyPrint
			      structure Report = Tools.Report)

    structure SortName = SortName(structure TyVar = TyVar  
				  structure TyCon = TyCon
			          structure TyName = TyName
				  structure Name = Name
			          structure IntFinMap = Tools.IntFinMap
				  structure Flags = Tools.Flags
                                  structure ListHacks = Tools.ListHacks
				  structure Crash = Tools.Crash
                                  structure PP = Tools.PrettyPrint
			          structure Report = Tools.Report)

      structure StatObject : STATOBJECT = 
	StatObject(structure SortedFinMap  = Tools.SortedFinMap
		   structure Name = Name
		   structure IntFinMap = Tools.IntFinMap
		   structure Ident = Ident
		   structure Lab = Lab
		   structure SCon = SCon
		   structure TyName = TyName
		   structure TyCon = TyCon
		   structure ExplicitTyVar = TyVar
		   structure Flags = Tools.Flags
		   structure Report = Tools.Report
		   structure FinMap = Tools.FinMap
		   structure FinMapEq = Tools.FinMapEq
		   structure PP = Tools.PrettyPrint
		   structure Crash = Tools.Crash
		  )
   structure RefObject = RefObject(structure StatObject = StatObject
				   structure ExplicitTyVar = TyVar
				   structure SortName = SortName
				   structure Lab = Lab
				   structure Name = Name
				   structure Crash = Tools.Crash
				   structure FinMap = Tools.FinMap
				   structure SortedFinMap = Tools.SortedFinMap
				   structure Flags = Tools.Flags
				   structure SCon = SCon
				   structure ListHacks = Tools.ListHacks
				   structure Report = Tools.Report
				   structure PP = Tools.PrettyPrint
				  )		 

   (* LexBasics is needed by SourceInfo, as well as all the parsing
      stuff. *)

    structure LexBasics = LexBasics(structure BasicIO = Tools.BasicIO
				    structure Report = Tools.Report
				    structure PP = Tools.PrettyPrint
				    structure Flags = Tools.Flags
				    structure Crash = Tools.Crash
				   )

    structure DFInfo = DFInfo
      (structure PrettyPrint = Tools.PrettyPrint
       structure InfixBasis = InfixBasis)
      
    structure SourceInfo = SourceInfo
      (structure LexBasics = LexBasics
       structure PrettyPrint = Tools.PrettyPrint
       structure Crash = Tools.Crash)

    structure ParseInfo = ParseInfo
      (structure SourceInfo = SourceInfo
       structure DFInfo = DFInfo
       structure PrettyPrint = Tools.PrettyPrint
       structure Crash = Tools.Crash)

    structure PreElabDecGrammar = DecGrammar
      (structure GrammarInfo =
	 struct
	   type GrammarInfo = ParseInfo.ParseInfo
 	   val bogus_info = 
	     ParseInfo.from_SourceInfo(SourceInfo.from_positions LexBasics.DUMMY LexBasics.DUMMY)
	 end
       structure Lab = Lab
       structure SCon = SCon
       structure TyVar = TyVar
       structure TyCon = TyCon
       structure StrId = StrId
       structure Ident = Ident
       structure PrettyPrint = Tools.PrettyPrint)

    structure Environments : ENVIRONMENTS = Environments
      (structure DecGrammar = PreElabDecGrammar
       structure Ident = Ident
       structure TyCon = TyCon
       structure StrId = StrId
       structure StatObject = StatObject
       structure TyName = TyName
       structure PP = Tools.PrettyPrint
       structure SortedFinMap = Tools.SortedFinMap
       structure FinMap = Tools.FinMap
       structure Timestamp = Tools.Timestamp
       structure Report = Tools.Report
       structure Flags = Tools.Flags
       structure Crash = Tools.Crash) 

      structure RefinedEnvironments = RefinedEnvironments
	            (structure StrId = StrId
                     structure Ident = Ident
		     structure TyCon = TyCon
		     structure TyName = TyName
		     structure SortName = SortName
		     structure StatObject = StatObject
		     structure Environments = Environments
		     structure RefObject = RefObject
		     structure PP = Tools.PrettyPrint
		     structure SortedFinMap = Tools.SortedFinMap
		     (* structure FinMap = Tools.FinMap *)
		     structure FinMapEq = Tools.FinMapEq
		     structure Flags = Tools.Flags
		     structure Timestamp = Tools.Timestamp
		     structure ListHacks = Tools.ListHacks
		     structure Report = Tools.Report
		     structure Crash = Tools.Crash
		    )


    structure ModuleStatObject =
      ModuleStatObject(structure StrId        = StrId
		       structure SigId        = SigId
		       structure FunId        = FunId
		       structure TyCon        = TyCon
		       structure TyName       = TyName
		       structure Name         = Name
		       structure StatObject   = StatObject
		       structure Environments = Environments
                       structure RefObject         = RefObject
                       structure RefinedEnvironments = RefinedEnvironments
		       structure FinMap       = Tools.FinMap
		       structure PP           = Tools.PrettyPrint
		       structure Report       = Tools.Report
		       structure Flags        = Tools.Flags
		       structure Crash        = Tools.Crash
		      )


    structure ModuleEnvironments =
      ModuleEnvironments(structure StrId             = StrId
			 structure SigId             = SigId
			 structure FunId             = FunId
			 structure TyCon             = TyCon
			 structure Ident             = Ident
			 structure FinMap            = Tools.FinMap
			 structure FinMapEq          = Tools.FinMapEq
			 structure StatObject        = StatObject
			 structure Environments      = Environments
                         structure RefObject         = RefObject
                         structure RefinedEnvironments = RefinedEnvironments
			 structure ModuleStatObject  = ModuleStatObject
			 structure PP                = Tools.PrettyPrint
			 structure Report	     = Tools.Report
			 structure Flags             = Tools.Flags
			 structure Crash             = Tools.Crash
			)

    structure OpacityEnv = OpacityEnv(structure FunId = FunId
                                      structure Crash = Tools.Crash                                     
                                      structure PP = Tools.PrettyPrint
                                      structure Report = Tools.Report
                                      structure Environments = Environments)
    structure RefineErrorInfo =
	  RefineErrorInfo(structure RefObject = RefObject
                          structure StatObject = StatObject
			  structure Ident = Ident
                          structure TyCon = TyCon
                          structure Report = Tools.Report
                          structure SourceInfo = SourceInfo
                         )

    structure Comp = Comp(structure S = struct type Error = RefineErrorInfo.Error end) (* Computations *)

    structure AllInfo =
      struct
	structure SourceInfo = SourceInfo
	structure DFInfo = DFInfo
	structure ParseInfo = ParseInfo
	structure RefineErrorInfo = RefineErrorInfo
	structure ErrorInfo = ErrorInfo
	  (structure StatObject = StatObject
	   structure ModuleStatObject = ModuleStatObject
           structure RefObject = RefObject
           structure RefineErrorInfo = RefineErrorInfo
	   structure Ident = Ident
	   structure Lab   = Lab
	   structure TyCon = TyCon
	   structure TyName = TyName
	   structure SigId = SigId
	   structure StrId = StrId
	   structure FunId = FunId
	   structure Report = Tools.Report
	   structure PrettyPrint = Tools.PrettyPrint)
	structure TypeInfo = TypeInfo
	  (structure Crash = Tools.Crash
	   structure Ident = Ident
	   structure ModuleEnvironments = ModuleEnvironments
	   structure StrId = StrId
	   structure TyCon = TyCon
	   structure StatObject=StatObject
	   structure Environments=Environments
	   structure PP = Tools.PrettyPrint
	   structure OpacityEnv = OpacityEnv)
	structure OverloadingInfo = OverloadingInfo
	  (structure StatObject = StatObject
	   structure PrettyPrint = Tools.PrettyPrint)
	structure ElabInfo = ElabInfo
	  (structure ParseInfo = ParseInfo
	   structure ErrorInfo = ErrorInfo
	   structure TypeInfo = TypeInfo
	   structure OverloadingInfo = OverloadingInfo
	   structure PrettyPrint = Tools.PrettyPrint
	   structure Crash = Tools.Crash)
        structure RefInfo = RefInfo
          (structure ElabInfo = ElabInfo
           structure REnv = RefinedEnvironments
	   structure RefObject = RefObject
           structure PP = Tools.PrettyPrint
           structure FinMapEq = Tools.FinMapEq
           structure Comp = Comp)
       end   (* of struct for AllInfo *)

  end;



signature TOPDEC_PARSING =
  sig
    structure Basics: BASICS

    (* structure PreElabDecGrammar: DEC_GRAMMAR *)
      (* sharing PreElabDecGrammar = Basics.PreElabDecGrammar *)

    structure PreElabTopdecGrammar: TOPDEC_GRAMMAR
      where type dec = Basics.PreElabDecGrammar.dec
      where type sigid = Basics.SigId.sigid
      where type funid = Basics.FunId.funid
      where type ty = Basics.PreElabDecGrammar.ty

     where type tyvar = Basics.StatObject.ExplicitTyVar
     where type id = Basics.AllInfo.ElabInfo.ErrorInfo.id
(*     where type funid = ElabInfo.ErrorInfo.funid *)
     where type longtycon = Basics.AllInfo.ElabInfo.ErrorInfo.longtycon
     where type strid = Basics.StrId.strid
     where type longstrid = Basics.StrId.longstrid 
     where type info = Basics.AllInfo.ParseInfo.ParseInfo
     where type tycon = Basics.SortName.sortcon
     where type DecGrammar.TyVar.Variance = Basics.SortName.Variance
     where type longid = Basics.Ident.longid
     where type DecGrammar.datbind = Basics.PreElabDecGrammar.datbind


    (* structure InfixBasis: INFIX_BASIS *)
    (*   sharing InfixBasis = Basics.InfixBasis *)

    structure Parse: PARSE
      where type topdec = PreElabTopdecGrammar.topdec
      where type InfixBasis = Basics.InfixBasis.Basis
  end;



functor TopdecParsing(structure Basics: BASICS) (* : TOPDEC_PARSING *) =
  struct
    structure Basics = Basics
    structure Tools = Basics.Tools
    structure AllInfo = Basics.AllInfo

    structure PreElabTopdecGrammar : TOPDEC_GRAMMAR
                                           (* where type DecGrammar.datbind = Basics.PreElabDecGrammar.datbind *)
       = TopdecGrammar
      (structure DecGrammar = Basics.PreElabDecGrammar
       structure SigId = Basics.SigId
       structure FunId = Basics.FunId
       structure PrettyPrint = Tools.PrettyPrint)

    structure Parse = Parse
      (structure TopdecGrammar = PreElabTopdecGrammar
       structure LexBasics = Basics.LexBasics
       structure ParseInfo = AllInfo.ParseInfo
       structure InfixBasis = Basics.InfixBasis
       structure Report = Tools.Report
       structure PrettyPrint = Tools.PrettyPrint
       structure FinMap = Tools.FinMap
       structure BasicIO = Tools.BasicIO
       structure Flags = Tools.Flags
       structure Crash = Tools.Crash)
  end;



signature ELABORATION =
  sig
    structure Basics : BASICS
	    
    structure ElabTopdec : ELABTOPDEC

    structure RefDec : REFDEC

(*     structure PostElabDecGrammar : DEC_GRAMMAR *)

    structure PostElabTopdecGrammar : TOPDEC_GRAMMAR
(*      where type dec = PostElabDecGrammar.dec *)
      where type strid = Basics.StrId.strid
      where type sigid = Basics.SigId.sigid
      where type funid = Basics.FunId.funid
      where type topdec = ElabTopdec.PostElabTopdec

      where type DecGrammar.lab = Basics.Lab.lab
      where type DecGrammar.scon = Basics.SCon.scon
      where type tycon = Basics.TyCon.tycon
      where type longtycon = Basics.TyCon.longtycon

      where type tyvar = Basics.TyVar.SyntaxTyVar
      where type DecGrammar.TyVar.Variance = Basics.TyVar.Variance
      where type id = Basics.Ident.id
      where type longid = Basics.Ident.longid
      where type info = Basics.AllInfo.ElabInfo.ElabInfo
      (* where type ty = Basics.PreElabDecGrammar.ty *)

    structure RefDecGrammar : DEC_GRAMMAR 
      where type lab = Basics.Lab.lab
      where type scon = Basics.SCon.scon
      where type tycon = Basics.TyCon.tycon
      where type longtycon = Basics.TyCon.longtycon
      where type tyvar = Basics.TyVar.SyntaxTyVar
      where type TyVar.Variance = Basics.TyVar.Variance
      where type id = Basics.Ident.id
      where type longid = Basics.Ident.longid
      where type info = Basics.AllInfo.RefInfo.RefInfo
      (* where type ty = Basics.PreElabDecGrammar.ty *)


(*
	  sharing type RefDecGrammar.info
		   = Basics.AllInfo.RefInfo.RefInfo *)
(*  structure RefTopdecGrammar : TOPDEC_GRAMMAR
	  sharing type RefTopdecGrammar.dec = RefDecGrammar.dec
	  sharing type RefTopdecGrammar.info
	    	   = Basics.AllInfo.RefInfo.RefInfo  *)
  end;




functor Elaboration(structure TopdecParsing : TOPDEC_PARSING): ELABORATION =
  struct
    structure Basics     = TopdecParsing.Basics

    local
      structure InfixBasis = Basics.InfixBasis
      structure PreElabDecGrammar = Basics.PreElabDecGrammar

      structure Tools      = Basics.Tools
      structure AllInfo    = Basics.AllInfo
      structure ElabInfo   = AllInfo.ElabInfo
    in

      structure PostElabDecGrammar =
	DecGrammar(structure GrammarInfo =
		     struct
		       type GrammarInfo = ElabInfo.ElabInfo
		       val bogus_info = ElabInfo.from_ParseInfo PreElabDecGrammar.bogus_info
		     end
		   structure Lab         = Basics.Lab
		   structure SCon        = Basics.SCon
		   structure TyVar       = Basics.TyVar
		   structure TyCon       = Basics.TyCon
		   structure StrId       = Basics.StrId
		   structure Ident       = Basics.Ident
		   structure PrettyPrint = Tools.PrettyPrint
		  )

      structure PostElabTopdecGrammar =
	TopdecGrammar(structure DecGrammar = PostElabDecGrammar
		      structure SigId = Basics.SigId
		      structure FunId = Basics.FunId
		      structure PrettyPrint = Tools.PrettyPrint)

      structure RefDecGrammar =
	DecGrammar(structure GrammarInfo =
		     struct
		       type GrammarInfo = AllInfo.RefInfo.RefInfo
                       val bogus_info = 
                         AllInfo.RefInfo.from_ElabInfo PostElabDecGrammar.bogus_info
		     end
		   structure Lab         = Basics.Lab
		   structure SCon        = Basics.SCon
		   structure TyVar       = Basics.TyVar
		   structure TyCon       = Basics.TyCon
		   structure StrId       = Basics.StrId
		   structure Ident       = Basics.Ident
		   structure PrettyPrint = Tools.PrettyPrint
		  )

(*    structure RefTopdecGrammar =
	TopdecGrammar(structure DecGrammar =  RefDecGrammar
         	      structure SigId = Basics.SigId
		      structure FunId = Basics.FunId
		      structure PrettyPrint = Tools.PrettyPrint) *)

      structure MapDecEtoR = MapDecInfo(structure IG = PostElabDecGrammar
					structure OG = RefDecGrammar)

      structure MapDecRtoE = MapDecInfo(structure IG = RefDecGrammar
					structure OG = PostElabDecGrammar);

     
      structure RefDec (* : REFDEC where type Comp.Error = AllInfo.ErrorInfo.RefineErrorInfo.Error *)
               =
                     RefDec( structure IG = PostElabDecGrammar
                             structure RG = RefDecGrammar
                             structure RefInfo = AllInfo.RefInfo
                             structure MapDecEtoR = MapDecEtoR
                             structure MapDecRtoE = MapDecRtoE
                             structure RefinedEnvironments = Basics.RefinedEnvironments
                             structure StatObject = Basics.StatObject
                             structure RefObject = Basics.RefObject
                             structure Lab = Basics.Lab
                             structure Ident = Basics.Ident
                             structure TyName = Basics.TyName
                             structure TyVar = Basics.TyVar
                             structure SortName = Basics.SortName
                             structure TyCon = Basics.TyCon
                             structure RefineErrorInfo = AllInfo.RefineErrorInfo
                             structure FinMap = Tools.FinMap
                             structure FinMapEq = Tools.FinMapEq
                             structure SortedFinMap = Tools.SortedFinMap
                             structure Report = Tools.Report
                             structure PP = Tools.PrettyPrint
                             structure Flags = Tools.Flags
                             structure Crash = Tools.Crash
                             structure Comp = Basics.Comp
                            )

      structure ElabDec (* : ELABDEC where type PreElabDatBind = TopdecParsing.PreElabTopdecGrammar.DecGrammar.datbind *)  =
		     ElabDec (structure TyCon = Basics.TyCon							
			      structure ElabInfo = AllInfo.ElabInfo
			      structure IG = TopdecParsing.Basics.PreElabDecGrammar
			      structure OG = PostElabDecGrammar
			      structure Environments = Basics.Environments
			      structure Ident = Basics.Ident
			      structure Lab = Basics.Lab
			      structure StatObject = Basics.StatObject
			      structure FinMap = Tools.FinMap
			      structure Report = Tools.Report
			      structure PP = Tools.PrettyPrint
			      structure Flags = Tools.Flags
			      structure Crash = Tools.Crash)
      
      structure ElabTopdec =
	ElabTopdec(structure PrettyPrint = Tools.PrettyPrint
		   structure IG = TopdecParsing.PreElabTopdecGrammar
		   structure OG = PostElabTopdecGrammar
		   structure Environments = Basics.Environments
		   structure ModuleEnvironments = Basics.ModuleEnvironments
                   structure RefObject = Basics.RefObject
		   structure StatObject = Basics.StatObject
                   structure SortName = Basics.SortName
		   structure ModuleStatObject = Basics.ModuleStatObject
		   structure ElabDec = ElabDec
                   structure RefDec = RefDec
		   structure StrId = Basics.StrId
		   structure SigId = Basics.SigId
		   structure ParseInfo = AllInfo.ParseInfo
		   structure ElabInfo = AllInfo.ElabInfo
		   structure BasicIO = Tools.BasicIO
		   structure Report = Tools.Report
		   structure Ident = Basics.Ident
		   structure PP = Tools.PrettyPrint
		   structure FinMap = Tools.FinMap
		   structure Flags = Tools.Flags
		   structure Crash = Tools.Crash)

    end
  end;

