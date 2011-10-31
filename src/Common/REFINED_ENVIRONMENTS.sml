(* Environments for refinement phase.  
   See Environments, Module_Environments.    - Rowan   *)

(*$REFINED_ENVIRONMENTS*)
signature REFINED_ENVIRONMENTS =
  sig
    type SortName 
    eqtype tycon
    type TyName
    eqtype Variance 
    structure SortName : SORTNAME 
       where type SortName = SortName
       where type TyName = TyName
       where type Variance = Variance
       where type sortcon = tycon

    (* Basis is currently really Context *)
    type Basis and Context and Env and TyNameEnv and SortVar and Type
    type Sort and SortScheme and SortFcn
    type ExplicitTyVar
    type realisation

    val emptyB    : Basis
    and initialB  : Basis
    and initialC  : Context
    and B_plus_B  : Basis * Basis   -> Basis

    val C_of_B :   Basis -> Context
    and T_of_B :   Basis -> TyNameEnv
    and E_of_B :   Basis -> Env
    and B_plus_C : Basis * Context -> Basis
    and E_in_B :   Env -> Basis
    and C_in_B :   Context -> Basis

    (********
    Unqualified identifiers
    ********)

    eqtype id

    (********
    PrettyPrinting functions
    ********)

    (** TyGoals **)
    type ToRealise   (* for opaque sortnames in TyGoals *)
    type TyGoals (* Type goals for RML *)

    structure TG :
      sig
	val empty                : TyGoals  (* RML goals *)
	val singleton0           : id * SortScheme list -> TyGoals
	val singleton            : id * (ToRealise * SortScheme list) -> TyGoals
        val add0                 : id * SortScheme list * TyGoals -> TyGoals
        val add                  : id * (ToRealise * SortScheme list) * TyGoals -> TyGoals
	val plus                 : TyGoals * TyGoals -> TyGoals
        val map                  : (ToRealise * SortScheme list -> ToRealise * SortScheme list) 
                                    -> TyGoals -> TyGoals
        (* use Lookup_tygoals which realises opaque tycons in the goals *)
        val layout               : TyGoals -> StringTree.t
      end


    (********
    Environments
    ********)

    val E_plus_E : Env * Env -> Env
    and emptyE   : Env
    and TG_in_E : TyGoals -> Env
    and TG_of_E : Env -> TyGoals
    and E_erase_TG : Env -> Env

    val bogus_Env: Env

    (********
    Refinement Lattices
    ********)

    type RefLattice 

    val singleRL : SortName -> RefLattice
    val conjoinRL' : RefLattice -> (SortName * SortName) -> SortName option
    (* val RL_plus_RL : RefLattice * RefLattice -> RefLattice  (* No completion! *) *)
    val addRL      : RefLattice * (SortName * SortName) * SortName -> RefLattice

    (********
    SortCons Environments
    ********)
    
    type SortCons

    val emptyRC    : SortCons

    and RC_plus_RC  : SortCons * SortCons -> SortCons

    and singleRC   : id * SortFcn option -> SortCons
    and lookupRC   : SortCons * id -> SortFcn list option option
    val RCfold : ((SortFcn list option * 'b) -> 'b) -> 'b -> SortCons -> 'b
    and RCFold : (((id * SortFcn list option) * 'c) -> 'c) -> 'c -> SortCons -> 'c
    and RCdom  : SortCons -> id list

    (********
    SortName Environments
    ********)
    
    type SortNameEnv

    val emptyR    : SortNameEnv

    and R_plus_R  : SortNameEnv * SortNameEnv -> SortNameEnv

    and singleR   : SortName * SortCons -> SortNameEnv
    and lookupR   : SortNameEnv * SortName -> SortCons option
    val Rfold : ((SortCons * 'b) -> 'b) -> 'b -> SortNameEnv -> 'b
    and RFold : (((SortName * SortCons) * 'c) -> 'c) -> 'c -> SortNameEnv -> 'c
    and Rdom  : SortNameEnv -> SortName list

    (********
    Constructor environments
    ********)

    type ConEnv
    val emptyCE    : ConEnv
    and domCE      : ConEnv -> id list
    and isemptyCE  : ConEnv -> bool
    and singleCE   : id * SortScheme -> ConEnv
    and equalCE    : TyNameEnv -> ConEnv * ConEnv -> bool (* up to bound vars *)
    and CEmap      : (SortScheme -> SortScheme) -> ConEnv -> ConEnv
    and CEfold     : ((SortScheme * 'a) -> 'a) -> 'a -> ConEnv -> 'a
    and CE_plus_CE : ConEnv * ConEnv -> ConEnv
    and lookupCE   : ConEnv * id -> SortScheme option


    (********
    Type functions
    ********)

    type TypeFcn
(*    val equal_TypeFcn : TypeFcn * TypeFcn -> bool  *)

    (********
    Type structures
    ********)

    type TyStr	

    (* bool below is "covariance" *)    
    val mkTyStr      : ConEnv * SortNameEnv * SortName * bool * RefLattice -> TyStr
    val unTyStr      : TyStr -> ConEnv * SortNameEnv * SortName * bool * RefLattice
    val CE_of_TyStr  : TyStr -> ConEnv
    val R_of_TyStr   : TyStr -> SortNameEnv
    val RL_of_TyStr  : TyStr -> RefLattice
    val mlSN_of_TyStr : TyStr -> SortName
    val covariant_of_TyStr : TyStr -> bool 
(*    val bogus_TyStr: TyStr *)

    (********
    Structure identifier
    ********)

    eqtype strid and longstrid

    val lookupE_strid : Env * strid -> Env option

    (********
    Structures
    ********)

(*  type Str
    val mkStr  : StrName * Env -> Str
    and unStr  : Str -> StrName * Env

    val bogus_Str: Str
*)
    (********
    The type of items associated with identifiers in a VarEnv
    ********)

    datatype VarEnvRange =
       LONGVAR   of SortScheme
     | LONGCON   of SortScheme
     | LONGEXCON of Sort

    (********
    Variable environments
    ********)

    type VarEnv

    val clearManySortVars : unit -> unit
    val emptyVE  : VarEnv
    and VE_in_E  : VarEnv -> Env
    and VE_of_E  : Env -> VarEnv
    and lookupVE : VarEnv  * id -> VarEnvRange option
    and VEfold   : ((VarEnvRange * 'a) -> 'a) -> 'a -> VarEnv -> 'a
    and VEFold   : (((id * VarEnvRange) * 'b) -> 'b) -> 'b -> VarEnv -> 'b
    and VE_plus_VE  : VarEnv * VarEnv -> VarEnv
    and VE_conjoin_vars  : TyNameEnv -> VarEnv * VarEnv -> VarEnv
    and VEdom       : VarEnv -> id EqSet.Set
    and singleVE : id * VarEnvRange -> VarEnv
    and singleVarVE : id * SortScheme -> VarEnv
    and closVE  : VarEnv * SortVar list -> VarEnv
    and subVE   : TyNameEnv -> VarEnv * VarEnv -> bool
    and eqVE    : TyNameEnv -> VarEnv * VarEnv -> bool
    and sntx_eqVE : VarEnv * VarEnv -> bool
    and trimVE : VarEnv -> VarEnv

    (********
    This function maps a CE to a VE
    ********)

    val CE_to_VE : ConEnv -> VarEnv

    (********
    Exception constructors
    ********)

    type excon = id

    (********
    Exception constructor environments
    ********)

    type ExConEnv

    val emptyEE  : ExConEnv
    and EEfold   : ((Sort * 'a) -> 'a) -> 'a -> ExConEnv -> 'a
    and EEFold   : (((excon * Sort) * 'a) -> 'a) -> 'a -> ExConEnv -> 'a
    and singleEE : excon * Sort -> ExConEnv
    and lookupEE : ExConEnv * excon -> Sort option 
    and VE_of_EE : ExConEnv -> VarEnv
    and EEdom      : ExConEnv -> excon EqSet.Set
    and EE_plus_EE : ExConEnv * ExConEnv -> ExConEnv
    and VE_and_EE_in_E : (VarEnv * ExConEnv) -> Env

    (********
    Type constructors
    ********)


    (********
    Type environments
    ********)

    type TyEnv

    val emptyTE    : TyEnv
    and TE_in_E    : TyEnv -> Env
    and TE_plus_TE : TyEnv * TyEnv -> TyEnv
    and singleTE   : tycon * SortFcn -> TyEnv
    and lookupTE   : TyEnv * tycon -> SortFcn option
    and VE_and_TE_in_E : VarEnv * TyEnv -> Env

    val TEfold : ((SortFcn * 'b) -> 'b) -> 'b -> TyEnv -> 'b
    and TEFold : (((tycon * SortFcn) * 'c) -> 'c) -> 'c -> TyEnv -> 'c
    and TEdom  : TyEnv -> tycon list

    val lookupE_tycon : Env * tycon -> SortFcn option

    (********
    Sort constructors
    ********)

    type sortcon = tycon  (* same type in RML/Kit3 - rowan 12jul01 *)

    (********
    Sort environments
    ********)

    type SortEnv

    val emptyRE    : SortEnv
    and RE_in_E    : SortEnv -> Env
    and RE_plus_RE : SortEnv * SortEnv -> SortEnv
    and singleRE   : sortcon * SortFcn -> SortEnv
    and lookupRE   : SortEnv * sortcon -> SortFcn option

    val REfold : ((SortFcn * 'b) -> 'b) -> 'b -> SortEnv -> 'b
    and REFold : (((sortcon * SortFcn) * 'c) -> 'c) -> 'c -> SortEnv -> 'c
    and REdom  : SortEnv -> sortcon list
    and convertREtoTE : SortEnv -> TyEnv
    and overrideDefaults : TyNameEnv -> SortEnv -> TyNameEnv

(*     val lookupE_sortcon : Env * sortcon -> SortFcn option *)

    (********
    Structure environments
    ********)

    type StrEnv
    val emptySE    : StrEnv
    val singleSE   : strid  * Env -> StrEnv
    val SE_plus_SE : StrEnv * StrEnv -> StrEnv
    val SE_in_E    : StrEnv -> Env
    val SEfold     : (Env * 'b  -> 'b) -> 'b -> StrEnv -> 'b
    val SEFold     : (((strid * Env) * 'c) -> 'c) -> 'c -> StrEnv -> 'c
    val SEdom      : StrEnv -> strid list
    val SEmap      : (Env -> Env) -> StrEnv -> StrEnv
    val mkEnv      : StrEnv * TyEnv * SortEnv * VarEnv * ExConEnv -> Env
    val unEnv      : Env -> StrEnv * TyEnv * SortEnv * VarEnv * ExConEnv
    val lookupSE   : StrEnv * strid -> Env option
    and SE_of_E  : Env -> StrEnv



    (********
    TypeName Environments
    ********)
    

    val emptyT    : TyNameEnv

    (* The TyStr's in second T should be strictly "richer" than the first *)
    and T_plus_T  : TyNameEnv * TyNameEnv -> TyNameEnv

    and singleT   : TyName * TyStr -> TyNameEnv
    and lookupT   : TyNameEnv * TyName -> TyStr option
    val Tfold : ((TyStr * 'b) -> 'b) -> 'b -> TyNameEnv -> 'b
    and TFold : (((TyName * TyStr) * 'c) -> 'c) -> 'c -> TyNameEnv -> 'c
    and Tdom  : TyNameEnv -> TyName list
    and Tmap  : (TyName * TyStr -> TyName * TyStr) -> TyNameEnv -> TyNameEnv
    and restrictT : TyNameEnv -> (TyName -> bool) -> TyNameEnv

    val conjSortNameT' : TyNameEnv -> SortName * SortName -> SortName option
    val conjSortNameT : TyNameEnv -> SortName * SortName -> SortName
    (* complete a lattice.  Returns the modified part of T.  
       VarEnv includes the full new sort of constructors. *)
    val completeT :  Context * (TyName * SortNameEnv) list * SortEnv
                             -> TyNameEnv * SortEnv * VarEnv
    val subSortNameT : TyNameEnv -> SortName * SortName -> bool

    (* Add/find a sortname in a non-datasort lattice that satisfies some lower and upper bounds. *)
    val extend_lattice : Context * TyName * sortcon * Variance list * SortName list * SortName list
                          -> TyNameEnv * SortName
    (* Add an incomparable element to an lattice and all intersections.  *)
    val RL_add_abstract : Context * TyName * sortcon * Variance list -> TyNameEnv * SortName

    (* Collapse two sortnames into one.  Collapses subsorts also.  *)    
    val RL_collapse : TyNameEnv * SortName * SortName -> realisation

    (********
    Contexts
    ********)

    type ExplicitTyVarEnv

    val mkC        : TyNameEnv * Env -> Context
    and emptyC     : Context
    and C_plus_VE  : Context * VarEnv    -> Context
    and C_plus_C   : Context * Context   -> Context
    and C_erase_TG    : Context -> Context
    and C_plus_T   : Context * TyNameEnv -> Context
    and C_plus_E   : Context * Env       -> Context
    val C_plus_U'  : Context * ExplicitTyVar list -> SortVar list * Context
    val U_of_C     : Context -> ExplicitTyVar list

    and E_of_C     : Context -> Env
    and T_of_C     : Context -> TyNameEnv
    and C_no_T :   Context -> Context

(*    and onC    : Substitution * Context  -> Context *)
    and E_in_C : Env -> Context
    and conjSortNameC : Context -> SortName * SortName -> SortName

    and emptyU     : ExplicitTyVarEnv
    and C_plus_U   : Context * ExplicitTyVarEnv -> Context
    val ExplicitTyVar_lookup : Context -> ExplicitTyVar -> Type


    (*******
    Patterns and domains of patterns
    *******)

(*
    type pat 
    val dom_pat : Context * pat -> id list
*)

    (********
    Qualified identifiers
    ********)

    type longid

    (********
    Qualified type and sort constructors
    ********)

    type longtycon
    type longsortcon = longtycon (* Same in RML/Kit3 - Rowan 12jul01 *)

    (********
    Functions to lookup in a context
    ********)

    val Lookup_longid    : Context * longid -> VarEnvRange option
    val Lookup_longtycon : Context * longtycon -> SortFcn option
    val Lookup_longsortcon : Context * longsortcon -> SortFcn option
    val Lookup_lstrid    : Basis * longstrid -> Env option
    val Lookup_tyname : Context * TyName -> TyStr option
    val Lookup_sortcon : Context * sortcon -> SortFcn option
    val Lookup_tygoal        : Context * id -> SortScheme list option

    val TNtoSN : Context -> TyName -> SortName
    val ty_to_srt : Context -> Type -> Sort

    (********
    Generate all Refinements of a type
    ********)

    val allRefinements : Context -> Type -> Sort list

    val hasMultRefments : Context -> Type -> bool

    (********
    Function which looks up a type constructor in the Env
    ********)

(*    val Lookup_tycon : Env * tycon -> TyStr option *)


    (********
    Type realisations  (fill this in later)
    ********)
(*
    type tyrea
    val tyrea_on_CE    : tyrea -> ConEnv   -> ConEnv
    and tyrea_on_TyStr : tyrea -> TyStr    -> TyStr
    and tyrea_on_TE    : tyrea -> TyEnv    -> TyEnv
    and tyrea_on_VE    : tyrea -> VarEnv   -> VarEnv
    and tyrea_on_EE    : tyrea -> ExConEnv -> ExConEnv
*)

    structure Rea :
      sig
        type conjSN = SortName * SortName -> SortName
        type trealisation 
        val on_SortName             : conjSN -> realisation -> SortName -> SortFcn
        val on_Sort                 : conjSN -> realisation -> Sort -> Sort
        val on_SortFcn              : conjSN -> realisation -> SortFcn -> SortFcn
        val on_SortScheme           : conjSN -> realisation -> SortScheme -> SortScheme
	val Id                      : realisation
	val is_Id                   : realisation -> bool
	val oo                      : conjSN -> realisation -> realisation -> realisation
	val singleton               : SortName * SortFcn -> realisation
        val singleSN                : SortName * SortName -> realisation
	val restrict                : SortName.Set.Set -> realisation -> realisation
	val restrict_from           : SortName.Set.Set -> realisation -> realisation
	val renaming         : trealisation -> SortName.Set.Set -> realisation
	val renaming'        : trealisation -> SortName.Set.Set -> SortName.Set.Set * realisation
	val dom                     : realisation -> SortName.Set.Set
        val plus                    : realisation -> realisation -> realisation
	val layout                  : realisation -> StringTree.t
        (* Vals Above are inherited from RefObject, below are defined in RefinedEnviroments. *)
        val on_Env : conjSN -> realisation -> Env -> Env
        val on_VarEnv : conjSN -> realisation -> VarEnv -> VarEnv
        val on_TyEnv : conjSN -> realisation -> TyEnv -> TyEnv
        val on_SortEnv : conjSN -> realisation -> SortEnv -> SortEnv
        val on_TyGoals : conjSN -> realisation -> TyGoals -> TyGoals
        val on_StrEnv : conjSN -> realisation -> StrEnv -> StrEnv
        val on_RC : conjSN -> realisation -> SortCons -> SortCons
        val on_SortNameEnv : conjSN -> realisation -> SortNameEnv -> SortNameEnv
        val on_TyStr : conjSN -> realisation -> TyStr -> TyStr
        val on_TyNameEnv : conjSN -> realisation -> TyNameEnv -> TyNameEnv
      end


    exception No_match  (* raised by mergeT if refinements don't match *)

    (* Merge a list of TyNames from a TyNameEnv into another TyName, 
       and modify an Env accordingly *)
    val mergeT : TyNameEnv * Env * TyName list * TyName -> TyNameEnv * Env 
    
    (* The following are needed for the top-level printout routines. *)
    type Report
    val reportRE: {sortEnv: SortEnv, T : TyNameEnv, bindings: bool} -> Report
    val reportEE: ExConEnv -> Report
    val reportT : TyNameEnv -> Report

    val layoutRL : RefLattice -> StringTree.t
    val layoutCE : ConEnv -> StringTree.t
    val layoutRC : SortCons -> StringTree.t
    val layoutR : SortNameEnv -> StringTree.t
    val layoutVE : VarEnv -> StringTree.t
    val layoutSE : StrEnv -> StringTree.t
    val layoutTE : TyEnv -> StringTree.t
    val layoutRE : SortEnv -> StringTree.t
    val layoutEE : ExConEnv -> StringTree.t
    val layoutEnv : Env -> StringTree.t
    val layoutTyStr : TyStr -> StringTree.t
    val layoutT : TyNameEnv -> StringTree.t
    val layoutC : Context -> StringTree.t
    val reportLayoutC : Context -> Report

    val debug_stack : (int * int * bool * (unit -> string list)) list ref
    val debug_push : (unit -> string list) -> unit
    val debug_push2 : (unit -> string list) -> ((unit -> string list) -> unit)
    val debug_pop  : (unit -> string list) -> unit
    val debug_push_must : (unit -> string list) -> unit
    val assert : (unit -> string list option) -> unit
  end;
