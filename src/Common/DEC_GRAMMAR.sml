(*************************************************************)
(* Grammar for Bare language - Definition v3 pages 8,9,70,71 *)
(* modified to have ident in place of con  var and excon     *)
(*************************************************************)

signature DEC_GRAMMAR =
sig
  eqtype scon
  eqtype lab
  eqtype id
  eqtype longid
  eqtype tyvar
  eqtype tycon
  eqtype longtycon
  eqtype strid
  eqtype longstrid

  structure Lab   : LAB   (* labels *)
      where type lab = lab
  structure SCon  : SCON  (* special constants *)                            
      where type scon = scon
  structure Ident : IDENT (* identifiers - variables or constructors *) 
      where type id = id
      where type longid = longid
	  where type strid = strid
  structure TyVar : TYVAR (* type variables *)  
      where type SyntaxTyVar = tyvar (*very confusing*)  
  structure TyCon : TYCON (* type constructors *)     
     where type strid = Ident.strid           
     where type tycon = tycon
     where type longtycon = longtycon	 
  structure StrId : STRID (* structure identifiers *)
     where type strid = strid
	 where type longstrid = longstrid

  type info       (* info about the position in the source text, errors etc *)
  val bogus_info : info

  datatype 'a op_opt = OP_OPT of 'a * bool
  datatype 'a WithInfo = WITH_INFO of info * 'a
  val strip_info : 'a WithInfo -> 'a

  datatype atexp =
	SCONatexp of info * scon |         
	IDENTatexp of info * longid op_opt |
	INSTatexp of info * longid op_opt * ty list |
	RECORDatexp of info * exprow option |
	LETatexp of info * dec * exp |
	PARatexp of info * exp

  and exprow =
	EXPROW of info * lab * exp * exprow option

  and exp =
	ATEXPexp of info * atexp |
	APPexp of info * exp * atexp |
	TYPEDexp of info * exp * ty |
        SORTEDexp of info * exp * ty list |
	HANDLEexp of info * exp * match |
	RAISEexp of info * exp |
	FNexp of info * match |
	UNRES_INFIXexp of info * atexp list
      
  and match =
        MATCH of info * mrule * match option

  and mrule =
        MRULE of info * pat * exp

  and dec = 
	VALdec of info * tyvar list * valbind |
        VALsdec of info * valsdesc |
	UNRES_FUNdec of info * tyvar list * FValBind |
		(* TEMPORARY: removed when resolving infixes after parsing. *)
	TYPEdec of info * typbind |
	SORTdec of info * typbind |
	DATATYPEdec of info * datbind |
	DATATYPE_REPLICATIONdec of info * tycon * longtycon |
	DATASORTdec of info * datbind |
	ABSTYPEdec of info * datbind * dec |
	EXCEPTIONdec of info * exbind |
	LOCALdec of info * dec * dec |
	OPENdec of info * longstrid WithInfo list |
	SEQdec of info * dec * dec |
	INFIXdec of info * int option * id list |
	INFIXRdec of info * int option * id list |
	NONFIXdec of info * id list |
	EMPTYdec of info |
	CIDREDBGdec of info * string

  and valbind =
	PLAINvalbind of info * pat * exp * valbind option |
	RECvalbind of info * valbind

  and valsdesc =
      VALSDESC of info * id * ty list * valsdesc option

  and FValBind = FVALBIND of info * FClause * FValBind option
  and FClause = FCLAUSE of info * atpat list * ty option * exp * FClause option

  and typbind =
        TYPBIND of info * tyvar list * tycon * ty * typbind option

  and datbind =
        DATBIND of info * tyvar list * tycon * conbind * datbind option

  and conbind =
        CONBIND of info * longid op_opt * ty option * conbind option

  and exbind =
        EXBIND of info * id op_opt * ty option * exbind option |
        EXEQUAL of info * id op_opt * longid op_opt * exbind option

  and atpat =
        WILDCARDatpat of info |
	SCONatpat of info * scon |
	LONGIDatpat of info * longid op_opt |
	RECORDatpat of info * patrow option |
	PARatpat of info * pat

  and patrow =
        DOTDOTDOT of info |
        PATROW of info * lab * pat * patrow option

  and pat =
        ATPATpat of info * atpat |
        CONSpat of info * longid op_opt * atpat |
        TYPEDpat of info * pat * ty |
        SORTEDpat of info * pat * ty list |
        LAYEREDpat of info * id op_opt * ty option * pat |
	UNRES_INFIXpat of info * atpat list

  and ty =
        TYVARty of info * tyvar |
        RECORDty of info * tyrow option |
        CONty of info * ty list * longtycon |
        INTERty of info * ty * ty |
        FNty of info * ty * ty |
        PARty of info * ty

  and tyrow =
        TYROW of info * lab * ty * tyrow option

(*  (*[ datasort resolvedDec = 
	VALdec of info * tyvar list * valbind |
        VALsdec of info * valsdesc |
	TYPEdec of info * typbind |
	SORTdec of info * typbind |
	DATATYPEdec of info * datbind |
	DATATYPE_REPLICATIONdec of info * tycon * longtycon |
	DATASORTdec of info * datbind |
	ABSTYPEdec of info * datbind * dec |
	EXCEPTIONdec of info * exbind |
	LOCALdec of info * dec * dec |
	OPENdec of info * longstrid WithInfo list |
	SEQdec of info * dec * dec |
	INFIXdec of info * int option * id list |
	INFIXRdec of info * int option * id list |
	NONFIXdec of info * id list |
	EMPTYdec of info
  ]*) *)

  val get_info_atexp : atexp -> info
  val get_info_exprow : exprow -> info
  val get_info_exp : exp -> info
  val get_info_match : match -> info
  val get_info_mrule : mrule -> info
  val get_info_dec : dec -> info
  val get_info_valsdesc : valsdesc -> info
  val get_info_valbind : valbind -> info
  val get_info_datbind : datbind -> info
  val get_info_conbind : conbind -> info
  val get_info_pat : pat -> info
  val get_info_atpat : atpat -> info
  val get_info_patrow : patrow -> info
  val get_info_ty : ty -> info
  val get_info_typbind : typbind -> info
  val get_info_tyrow : tyrow -> info
  val get_info_exbind : exbind -> info
  val get_info_FValBind : FValBind -> info
  val get_info_FClause : FClause -> info

  val map_atexp_info : (info -> info) -> atexp -> atexp
  val map_exprow_info : (info -> info) -> exprow -> exprow
  val map_exp_info : (info -> info) -> exp -> exp
  val map_match_info : (info -> info) -> match -> match
  val map_mrule_info : (info -> info) -> mrule -> mrule
  val map_dec_info : (info -> info) -> dec -> dec
  val map_valbind_info : (info -> info) -> valbind -> valbind
  val map_valsdesc_info : (info -> info) -> valsdesc -> valsdesc
  val map_datbind_info : (info -> info) -> datbind -> datbind
  val map_conbind_info : (info -> info) -> conbind -> conbind
  val map_pat_info : (info -> info) -> pat -> pat
  val map_atpat_info : (info -> info) -> atpat -> atpat
  val map_patrow_info : (info -> info) -> patrow -> patrow
  val map_ty_info : (info -> info) -> ty -> ty


  val getExplicitTyVarsTy      : ty -> tyvar list
  and getExplicitTyVarsConbind : conbind -> tyvar list

  (*expansive harmless_con exp = true iff exp is expansive.
   harmless_con longid = true iff longid is an excon or a con different
   from id_REF.  To know this, the context is necessary; that is the
   reason you must provide harmless_con.*)

  val expansive : (longid -> bool) -> exp -> bool

  val find_topmost_id_in_pat : pat -> string option
  val find_topmost_id_in_atpat: atpat -> string option

  (*is_'true'_'nil'_etc & is_'it' are used to enforce some syntactic
   restrictions (Definition, �2.9 & �3.5).*)

  val is_'true'_'nil'_etc : id -> bool
  val is_'it' : id -> bool

  val layoutTyvarseq : tyvar list -> StringTree.t option
  val layoutTy :       ty	  -> StringTree.t
  val layoutAtpat :    atpat	  -> StringTree.t
  val layoutPat :      pat	  -> StringTree.t
  val layoutExp :      exp	  -> StringTree.t
  val layoutAtexp :    atexp	  -> StringTree.t
  val layoutDec :      dec	  -> StringTree.t
  val layout_datatype_replication : info * tycon * longtycon -> StringTree.t
  val layoutMatch :    match      -> StringTree.t
end;
