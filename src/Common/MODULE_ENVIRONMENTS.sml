(*B in Basis, F in FunEnv, and G in SigEnv, Definition 1997,
fig. 11, p. 29*)

signature MODULE_ENVIRONMENTS =
  sig

(*    structure rEnv : REFINED_ENVIRONMENTS *)

    (*types provided by this module:*)
    type Basis  
    type FunEnv 
    type SigEnv 

    structure rEnv : REFINED_ENVIRONMENTS 
    structure TyName : TYNAME 

    (*types from other modules:*)
    type TyName = TyName.TyName
    type TyVar
    type TyStr
    type Env
    type rEnv = rEnv.Env
    type Sig
    type FunSig
    type Context
    type rContext = rEnv.Context
    type rTyNameEnv = rEnv.TyNameEnv
    type realisation
    eqtype id
    type longid
    type strid (* = rEnv.strid *)
    type tycon = TyName.tycon
    type longstrid (* = rEnv.longstrid *)
    type longtycon (* = rEnv.longtycon *)
    type sigid
    type funid
    type Report

(*    structure TyName : TYNAME 
                          where type TyName = TyName 
                            and type tycon = tycon
                            and type Set.StringTree = StringTree
*)
(*    structure rEnv : REFINED_ENVIRONMENTS 
                        where   type Env = rEnv
                            and type Context = rContext
                            and type TyNameEnv = rTyNameEnv
                            and type tycon = tycon
                            and type longtycon = longtycon
*)

    (*The report functions below are used for top-level printout*)

                       (*G, signature environments*)

    structure G :
      sig
	val empty            : SigEnv
	val singleton        : sigid  * Sig    -> SigEnv
	val plus             : SigEnv * SigEnv -> SigEnv
	val lookup           : SigEnv -> sigid -> Sig option
	val tynames          : SigEnv -> TyName.Set.Set
	val dom              : SigEnv -> sigid EqSet.Set
	val layout           : SigEnv -> StringTree.t
	val report           : (sigid * Sig -> Report) * SigEnv -> Report
      end


                       (*F, functor environments*)
    type prjid = string

    structure F :
      sig
	val empty            : FunEnv
	val singleton        : funid  * (prjid * FunSig) -> FunEnv
	val plus             : FunEnv * FunEnv -> FunEnv
	val lookup           : FunEnv -> funid -> (prjid * FunSig) option
	val tynames          : FunEnv -> TyName.Set.Set
	val tyvars           : FunEnv -> TyVar list
	val tyvars'          : FunEnv -> (id * TyVar list) list
	val dom              : FunEnv -> funid EqSet.Set
	val layout           : FunEnv -> StringTree.t
	val report           : (funid * FunSig -> Report) * FunEnv -> Report
      end


                          (*B, static basis*)

    structure B :
      sig
	val empty            : Basis
	val initial          : Basis
	val bogus            : Basis
	val plus             : Basis * Basis   -> Basis
(*	val on               : realisation -> Basis -> Basis *)
	val tyvars           : Basis -> TyVar list
	val tynames          : Basis -> TyName.Set.Set
	val to_C             : Basis -> Context
        val to_rC            : Basis -> rContext
	val layout           : Basis -> StringTree.t
	val tyvars'          : Basis -> (id * TyVar list) list

                     	(*E component*)
	val plus_E           : Basis * Env     -> Basis
	val from_E           : Env -> Basis
	val to_E             : Basis -> Env
	val lookup_strid     : Basis -> strid -> Env option
	val lookup_longstrid : Basis -> longstrid -> Env option
	val lookup_longtycon : Basis -> longtycon -> TyStr option

                     	(* rE component *)
	val plus_rE           : Basis * rEnv     -> Basis
	val plus_rT           : Basis * rTyNameEnv  -> Basis
	val from_rE           : rEnv -> Basis
        val from_rE_rT        : rEnv * rTyNameEnv -> Basis
	val to_rE             : Basis -> rEnv
	val lookup_rstrid     : Basis -> strid -> rEnv option
	val lookup_rlongstrid : Basis -> longstrid -> rEnv option
(*	val lookup_rlongtycon : Basis -> longtycon -> TyStr option (* fix for sharing 16jan02 *) *)
        val erase_TG           : Basis -> Basis

	                (*G component*)
	val from_G           : SigEnv -> Basis
	val to_G             : Basis -> SigEnv
	val plus_G           : Basis  * SigEnv -> Basis
	val lookup_sigid  : Basis -> sigid -> Sig option

	                (*F component*)
	val from_F           : FunEnv -> Basis
	val to_F             : Basis -> FunEnv
	val plus_F           : Basis  * FunEnv -> Basis
	val lookup_funid     : Basis -> funid -> (prjid * FunSig) option

	           (*for compilation manager*)
(*	val enrich           : Basis * Basis -> bool *)
	val agree            : longstrid list * Basis * Basis -> bool
	val restrict         : Basis * {longvids : longid list, longtycons : longtycon list,
					longstrids : longstrid list, funids : funid list,
					sigids : sigid list} -> Basis
	val match            : Basis * Basis -> unit
      end
  end;
