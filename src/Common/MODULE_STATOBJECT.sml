(*Sigma in Sig and Phi in FunSig, Definition 1997, fig. 11,
 p. 29*)

signature MODULE_STATOBJECT =
  sig
    (*types provided by this module:*)
    type Sig
    type FunSig

    type rT and rEnv

    (*types from other modules:*)
    type TyName 
    eqtype tycon
    structure TyName : TYNAME 
       where type TyName = TyName
       where type tycon = tycon

    type TyVar
    type Type
    type TypeScheme
    type TypeFcn
    type Env
    type realisation

    type Sort
    type SortScheme

    eqtype id
    type strid
    type longstrid
    type longtycon

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
    | SUBSORT of Sort * Sort
    | VARIANCE  of tycon

    exception No_match of SigMatchError
    (*raised by Sigma.match and Phi.match when matching fails*)

			    (*signature*)
    structure Sigma :
      sig
	val triv                   : Sig
	val to_T_E_rT_rE           : Sig -> TyName.Set.Set * Env * rT * rEnv
	val from_T_E_rT_rE         : TyName.Set.Set * Env * rT * rEnv -> Sig
	val tynames                : Sig -> TyName.Set.Set
	val tyvars                 : Sig -> TyVar list
	val bogus                  : Sig
	val layout                 : Sig -> StringTree.t

	(*instance Sigma = the E from a signature where the bound
	 tynames have been replaced by fresh tynames:*)

	val instance               : rT -> Sig -> Env * rT * rEnv
	val instance'              : rT -> Sig -> TyName.Set.Set * Env * rT * rEnv

	(*If E' = match (Sigma, E) succeeds, then E matches Sigma, i.e., E' is an instance of Sigma
	  and E enriches E'.  match will raise No_match if there is no match. *)
        (* rT provides the base sort lattices for sortnames in rEnv *)
	val match  : rT * Sig * Env * rEnv -> Env * rEnv
        (* match' is for opaque signature matching *)
	val match' : rT * Sig * Env * rEnv -> Env * TyName.Set.Set * Env * rT * rEnv * realisation 

	(* val eq : Sig * Sig -> bool *)
(*	val on : realisation * Sig -> Sig *)
      end

                         (*functor signature*)
    structure Phi : 
      sig
	val from_T_E_rT_rE_Sigma   : TyName.Set.Set * Env * rT * rEnv * Sig -> FunSig
	val to_T_E_rT_rE_Sigma     : FunSig -> TyName.Set.Set * Env * rT * rEnv * Sig
	val tynames                : FunSig -> TyName.Set.Set
	val tyvars                 : FunSig -> TyVar list
	val tyvars'                : FunSig -> (id * TyVar list) list
	val layout                 : FunSig -> StringTree.t

	(*If Sigma'= match (Phi, E) succeeds, then E matches the
	 argument signature of the functor signature Phi and Sigma' is 
	 the signature of the actual result of the functor application.
	 match will raise No_match if there is no match:*)

(*	val match                  : FunSig * Env -> Sig     No refinements, unused.  *)
	val match_via              : FunSig * Env * rT * rEnv -> Sig * realisation * realisation

(*	val eq : FunSig * FunSig -> bool *)

(*	val on : realisation * FunSig -> FunSig *)
      end
  end;
