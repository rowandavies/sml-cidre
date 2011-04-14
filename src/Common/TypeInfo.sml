(* Type information for compiler *)

functor TypeInfo (structure Crash: CRASH
                  structure Ident: IDENT
		  structure StrId: STRID
		  structure TyCon: TYCON
		  structure PP: PRETTYPRINT
		  structure OpacityEnv : OPACITY_ENV
		  structure StatObject : STATOBJECT
		  structure Environments : ENVIRONMENTS
		    where type realisation = StatObject.realisation
		  structure ModuleEnvironments : MODULE_ENVIRONMENTS
		    ) : TYPE_INFO =
  struct

    fun die s = Crash.impossible ("TypeInfo." ^ s)

    type longid = Ident.longid
    type Type = StatObject.Type
    type TyVar = StatObject.TyVar
    type TyEnv = Environments.TyEnv
    type Env = Environments.Env
    type realisation = StatObject.realisation
    type opaq_env = OpacityEnv.opaq_env
    type strid = StrId.strid
    type tycon = TyCon.tycon
    type id = Ident.id
    type Basis = ModuleEnvironments.Basis
    structure TyName = StatObject.TyName
    type TyName = TyName.TyName
    type ExplicitTyVarEnv = Environments.ExplicitTyVarEnv

    fun map_opt f (SOME a) = SOME (f a)
      | map_opt f NONE = NONE

    val layoutType = StatObject.Type.layout
    val layoutTyVar = StatObject.TyVar.layout
    val layoutEnv = Environments.E.layout
    val layoutTyEnv = Environments.E.layout o Environments.E.from_TE

    datatype TypeInfo =
      LAB_INFO of {index: int}
    | RECORD_ATPAT_INFO of {Type : Type}
    | VAR_INFO of {instances: Type list}
    | VAR_PAT_INFO of {tyvars: TyVar list, Type: Type}
    | CON_INFO of {numCons: int, index: int, instances: Type list, longid:longid}
    | EXCON_INFO of {Type:Type,longid:longid}
    | EXBIND_INFO of {TypeOpt : Type option}
    | TYENV_INFO of TyEnv
    | ABSTYPE_INFO of TyEnv * realisation
    | DATBIND_INFO of {TyName: TyName}
    | EXP_INFO of {Type:Type}
    | MATCH_INFO of {Type:Type}
    | PLAINvalbind_INFO of {tyvars: TyVar list, Type: Type, Uenv : ExplicitTyVarEnv}
    | OPEN_INFO of strid list * tycon list * id list
    | INCLUDE_INFO of strid list * tycon list
    | FUNCTOR_APP_INFO of {rea_inst : realisation, rea_gen : realisation, Env : Env}
    | FUNBIND_INFO of {argE: Env,elabBref: Basis ref, T: TyName.Set.Set, resE: Env, opaq_env_opt: opaq_env option}
    | TRANS_CONSTRAINT_INFO of Env
    | OPAQUE_CONSTRAINT_INFO of Env * realisation
    | SIGBIND_INFO of TyName.Set.Set
    | DELAYED_REALISATION of realisation * TypeInfo

    fun on_TypeInfo' (phi,ti) =
      let fun phi_on_Type tau = StatObject.Realisation.on_Type phi tau
	  fun phi_on_TE TE = Environments.Realisation.on_TyEnv phi TE (* fn TE => TE *)  (* I wonder if abstype works now - Martin *)
	  fun phi_on_E E = Environments.Realisation.on_Env phi E        (* it used to be the identity *)
	  fun phi_on_phi' phi' = StatObject.Realisation.oo(phi,phi')
	  fun phi_on_T T = StatObject.Realisation.on_TyName_set phi T
          fun phi_on_datTyName TyName = 
              case StatObject.TypeFcn.to_TyName (StatObject.Realisation.on_TyName phi TyName)
                of NONE => Crash.impossible "TypeInfo.phi_on_datTyName"
                 | SOME TyName' => TyName'
      in case ti 
	   of LAB_INFO _ => ti
	    | RECORD_ATPAT_INFO{Type} => RECORD_ATPAT_INFO{Type=phi_on_Type Type}
	    | VAR_INFO {instances} => VAR_INFO {instances = map phi_on_Type instances}
	    | VAR_PAT_INFO {tyvars,Type} => VAR_PAT_INFO{tyvars=tyvars,Type=phi_on_Type Type}
	    | CON_INFO {numCons, index, instances, longid} =>
	     CON_INFO {numCons=numCons,index=index,
		       instances=map phi_on_Type instances,
		       longid=longid}
	    | EXCON_INFO {Type,longid} => EXCON_INFO {Type=phi_on_Type Type, longid=longid}
	    | EXBIND_INFO {TypeOpt} => EXBIND_INFO {TypeOpt = map_opt phi_on_Type TypeOpt}
	    | TYENV_INFO TE => TYENV_INFO (phi_on_TE TE)
	    | ABSTYPE_INFO (TE,phi') => ABSTYPE_INFO (phi_on_TE TE, phi_on_phi' phi')
            | DATBIND_INFO {TyName} => DATBIND_INFO {TyName=phi_on_datTyName TyName}
	    | EXP_INFO {Type} => EXP_INFO{Type=phi_on_Type Type}
	    | MATCH_INFO {Type} => MATCH_INFO{Type=phi_on_Type Type}
	    | PLAINvalbind_INFO {tyvars, Type, Uenv} =>
	     PLAINvalbind_INFO {tyvars=tyvars, Type=phi_on_Type Type, Uenv=Uenv}
	    | OPEN_INFO _ => ti
	    | INCLUDE_INFO _ => ti
	    | FUNCTOR_APP_INFO {rea_inst,rea_gen,Env} => 
	     FUNCTOR_APP_INFO {rea_inst=phi_on_phi' rea_inst, rea_gen=phi_on_phi' rea_gen, Env=phi_on_E Env}
            | FUNBIND_INFO {argE,elabBref,T,resE,opaq_env_opt} => die "on_TypeInfo': FUNBIND_INFO"
(*	     FUNBIND_INFO {argE=phi_on_E argE,elabB=elabB,T=T,resE=resE,rea_opt=SOME phi} *)
            | TRANS_CONSTRAINT_INFO E => TRANS_CONSTRAINT_INFO (phi_on_E E)
            | OPAQUE_CONSTRAINT_INFO (E,phi') => OPAQUE_CONSTRAINT_INFO (phi_on_E E, phi_on_phi' phi')
	    | SIGBIND_INFO T => SIGBIND_INFO (phi_on_T T)
(*	    | DELAYED_REALISATION (phi',ti) => on_TypeInfo'(phi_on_phi' phi', ti)  *)
	    | DELAYED_REALISATION (phi',ti) => on_TypeInfo'(phi, on_TypeInfo'(phi', ti))
      end

    fun on_TypeInfo a = DELAYED_REALISATION a
    fun normalise (DELAYED_REALISATION(phi,ti)) = on_TypeInfo'(phi, ti)
      | normalise ti = ti 

    fun layout_tyvars tyvars = 
      StringTree.NODE{start="[",finish="]",indent=0,childsep=StringTree.RIGHT",",
	      children=map layoutTyVar tyvars}

    fun layout_list (s, lay_elem) elems = 
      StringTree.NODE{start=s ^ " = [",finish="]",indent=0,childsep=StringTree.RIGHT",",
	      children=map lay_elem elems}

    val layout_strids = layout_list ("strids", StringTree.LEAF o StrId.pr_StrId)
    val layout_tycons = layout_list ("tycons", StringTree.LEAF o TyCon.pr_TyCon)
    val layout_ids = layout_list ("ids", StringTree.LEAF o Ident.pr_id)

    fun layout info =
      case info
	of LAB_INFO{index} => 
	  StringTree.LEAF ("LAB_INFO{index=" ^ Int.toString index ^ "}")

         | RECORD_ATPAT_INFO {Type} =>
	     StringTree.NODE{start="RECORD_ATPAT_INFO(",finish=")",indent=2,
		  children=[layoutType Type],
		  childsep = StringTree.NOSEP}
         | VAR_INFO {instances} => 
	     StringTree.NODE{start="VAR_INFO(", finish=")",indent=2,
		     children=map layoutType instances,
		     childsep = StringTree.RIGHT ","}
	 | VAR_PAT_INFO {tyvars, Type} =>
	     StringTree.NODE{start="VAR_PAT_INFO(",finish=")",indent=2,
		     children=[layout_tyvars tyvars,
			       layoutType Type],
		     childsep=StringTree.RIGHT ","}
	 | CON_INFO{numCons, index, longid, instances} =>
	     StringTree.NODE{start="CON_INFO(",  finish=")", indent=2,
		     children=[StringTree.LEAF("numCons: " ^ Int.toString numCons),
			       StringTree.LEAF("index: " ^ Int.toString index),
			       StringTree.NODE{start="instances: ",finish="",
				       indent=4,
				       children=map layoutType instances,
				       childsep = StringTree.RIGHT ","},
			       StringTree.LEAF("longid: " ^ Ident.pr_longid longid)
			      ],
		     childsep=StringTree.RIGHT ", "
		    }

         | EXCON_INFO{Type,longid} =>
	     StringTree.NODE{start="EXCON_INFO(",finish=")",indent=2,
		     children=[StringTree.NODE{start="Type: ",finish="",
				       indent=4,
				       children=[layoutType Type],
				       childsep = StringTree.NOSEP},
			       StringTree.LEAF("longid: " ^ Ident.pr_longid longid)],
		       childsep = StringTree.NOSEP}
	 | EXBIND_INFO{TypeOpt} =>
	     StringTree.NODE{start="EXBIND_INFO(",finish=")",indent=2,
		     children=[case TypeOpt
				 of NONE => StringTree.LEAF "NONE"
				  | SOME tau => layoutType tau],
		     childsep = StringTree.NOSEP}
	 | TYENV_INFO TE =>
	     StringTree.NODE{start="TYENV_INFO(",finish=")",indent=2,
		     children=[layoutTyEnv TE],
		     childsep = StringTree.NOSEP}
	 | ABSTYPE_INFO (TE,phi) =>
	     StringTree.NODE{start="ABSTYPE_INFO(",finish=")",indent=2,
		     children=[layoutTyEnv TE, StringTree.LEAF "phi"],
		     childsep = StringTree.RIGHT ", "}
	 | DATBIND_INFO{TyName} => 
	     StringTree.NODE{start="DATBIND_INFO(",finish=")",indent=2,
		     children=[TyName.layout TyName],
		     childsep = StringTree.NOSEP}
	 | EXP_INFO{Type} => 
	     StringTree.NODE{start="EXP_INFO(",finish=")",indent=2,
		     children=[layoutType Type],
		     childsep = StringTree.NOSEP}
	 | MATCH_INFO{Type} => 
	     StringTree.NODE{start="MATCH_INFO(",finish=")",indent=2,
		     children=[layoutType Type],
		     childsep = StringTree.NOSEP}
	 | PLAINvalbind_INFO{tyvars, Type, Uenv} =>  (* doesn't print Uenv *)
	     StringTree.NODE{start="PLAINvalbind_INFO(",finish=")",indent=2,
		     children=[layout_tyvars tyvars,
			       layoutType Type],
		     childsep = StringTree.NOSEP}
	 | OPEN_INFO (strids,tycons,ids) => StringTree.NODE{start="OPEN_INFO(",finish=")",indent=2,childsep=StringTree.RIGHT ", ",
						    children=[layout_strids strids,
							      layout_tycons tycons,
							      layout_ids ids]}
	 | INCLUDE_INFO (strids,tycons) => StringTree.NODE{start="INCLUDE_INFO(",finish=")",indent=2,childsep=StringTree.RIGHT ", ",
						   children=[layout_strids strids,
							     layout_tycons tycons]}
	 | FUNCTOR_APP_INFO {rea_inst,rea_gen,Env} => StringTree.LEAF "FUNCTOR_APP_INFO{rea_inst,rea_gen,Env}"
	 | FUNBIND_INFO {argE,elabBref,T,resE,opaq_env_opt} => StringTree.NODE{start="FUNBIND_INFO(", finish=")",
								    indent=2,childsep=StringTree.NOSEP,
								    children=[layoutEnv argE]}
	 | TRANS_CONSTRAINT_INFO Env => StringTree.NODE{start="TRANS_CONSTRAINT_INFO(", finish=")",
						indent=2,childsep=StringTree.NOSEP,
						children=[layoutEnv Env]}
	 | OPAQUE_CONSTRAINT_INFO (Env,phi) => StringTree.NODE{start="OPAQUE_CONSTRAINT_INFO(", finish=")",
						indent=2,childsep=StringTree.RIGHT ", ",
						children=[layoutEnv Env, StringTree.LEAF "phi"]}
	 | SIGBIND_INFO T => StringTree.NODE{start="SIGBIND_INFO(", finish=")",
				     indent=2,childsep=StringTree.NOSEP,
				     children=[TyName.Set.layoutSet {start="", finish="", sep=", "}
					       (StringTree.LEAF o TyName.pr_TyName) T]}
         | DELAYED_REALISATION(phi,ti) => layout (on_TypeInfo'(phi,ti))
  end;
