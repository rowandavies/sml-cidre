
(* Map from a grammar to another one with a different info type attached to 
   the AST nodes.
*)

functor MapDecInfo(structure IG : DEC_GRAMMAR
		   structure OG : DEC_GRAMMAR
		     sharing OG.Lab = IG.Lab
		     sharing OG.SCon = IG.SCon
		     sharing OG.Ident = IG.Ident
		     sharing OG.TyVar = IG.TyVar
		     sharing OG.TyCon = IG.TyCon
		     sharing OG.StrId = IG.StrId
) : MAP_DEC_INFO (* where IG = IG and OG = OG *)

=

struct

  structure IG = IG
  structure OG = OG

  fun map_WITH_INFO (f: IG.info -> OG.info) (IG.WITH_INFO (i, x)) = OG.WITH_INFO (f i, x)

  fun do_op_opt (IG.OP_OPT (x, b)) = OG.OP_OPT(x, b)

  local 
    fun do_opt opt f =
      case opt of 
	NONE => NONE 
      | SOME x  => SOME(f x)
    open IG
  in
    fun map_atexp_info (f:IG.info -> OG.info) (atexp : IG.atexp) : OG.atexp =
      case atexp of
	IG.SCONatexp(i,scon) => OG.SCONatexp(f i,scon)
      | IG.IDENTatexp(i, op_opt) => OG.IDENTatexp(f i, do_op_opt op_opt)
      | IG.RECORDatexp(i, NONE) => OG.RECORDatexp(f i,NONE)
      | IG.RECORDatexp(i, SOME exprow) =>
	  OG.RECORDatexp(f i, SOME (map_exprow_info f exprow))
      | IG.LETatexp(i, dec, exp) => OG.LETatexp(f i, map_dec_info f dec, map_exp_info f exp)
      | IG.PARatexp(i, exp) => OG.PARatexp(f i, map_exp_info f exp)
	    
    and map_exprow_info f (exprow: exprow)  =
      case exprow of 
	IG.EXPROW(i, l, exp, NONE) => OG.EXPROW(f i, l, map_exp_info f exp, NONE)
      | IG.EXPROW(i, l, exp, SOME exprow) =>
	  OG.EXPROW(f i, l, map_exp_info f exp, SOME (map_exprow_info f exprow))
	    
    and map_exp_info f (exp: exp)  =
      case exp of
	IG.ATEXPexp(i, atexp) => OG.ATEXPexp(f i, map_atexp_info f atexp)
      | IG.APPexp(i, exp, atexp) => OG.APPexp(f i, map_exp_info f exp, map_atexp_info f atexp)
      | IG.TYPEDexp(i, exp, ty) =>
	  OG.TYPEDexp(f i, map_exp_info f exp, map_ty_info f ty)
      | IG.SORTEDexp(i, exp, tys) =>
	  OG.SORTEDexp(f i, map_exp_info f exp, map (map_ty_info f) tys)
      | IG.HANDLEexp(i, exp, match) =>
	  OG.HANDLEexp(f i, map_exp_info f exp, map_match_info f match)
      | IG.RAISEexp(i, exp) => OG.RAISEexp(f i, map_exp_info f exp)
      | IG.FNexp(i, match) =>
	  OG.FNexp(f i, map_match_info f match)
      | IG.UNRES_INFIXexp(i,atexps) =>
	  OG.UNRES_INFIXexp(f i, map (map_atexp_info f) atexps)
	    
    and map_match_info f (match: match)  =
      case match of 
	IG.MATCH(i, mrule, NONE) => OG.MATCH(f i, map_mrule_info f mrule, NONE)
      | IG.MATCH(i, mrule, SOME match) =>
	  OG.MATCH(f i, map_mrule_info f mrule, SOME (map_match_info f match))
	    
    and map_mrule_info f (IG.MRULE(i, pat, exp) : mrule)  =
      OG.MRULE(f i, map_pat_info f pat, map_exp_info f exp)
	
    and map_dec_info f (dec : dec)  =
      case dec of 
	IG.VALdec(i, tyvars, valbind) => OG.VALdec(f i, tyvars, map_valbind_info f valbind)
      |	IG.VALsdec(i, valsdesc) => OG.VALsdec(f i, map_valsdesc_info f valsdesc)
      | IG.UNRES_FUNdec(i, tyvars, FValBind) =>
	  OG.UNRES_FUNdec(f i, tyvars, map_FValBind_info f FValBind)
      | IG.TYPEdec(i,typbind) => OG.TYPEdec(f i, map_typbind_info f typbind)
      | IG.SORTdec(i,typbind) => OG.SORTdec(f i, map_typbind_info f typbind)
      | IG.DATATYPEdec(i,datbind) => OG.DATATYPEdec(f i,map_datbind_info f datbind)
      | IG.DATATYPE_REPLICATIONdec(i, tycon, longtycon) =>
	  OG.DATATYPE_REPLICATIONdec(f i, tycon, longtycon)
      | IG.DATASORTdec(i,datbind) => OG.DATASORTdec(f i,map_datbind_info f datbind)
      | IG.ABSTYPEdec(i, datbind, dec) =>
	  OG.ABSTYPEdec(f i, map_datbind_info f datbind, map_dec_info f dec)
      | IG.EXCEPTIONdec(i,exbind) => OG.EXCEPTIONdec(f i, map_exbind_info f exbind)
      | IG.LOCALdec(i, dec1, dec2) =>
	  OG.LOCALdec(f i, map_dec_info f dec1, map_dec_info f dec2)
      | IG.OPENdec(i, ls) => OG.OPENdec(f i, map (map_WITH_INFO f) ls)
      | IG.SEQdec(i, dec1, dec2) =>
	  OG.SEQdec(f i, map_dec_info f dec1, map_dec_info f dec2)
      | IG.INFIXdec(i,x,y) => OG.INFIXdec(f i,x,y)
      | IG.INFIXRdec(i,x,y) => OG.INFIXRdec(f i,x,y)
      | IG.NONFIXdec(i,x) => OG.NONFIXdec(f i,x)
      | IG.EMPTYdec i => OG.EMPTYdec (f i)
	    
    and map_FValBind_info f (IG.FVALBIND(i,FClause,FValBind_opt))  =
      OG.FVALBIND(f i, 
	       map_FClause_info f FClause,
	       case FValBind_opt of
		 NONE => NONE 
	       | SOME FValBind => SOME (map_FValBind_info f FValBind))
	  
    and map_FClause_info f (IG.FCLAUSE(i, atpats,tyOpt,exp,FClause_opt))  =
      OG.FCLAUSE(f i, 
	      map (map_atpat_info f) atpats,
	      case tyOpt of 
		  NONE => NONE
		| SOME ty => SOME(map_ty_info f ty),
	      map_exp_info f exp,
	      case FClause_opt of
		  NONE => NONE 
		| SOME FClause => SOME(map_FClause_info f FClause))
			      
    and map_valbind_info f (valbind : valbind)  =
      case valbind of
	IG.PLAINvalbind(i, pat, exp, NONE) =>
	  OG.PLAINvalbind(f i, map_pat_info f pat, map_exp_info f exp, NONE)
      | IG.PLAINvalbind(i, pat, exp, SOME valbind) =>
	  OG.PLAINvalbind(f i, map_pat_info f pat, 
		       map_exp_info f exp, SOME (map_valbind_info f valbind))
      | IG.RECvalbind(i, valbind) =>
	  OG.RECvalbind(f i, map_valbind_info f valbind)

    and map_valsdesc_info f (IG.VALSDESC(i, id, tys, valsdesc_opt) : valsdesc)  =
	  OG.VALSDESC(f i, id, map (map_ty_info f) tys,
                  case valsdesc_opt of NONE => NONE
		                    | SOME valsdesc => SOME (map_valsdesc_info f valsdesc))
      
    and map_typbind_info f (IG.TYPBIND(i,tyvars,tycon,ty,typbind_opt)) =
      OG.TYPBIND(f i,tyvars,tycon,map_ty_info f ty,
	      do_opt typbind_opt (map_typbind_info f))

    and map_datbind_info f (IG.DATBIND(i,tyvars,tycon,conbind,datbind_opt)) =
      OG.DATBIND(f i,tyvars,tycon,map_conbind_info f conbind,
	      do_opt datbind_opt (map_datbind_info f))

    and map_conbind_info f (IG.CONBIND(i,longid,ty_opt,conbind_opt)) =
      OG.CONBIND(f i, do_op_opt longid, do_opt ty_opt (map_ty_info f),
	      do_opt conbind_opt (map_conbind_info f))

    and map_exbind_info f (exbind: exbind) =
      case exbind of
	IG.EXBIND(i,id,ty_opt,exbind_opt) =>
	  OG.EXBIND(f i, do_op_opt id, do_opt ty_opt (map_ty_info f),
		 do_opt exbind_opt (map_exbind_info f))
      | IG.EXEQUAL(i,id,longid,exbind_opt) => 
	  OG.EXEQUAL(f i, do_op_opt id, do_op_opt longid,do_opt exbind_opt (map_exbind_info f))

    and map_atpat_info f (atpat : atpat)  =
      case atpat of
	IG.WILDCARDatpat i => OG.WILDCARDatpat (f i)
      | IG.SCONatpat(i,scon) => OG.SCONatpat(f i, scon)
      | IG.LONGIDatpat(i,x) => OG.LONGIDatpat(f i, do_op_opt x)
      | IG.RECORDatpat(i, NONE) => OG.RECORDatpat(f i,NONE)
      | IG.RECORDatpat(i, SOME patrow) =>
	  OG.RECORDatpat(f i, SOME (map_patrow_info f patrow))
      | IG.PARatpat(i, pat) => OG.PARatpat(f i, map_pat_info f pat)

    and map_patrow_info f (patrow : patrow)  =
      case patrow of
	IG.DOTDOTDOT(i) => OG.DOTDOTDOT (f i)
      | IG.PATROW(i, lab, pat, NONE) => OG.PATROW(f i, lab, map_pat_info f pat, NONE)
      | IG.PATROW(i, lab, pat, SOME patrow) =>
	  OG.PATROW(f i, lab, map_pat_info f pat, SOME (map_patrow_info f patrow))

    and map_pat_info f (pat : pat)  =
      case pat of
	IG.ATPATpat(i, atpat) =>
	  OG.ATPATpat(f i, map_atpat_info f atpat)
      | IG.CONSpat(i, longidopt, atpat) =>
	  OG.CONSpat(f i, do_op_opt longidopt, map_atpat_info f atpat)
      | IG.TYPEDpat(i, pat, ty) =>
	  OG.TYPEDpat(f i, map_pat_info f pat, map_ty_info f ty)
      | IG.SORTEDpat(i, pat, tys) =>
	  OG.SORTEDpat(f i, map_pat_info f pat, map (map_ty_info f) tys)
      | IG.LAYEREDpat(i, id, ty_opt, pat) =>
	  OG.LAYEREDpat(f i, do_op_opt id, do_opt ty_opt (map_ty_info f),
		     map_pat_info f pat)
      | IG.UNRES_INFIXpat(i,atpats) =>
	  OG.UNRES_INFIXpat(f i, map (map_atpat_info f) atpats)

    and map_ty_info (f:IG.info -> OG.info) (ty: ty) : OG.ty  =
      case ty of
	IG.TYVARty(i,tyvar) => OG.TYVARty(f i, tyvar)
      | IG.RECORDty(i,tyrow_opt) => OG.RECORDty(f i, do_opt tyrow_opt (map_tyrow_info f))
      | IG.CONty(i,tys,longtycon) => OG.CONty(f i, map (map_ty_info f) tys,longtycon)
      | IG.INTERty(i,ty,ty') => OG.INTERty(f i, map_ty_info f ty, map_ty_info f ty')
      | IG.FNty(i,ty,ty') => OG.FNty(f i, map_ty_info f ty, map_ty_info f ty')
      | IG.PARty(i,ty) => OG.PARty(f i, map_ty_info f ty)

    and map_tyrow_info f (IG.TYROW(i,lab,ty,tyrow_opt))  =
      OG.TYROW(f i, lab, map_ty_info f ty, do_opt tyrow_opt (map_tyrow_info f))
  end

end;
