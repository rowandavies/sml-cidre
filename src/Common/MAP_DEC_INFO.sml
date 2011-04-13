
signature MAP_DEC_INFO = 
  sig 
        structure IG : DEC_GRAMMAR
        structure OG : DEC_GRAMMAR
		     sharing OG.Lab = IG.Lab
		     sharing OG.SCon = IG.SCon
		     sharing OG.Ident = IG.Ident
		     sharing OG.TyVar = IG.TyVar
		     sharing OG.TyCon = IG.TyCon
		     sharing OG.StrId = IG.StrId
        val map_dec_info : (IG.info -> OG.info) -> IG.dec -> OG.dec
        val map_ty_info : (IG.info -> OG.info) -> IG.ty -> OG.ty
        val map_datbind_info : (IG.info -> OG.info) -> IG.datbind -> OG.datbind
  end
