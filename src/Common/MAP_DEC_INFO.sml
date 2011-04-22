
signature MAP_DEC_INFO = 
  sig 
        structure IG : DEC_GRAMMAR
        structure OG : DEC_GRAMMAR
		     where type lab = IG.lab
		     where type scon = IG.scon
		     where type strid = IG.strid
		     where type longid = IG.longid
		     where type id = IG.id
		     where type tyvar = IG.tyvar
		     where type TyVar.Variance = IG.TyVar.Variance
		     where type tycon = IG.tycon
		     where type longtycon = IG.longtycon
		     where type longstrid = IG.longstrid
        val map_dec_info : (IG.info -> OG.info) -> IG.dec -> OG.dec
        val map_ty_info : (IG.info -> OG.info) -> IG.ty -> OG.ty
        val map_datbind_info : (IG.info -> OG.info) -> IG.datbind -> OG.datbind
  end
