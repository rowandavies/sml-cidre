
signature MAP_DEC_INFO = 
  sig 
        structure IG : DEC_GRAMMAR
        structure OG : DEC_GRAMMAR
		     where type Lab.lab = IG.Lab.lab
		     where type SCon.scon = IG.SCon.scon
		     where type Ident.strid = IG.Ident.strid
		     where type Ident.longid = IG.Ident.longid
		     where type Ident.id = IG.Ident.id
		     where type TyVar.SyntaxTyVar = IG.TyVar.SyntaxTyVar
		     where type TyVar.Variance = IG.TyVar.Variance
		     where type TyCon.tycon = IG.TyCon.tycon
		     where type TyCon.longtycon = IG.TyCon.longtycon
		     where type StrId.longstrid = IG.StrId.longstrid
        val map_dec_info : (IG.info -> OG.info) -> IG.dec -> OG.dec
        val map_ty_info : (IG.info -> OG.info) -> IG.ty -> OG.ty
        val map_datbind_info : (IG.info -> OG.info) -> IG.datbind -> OG.datbind
  end
