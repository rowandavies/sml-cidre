(*$REFDEC*)

signature REFDEC =
sig
   structure Comp : COMP

   type Context 
    and TyNameEnv 
    and Env 
    and Sort

   type PostElabDec
   type PostElabTy
   type PostElabDatBind

   val ref_dec : Context * PostElabDec -> (TyNameEnv * Env) Comp.Comp
   val ref_ty_covar : Context * PostElabTy -> Sort Comp.Comp

   val ref_datsortbind_complete : Context * PostElabDatBind -> (TyNameEnv * Env) Comp.Comp
end;
