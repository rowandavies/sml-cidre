(*$REFDEC*)

signature REFDEC =
sig
   structure Comp : COMP

   type 'a Comp = bool -> 'a * Comp.Error list

   type Context 
    and TyNameEnv 
    and Env 
    and Sort

   type PostElabDec
   type PostElabTy
   type PostElabDatBind
   
   val howMuchIsTooMuch : int ref

   val ref_dec : Context * PostElabDec -> (TyNameEnv * Env) Comp
   val ref_ty_covar : Context * PostElabTy -> Sort Comp

   val ref_datsortbind_complete : Context * PostElabDatBind -> (TyNameEnv * Env) Comp
end;
