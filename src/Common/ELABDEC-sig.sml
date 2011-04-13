(* Elaborate a Core Language Declaration, dec *)

(*$ELABDEC*)

signature ELABDEC =
sig
  type Context and Env and Type and TyName
  eqtype id

  type PreElabDec and PostElabDec
  type PreElabTy  and PostElabTy

  val found_error : bool ref  (* set each time an error is found *)

  val elab_dec : Context * PreElabDec -> TyName list * Env * PostElabDec
  and elab_ty  : Context * PreElabTy  -> Type option * PostElabTy
  and elab_srt  : Context * PreElabTy  -> Type option * PostElabTy

    (* elab_ty returns `NONE' if an error occurred when elborating the
     * type expression. The reason we do things this way is that
     * errors are dealt with in two different ways depending on the
     * construct the type expression is part of. *)

  type PreElabDatBind and PostElabDatBind and TyEnv

  val datasort_TE : Context * PreElabDatBind -> TyEnv
  val elab_datbind_s : Context * PreElabDatBind -> PostElabDatBind

end;
