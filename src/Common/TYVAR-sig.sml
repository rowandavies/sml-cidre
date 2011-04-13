(* type variables - Definition v3 page 4 *)

(*$TYVAR*)
signature TYVAR = 
  sig 
    eqtype SyntaxTyVar

    val mk_TyVar: string -> SyntaxTyVar	(* NEW PARSER *)
    and pr_tyvar: SyntaxTyVar -> string

    val isEquality: SyntaxTyVar -> bool

    datatype Variance = COVARIANT | CONTRAVARIANT | IGNORED | MIXED

    val variance : SyntaxTyVar -> Variance
    val variancePrefix : Variance -> string

    val covariant : Variance -> bool
    val contravariant : Variance -> bool

    val join_variance : Variance * Variance -> Variance

  end;
