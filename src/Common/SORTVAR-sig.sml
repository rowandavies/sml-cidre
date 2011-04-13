(* sort variables *)
(* NOT USED: in RML/Kit3, TYVAR is used instead *)


(*$SORTVAR*)
signature SORTVAR = 
  sig 
    eqtype SyntaxSortVar
    datatype Variance = COVARIANT | CONTRAVARIANT | IGNORED | MIXED

    val mk_SortVar: string -> SyntaxSortVar	(* NEW PARSER *)
    and pr_sortvar: SyntaxSortVar -> string

    val variance : SyntaxSortVar -> Variance
    val variancePrefix : Variance -> string

    val covariant : Variance -> bool
    val contravariant : Variance -> bool

    val join_variance : Variance * Variance -> Variance

  end;
