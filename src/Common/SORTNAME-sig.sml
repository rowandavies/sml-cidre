(* Sort names *)

(*
$File: Common/SORTNAME.sml $
$Date: 2004/10/06 02:36:39 $
$Revision: 1.4 $
$Locker:  $
*)

(*$SORTNAME*)
signature SORTNAME =
  sig 
    type TyName

    type SortName
    type Variance

    (* Pervasive sort names (Definition p. 74) *)

    val sortName_BOOL    : SortName
    and sortName_INT     : SortName
    and sortName_REAL    : SortName
    and sortName_STRING  : SortName
    and sortName_LIST    : SortName
    and sortName_REF     : SortName
    and sortName_EXN     : SortName
    val sortName_CHAR    : SortName
    val sortName_WORD    : SortName
    val sortName_WORD8   : SortName
    val sortName_WORD_TABLE : SortName

    type sortcon

    val arity    : SortName -> int
    and conjuncts: SortName -> sortcon list
    and tyname   : SortName -> TyName
    and variance : SortName -> Variance list
    and leq      : SortName * SortName -> bool
    and eq       : SortName * SortName -> bool
    and isEmpty  : SortName -> bool

    val set_Covariance : SortName -> unit
    val allVars_Covariant : SortName -> bool
    and idnum    : SortName -> int

    val compare : SortName * SortName -> order

    type name
    val name : SortName -> name

    (* conjuncts must not be nil.  *)
    val freshSortName  : {conjuncts : sortcon list, 
			  variance : Variance list,
			  tyname : TyName} 
                           -> SortName

    val freshTypeSortName : TyName * Variance list -> SortName
    val freshEmptySortName: TyName -> SortName
    val sortNameConjuncts : SortName -> SortName list
    val newConjSortName   : SortName * SortName * bool * Variance list * TyName -> SortName
    val newConjList       : SortName list * bool * Variance list * TyName -> SortName
    val join_variance     : Variance list * Variance list -> Variance list
    val improve_variance  : SortName -> Variance list -> unit

    (* Print a sortname, with a prefix for each conjunct *)
    val pr_SortName : (string * SortName) -> string
    val pr_SortName_parens : (string * SortName) -> string    (* Adds parentheses if >1 conjunct *)
    val pr_SortName_noprefix : SortName -> string
    val layout : SortName -> StringTree.t

    structure Map : MONO_FINMAP where type dom = SortName
    structure Set : KIT_MONO_SET where type elt = SortName

  end;

