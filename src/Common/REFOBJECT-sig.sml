(* Objects of the refinement semantics *)

(*
$File: Common/REFOBJECT.sml $
$Date: 2004/10/14 14:10:59 $
$Revision: 1.6 $
$Locker:  $
*)

(*$REFOBJECT *)

signature REFOBJECT =
  sig 

    structure ExplicitTyVar : TYVAR

    type TyVar
    type Type
    type SortName
    type TyName
    type ('a, 'b) sortedFinMap    

    (********
    Sort variables
    ********)

    type BoundSortVar
    type SortVar
    type Variance = ExplicitTyVar.Variance
    type SVNames and TVNames

    val newSVNames: unit -> SVNames
    val TVNamesOfSVNames : SVNames -> TVNames

    val equal_SortVar : SortVar * SortVar -> bool
    val SVofTV : TyVar -> SortVar
    val TVofSV : SortVar -> TyVar option  (* None if bound SortVar *)
    val fresh_sv : unit -> SortVar

    val pr_SortVar: SortVar -> string
    val pr_SortVarPRETTY: SVNames -> SortVar -> string

    (****
    Sorts
    ****)

    type Sort and FunSort and ConsSort and RecSort and ConjSort

    (* Conjoin sortnames.  Can be used to check subsorting via "conjSN (s1, s2) = s1" *)
    type conjSN = SortName * SortName -> SortName 
    type conjSN_opt = SortName * SortName -> SortName option (* None if no SN for conj'n yet *)

    (* Apply a refinement of a function type to an argument sort, return the best sort for result*)
    val applySort  : conjSN -> Sort * Sort -> Sort option

    (* Check inclusion of a sort in a union of sorts, given a function to compare an intersection
       of sortnames with a union of intersections of sortnames.  Also given conjSN_opt which may
       return intersections of sortnames if they've been created (for efficiency).  *)
    val subSortList : ((SortName list * SortName list list -> bool) * conjSN_opt)
                      -> Sort * Sort list -> bool
    val subSort    : conjSN -> Sort * Sort -> bool  (* Subsorting.  conjSN is for base lattices. *)
    val equal_Sort : conjSN -> Sort * Sort -> bool
    val emptySort  : conjSN -> Sort -> bool
    val addSortToMaximals : conjSN -> Sort * Sort list -> Sort list
    val compatible_Sort : Sort * Sort -> bool
    val sortvarsSort : Sort -> SortVar list
    val pr_Sort: Sort -> string
    val pr_SortPRETTY: SVNames -> Sort -> string

    val sntx_eqsrt : Sort * Sort -> bool

    (* These can be set from elsewhere, for consistent debug printing.  *)
    (* See RefinedEnvironments.  *)
    val debug_push_ref : ((unit -> string list) -> unit) ref
    val debug_push_must_ref : ((unit -> string list) -> unit) ref
    val debug_pop_ref: ((unit -> string list) -> unit) ref
    val assert_ref : ((unit -> string list option) -> unit) ref




    (********
    SortSchemes
    ********)

    type SortScheme
    val equal_SortScheme : conjSN
                              -> SortScheme * SortScheme -> bool
    val pr_SortScheme: SortScheme -> string
    val pr_SortSchemePRETTY: SVNames -> SortScheme -> string

    (********
    Sort constructors - changed to tycon, not needed here - rowan 11jul01
    ********)

    (********
    Get an instance of a SortScheme
     - be careful with bound sort vars in Sort list.
    ********)

    val instance : SortScheme * Sort list -> Sort
    val instance_vars : SortScheme -> SortVar list * Sort

    (********
    Construct and destruct sorts
    ********)

    val mkSortSortVar  : SortVar -> Sort
    and unSortSortVar : Sort -> SortVar option

    val mkSortRecSort : RecSort -> Sort
    and unSortRecSort : Sort -> RecSort option

    val mkSortFunSort : FunSort -> Sort
    and unSortFunSort : Sort -> FunSort option

    val mkSortConsSort : ConsSort -> Sort
    and unSortConsSort : Sort -> ConsSort option

    val mkSortConjSort : ConjSort -> Sort
    and unSortConjSort : Sort -> ConjSort option

    val bogusSort : Sort
    and isBogusSort : Sort -> bool

    (********
    Record labels
    ********)

    type lab

    (********
    Construct and destruct record sorts
    ********)

    val emptyRecSort : RecSort			(* "{}", refines type "{}" *)
    val emptyFlexRecSort: unit -> RecSort	(* "{...}" *)

    val addField     : lab * Sort -> RecSort -> RecSort

    val mkSortPair : Sort * Sort -> Sort
    and unRecPair : RecSort -> Sort * Sort
    val mkSortTriple : Sort * Sort * Sort -> Sort

    val SortUnit : Sort

    (********
    Construct and destruct function sorts
    ********)

    val mkFunSort : Sort * Sort -> FunSort
    and unFunSort : FunSort -> (Sort * Sort) option

    (********
    Construct and destruct constructed sorts
    ********)

    val mkConsSort : Sort list * SortName -> ConsSort
    and unConsSort : ConsSort -> (Sort list * SortName) option

    (********
    Construct and destruct conjuction sorts
    ********)

    val mkConjSort : conjSN_opt 
                        -> Sort * Sort -> ConjSort
    and unConjSort : ConjSort -> (Sort * Sort) option

    (********
    Construct and destruct sort schemes
    ********)

    val mkSortScheme : SortVar list * Sort -> SortScheme
    val unSortScheme : SortScheme -> SortVar list * Sort

    (********
    Make a sort/sortscheme into a sortscheme with no/all/some bound variables
    ********)

    val Sort_in_SortScheme : Sort -> SortScheme
    val Close_Sort : Sort -> SortScheme   (* careful!  Closes ALL sortvars.  *)
    val Close_SortScheme : SortScheme * SortVar list -> SortScheme

    (********
    Check compatibility.  
    Conjoin if compatible, otherwise return the second.
    ********)

    val compatible_SortScheme : SortScheme * SortScheme -> bool
    val plus_SortScheme : conjSN_opt
                          -> SortScheme * SortScheme -> SortScheme


    (* Combine constructor sortschemes, adding conjuncts for distributivity *)
    val combineSortSchemes : conjSN_opt
                             -> SortScheme * SortScheme -> SortScheme

    (***********
    Instantiation via upper and lower bounds, a la local type inference.
    ************)
    val isInstance : conjSN -> Sort -> SortScheme -> bool

    val sntx_eqsrtsch : SortScheme * SortScheme -> bool

    (********
    Derived constructors and destructors
    ********)

    val SortExn: Sort
    and isSortExn: Sort -> bool

    val SortBool: Sort

    val mkSortArrow: Sort * Sort -> Sort
    and unSortArrow: Sort -> (Sort * Sort) option
    val isSortArrow: Sort -> bool

    val mkSortCons: Sort list * SortName -> Sort
    and unSortCons: Sort -> (Sort list * SortName) option
    val isSortCons: Sort -> bool

    val mkSortConj: conjSN_opt -> Sort * Sort -> Sort
    and unSortConj: Sort -> (Sort * Sort) option
    val isSortConj: Sort -> bool
    and firstConjunct : Sort -> Sort
    and list_Conjuncts : Sort -> Sort list

    val recSortToMap : RecSort -> (lab, Sort) sortedFinMap

    (********
    Find the type refined by a sort, and ML Sort of a type.
    ********)

    val tyOfSort   : Sort -> Type
    val MLSortOfTy : (TyName -> SortName) -> Type -> Sort

    (********
    Test whether there are any "covariant" constructors in a sort,
    given a function which makes the test on sort names.
    ********)

    val covariant_sort : (SortName -> bool) -> Sort -> bool

    (********
    Special constants
    ********)

    type scon

    (********  
    Get sort of special constants
    ********)

    val SortInt : Sort
    and SortReal : Sort
    and SortString : Sort
    val GetSortscon : scon * bool -> Sort  (* bool indicates WORD8 *)

    (********
    Sort functions
    ********)

    type SortFcn and TypeFcn
    val equal_SortFcn : conjSN
                          -> SortFcn * SortFcn -> bool
    val mkSortFcn     : SortVar list * Sort -> SortFcn
    val unSortFcn     : SortFcn -> SortVar list * Sort
    val varianceSortFcn : SortFcn -> Variance list

    val compatible_SortFcn : SortFcn * SortFcn -> bool
    val conjSortFcn : conjSN_opt -> SortFcn * SortFcn -> SortFcn

    (* Be careful not to have bound sort-vars in the Sort list. *)
    val applySortFcn  : SortFcn * Sort list -> Sort
    val applySortFcnVars  : SortFcn -> SortVar list * Sort

    val arity_SortFcn : SortFcn -> int
    and SortName_in_SortFcn : SortName  -> SortFcn
    and SortFcn_to_SortName : SortFcn -> SortName option
    and TypeFcn_of_SortFcn  : SortFcn -> TypeFcn

    val pr_SortFcnPRETTY : SVNames -> SortFcn -> {vars: string, body: string}
    
    (* Rename some sortnames and simplify syntactic conjunctions. *)
    val renameSimpl_SFcn : (SortName -> SortName) * conjSN -> SortFcn -> SortFcn

    (********
    PrettyPrinter hooks
    ********)

    val layoutSortName: SortName -> StringTree.t
    val layoutSortVar: SortVar -> StringTree.t
    val layoutSort : Sort -> StringTree.t
    val layoutSortScheme : SortScheme -> StringTree.t
    val layoutSortFcn : SortFcn -> StringTree.t

    type realisation 
     and trealisation (* realisations for tynames (see StatObject) *)
    structure SortName : SORTNAME
    structure Realisation :
      sig
	val on_SortName             : conjSN -> realisation -> SortName -> SortFcn
(*	val on_SortName_set         : realisation -> SortName.Set.Set -> SortName.Set.Set *)
	val on_Sort                 : conjSN -> realisation -> Sort -> Sort
	val on_SortFcn              : conjSN -> realisation -> SortFcn -> SortFcn
	val on_SortScheme           : conjSN -> realisation -> SortScheme -> SortScheme
	val Id                      : realisation
	val is_Id                   : realisation -> bool
	val oo                      : conjSN -> realisation -> realisation -> realisation
	val singleton               : SortName * SortFcn -> realisation
        val singleSN                : SortName * SortName -> realisation

	(*from_T_and_sortname (T, t0) = the realisation {t |-> t0 | t in T} *)
	val restrict                : SortName.Set.Set -> realisation -> realisation
	val restrict_from           : SortName.Set.Set -> realisation -> realisation
	val renaming        : trealisation -> SortName.Set.Set -> realisation
	val renaming'       : trealisation -> SortName.Set.Set -> SortName.Set.Set * realisation
(*	val inverse                 : realisation -> realisation option
	val enrich                  : realisation * (realisation * SortName.Set.Set) -> bool
	val match                   : conjSN -> realisation * realisation -> unit  *)
	val dom                     : realisation -> SortName.Set.Set
        val plus                    : realisation -> realisation -> realisation
(*	val eq                      : realisation * realisation -> bool *)
	val layout                  : realisation -> StringTree.t

      end (*Realisation*)

  end;
