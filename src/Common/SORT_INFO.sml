(* Sort information for the compiler *)

(*
$File: Common/SORT_INFO.sml $
$Date: 2001/07/06 08:53:07 $
$Revision: 1.4 $
$Locker:  $
*)

(*$SORT_INFO*)
signature SORT_INFO =
  sig
    type Sort
    type SortVar
    type SortName

    (*  See comment in TYPE_INFO *)

    datatype SortInfo =
	LAB_INFO of {sortvars: SortVar list, Sort : Sort}
			(* Attached to PATROW. 
                           The Sort field is the sort of the pattern
			   corresponding to the label, sortvars are the bound 
                           sort variables; there will only be bound sortvars
			   when attached to a pattern in a valbind.*)

      | RECORD_ATPAT_INFO of {Sort : Sort}
	                (* Attached to RECORDatpat during refinement.  The
			   sort must be a record sort. *)

      | VAR_INFO of {instances : Sort list}
	                (* Attached to IDENTatexp,
			   instances is the list of sorts which have been 
			   chosen to instantiate the generic sortvars at this 
			   variable. *)
      | VAR_PAT_INFO of {sortvars: SortVar list, Sort: Sort}
	                (* Attached to LAYEREDpat and LONGIDatpat (for LONGVARs)
			   The Sort field is the sort of the pattern corresponding
			   to the variable, sortvars are the bound sort variables;
			   there will only be bound sortvars when attached to a pattern
			   in a valbind. *)
      | CON_INFO of {instances: Sort list, sortvars : SortVar list, Sort: Sort}
			(* Attached to IDENTatexp, LONGIDatpat, CONSpat.
			   instances is the list of sorts wich have been
			   chosen to instantiate the generic sortars at this 
			   occurrence of the constructor.
			   Sort is the sort of the occurrence of the constructor,
			   sortvars are the bound sort variables; 
			   there will only be bound sortvars when 
			   attached to a pattern in a valbind
			 *)
      | EXCON_INFO of {Sort: Sort}
			(* Attached to IDENTatexp, LONGIDatpat, CONSpat.
			   The Sort field is the sort of the occurrence of the
			   excon. *)
      | DATSORTBIND_INFO of {SortName: SortName}
	                (* ATTACHED to DATSORTBIND
			 * The sort name associated with this datasort binding *)
      | EXP_INFO of {Sort: Sort} 
	                (* Attached to all exp's *)
      | MATCH_INFO of {Sort: Sort}
	                (* Attached to MATCH *)
      | PLAINvalbind_INFO of {sortvars: SortVar list, Sort: Sort}
	                (* Attached to PLAINvalbind 
			   for 'pat = exp' this is the sort of the exp, and 
			   a list of bound sort variables. *)

    type StringTree
    val layout: SortInfo -> StringTree
  end;
