(* RefineError information, including source-info *)

(*
$File: Common/REFINE_ERROR_INFO.sml $
$Date: 2002/02/05 15:02:49 $
$Revision: 1.6 $
$Locker:  $
*)

(*$REFINE_ERROR_INFO*)

signature REFINE_ERROR_INFO =
  sig
    type Sort and SortScheme and Type and longsortcon and longid 
     and SourceInfo
    type Report

    datatype ErrorInfo =
        LOOKUP_LONGCON of longid
      | LOOKUP_LONGSORTCON of longsortcon
      | SORTING of Sort
      | NOT_SUBSORT of Sort * Sort
      | CANT_APPLY of Sort
      | REDEX
      | INCOMPATIBLE of SortScheme * SortScheme
      | WRONG_TYPE of Sort * Type
      | SHADOWED of longsortcon
      | VARIANCE
      | WRONG_ARITY of {expected: int, actual: int}
      | NOT_COVARIANT
      | MULTIPLE_BINDINGS
      | UNMATCHED of Sort
      | NOT_UNIQUE
      | SHARING_REFINEMENTS_DIFFER

    type Error = SourceInfo * ErrorInfo

    val report: Error -> Report
  end;
