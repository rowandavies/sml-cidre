(* sort constructors  *)

(*
$File: Common/SORTCON.sml $
$Date: 2004/09/17 22:35:51 $
$Revision: 1.1 $
$Locker:  $
*)

(*$SORTCON*)
signature SORTCON =
  sig
    eqtype tycon
    eqtype longtycon
    eqtype strid

    eqtype sortcon
    eqtype longsortcon

    val mk_SortCon: string -> sortcon
    val mk_TypeSortCon: tycon -> sortcon

    val mk_LongSortCon: string list -> longsortcon
    val mk_TypeLongSortCon: longtycon -> longsortcon

    val implode_LongSortCon : strid list * sortcon -> longsortcon
    and explode_LongSortCon : longsortcon -> strid list * sortcon

    val sortcon_INT    : sortcon
    and sortcon_REAL   : sortcon
    and sortcon_STRING : sortcon
    and sortcon_EXN    : sortcon
    and sortcon_REF    : sortcon
    and sortcon_BOOL   : sortcon
    and sortcon_LIST   : sortcon
    and sortcon_INSTREAM    : sortcon
    and sortcon_OUTSTREAM   : sortcon
    and sortcon_UNIT   : sortcon

    val pr_SortCon : sortcon -> string
    val pr_LongSortCon : longsortcon  -> string

    val < : sortcon * sortcon -> bool		(* Needed to order
						   top-level printout. *)
  end;
