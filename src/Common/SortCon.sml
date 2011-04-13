(* Sort constructors  *)

(*
$File: Common/SortCon.sml $
$Date: 2000/07/19 01:36:29 $
$Revision: 1.2 $
$Locker:  $
*)

(*$SortCon: TYCON STRID CRASH SORTCON*)

functor SortCon(structure TyCon: TYCON
		structure StrId: STRID
		  sharing type StrId.strid = TyCon.strid
  	        structure Crash: CRASH
	       ): SORTCON =
  struct
    type tycon = TyCon.tycon
    type longtycon = TyCon.longtycon
    type strid = StrId.strid

    datatype sortcon = SORTCON of string

    fun pr_SortCon(SORTCON str) = str

    datatype longsortcon = LONGSORTCON of strid list * sortcon

    fun pr_LongSortCon (LONGSORTCON(strid_list, sortcon)) =
      let
	val string_list = (map (fn s => StrId.pr_StrId s ^ ".") strid_list)

	fun join [] = ""
	  | join (s :: rest) = s ^ join rest
      in
	join string_list ^ pr_SortCon sortcon
      end


    fun implode_LongSortCon (strid_list, sortcon) =
      LONGSORTCON(strid_list, sortcon)

    fun explode_LongSortCon (LONGSORTCON(strid_list, sortcon)) =
      (strid_list, sortcon)

    val sortcon_INT    = SORTCON "int"
    and sortcon_REAL   = SORTCON "real"
    and sortcon_STRING = SORTCON "string"
    and sortcon_EXN    = SORTCON "exn"
    and sortcon_REF    = SORTCON "ref"
    and sortcon_BOOL   = SORTCON "bool"
    and sortcon_LIST   = SORTCON "list"
    and sortcon_INSTREAM = SORTCON "instream"
    and sortcon_OUTSTREAM = SORTCON "outstream"
    and sortcon_UNIT   = SORTCON "unit"


    val mk_SortCon = SORTCON    

    fun mk_LongSortCon ids =
      case rev ids
	of t :: strs =>
	     let
	       val strids = map StrId.mk_StrId (rev strs)
	     in
	       LONGSORTCON(strids, SORTCON t)
	     end

	 | nil => Crash.impossible "SortCon.mk_LongSortCon"

    fun mk_TypeSortCon tycon = mk_SortCon (TyCon.pr_TyCon tycon)

    fun mk_TypeLongSortCon longtycon =
      let 
	val (strid_list, tycon) = TyCon.explode_LongTyCon longtycon
	val sortcon = mk_TypeSortCon tycon
      in
	implode_LongSortCon(strid_list, sortcon)
      end

    fun (SORTCON str1) < (SORTCON str2) = String.< (str1, str2)
  end;
