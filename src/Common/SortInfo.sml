(* Sort information for compiler *)

(*
$File: Common/SortInfo.sml $
$Date: 2001/07/06 08:53:07 $
$Revision: 1.4 $
$Locker:  $
*)

(*$SortInfo: LAB CON EXCON CRASH SORTNAME SORT_INFO *)

functor SortInfo(structure SortName : SORTNAME
		 structure PP: PRETTYPRINT

		 type Sort  (* Refinement Semantic objects Sort and SortVar *)
		 type SortVar
		 val layoutSort  : Sort -> PP.StringTree
		 val layoutSortVar : SortVar -> PP.StringTree
		): SORT_INFO =
  struct
    type Sort = Sort
    type SortName = SortName.SortName
    type SortVar = SortVar

    datatype SortInfo =
	LAB_INFO of {sortvars: SortVar list, Sort : Sort}
      | RECORD_ATPAT_INFO of {Sort : Sort}
      | VAR_INFO of {instances : Sort list}
      | VAR_PAT_INFO of {sortvars: SortVar list, Sort: Sort}
      | CON_INFO of {instances: Sort list, sortvars : SortVar list, Sort: Sort}
      | EXCON_INFO of {Sort: Sort}
      | DATSORTBIND_INFO of {SortName: SortName}
      | EXP_INFO of {Sort: Sort} 
      | MATCH_INFO of {Sort: Sort}
      | PLAINvalbind_INFO of {sortvars: SortVar list, Sort: Sort}

    type StringTree = PP.StringTree
    fun layout_sortvars sortvars = 
      PP.NODE{start="[",finish="]",indent=0,childsep=PP.RIGHT",",
	      children=map layoutSortVar sortvars}

    fun layout info =
      case info
	of LAB_INFO{sortvars, Sort} => 
	  PP.NODE{start="LAB_INFO(", finish=")",indent=2,
		  children=[layout_sortvars sortvars,
			    layoutSort Sort],
		  childsep = PP.RIGHT ","}

         | RECORD_ATPAT_INFO {Sort} =>
	     PP.NODE{start="RECORD_ATPAT_INFO(",finish=")",indent=2,
		  children=[layoutSort Sort],
		  childsep = PP.NOSEP}
         | VAR_INFO {instances} => 
	     PP.NODE{start="VAR_INFO(", finish=")",indent=2,
		     children=map layoutSort instances,
		     childsep = PP.RIGHT ","}
	 | VAR_PAT_INFO {sortvars, Sort} =>
	     PP.NODE{start="VAR_PAT_INFO(",finish=")",indent=2,
		     children=[layout_sortvars sortvars,
			       layoutSort Sort],
		     childsep=PP.RIGHT ","}
	 | CON_INFO{sortvars,Sort,instances} =>
	     PP.NODE{start="CON_INFO(",  finish=")", indent=2,
		     children=[PP.NODE{start="sortvars: ",finish="",
				       indent=4,
				       children=[layout_sortvars sortvars],
				       childsep = PP.NOSEP},
			       PP.NODE{start="Sort: ",finish="",
				       indent=4,
				       children=[layoutSort Sort],
				       childsep = PP.NOSEP},
			       PP.NODE{start="instances: ",finish="",
				       indent=4,
				       children=map layoutSort instances,
				       childsep = PP.RIGHT ","}
			      ],
		     childsep=PP.RIGHT ", "
		    }

         | EXCON_INFO{Sort} =>
	     PP.NODE{start="EXCON_INFO(",finish=")",indent=2,
		     children=[PP.NODE{start="Sort: ",finish="",
				       indent=4,
				       children=[layoutSort Sort],
				       childsep = PP.NOSEP}],
		       childsep = PP.NOSEP}

	 | DATSORTBIND_INFO{SortName} => 
	     PP.LEAF(SortName.pr_SortName ("", SortName))
	 | EXP_INFO{Sort} => 
	     PP.NODE{start="EXP_INFO(",finish="",indent=2,
		     children=[layoutSort Sort],
		     childsep = PP.NOSEP}
	 | MATCH_INFO{Sort} => 
	     PP.NODE{start="MATCH_INFO(",finish="",indent=2,
		     children=[layoutSort Sort],
		     childsep = PP.NOSEP}
	 | PLAINvalbind_INFO{sortvars,Sort} => 
	     PP.NODE{start="PLAINvalbind_INFO(",finish="",indent=2,
		     children=[layout_sortvars sortvars,
			       layoutSort Sort],
		     childsep = PP.NOSEP}
  end;
