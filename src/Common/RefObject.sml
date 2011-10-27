(* Refinement Objects *)

functor RefObject( structure StatObject : STATOBJECT
		   structure ExplicitTyVar: TYVAR
		   structure SortName: SORTNAME
		     where type TyName = StatObject.TyName 
		     where type Variance = ExplicitTyVar.Variance 
		   structure Lab: LAB where type lab = StatObject.lab
		   structure Name : NAME
      	   structure Crash: CRASH
       		   structure FinMap: FINMAP 
       	   structure SortedFinMap: SORTED_FINMAP 
		   structure Flags: FLAGS
		   structure SCon: SCON
		   structure ListHacks: LIST_HACKS
		   structure Report: REPORT
		   structure PP: PRETTYPRINT
		     sharing type PP.Report = Report.Report
(*		     sharing type SortedFinMap.StringTree = PP.StringTree
		     sharing type SortName.StringTree = PP.StringTree *)
		  ):REFOBJECT =
  struct

    structure SO = StatObject  (* abbreviation *)
    structure ExplicitTyVar = ExplicitTyVar

    type TyVar = StatObject.TyVar
    type Type = StatObject.Type
    type TyName = StatObject.TyName
    type TypeFcn = StatObject.TypeFcn
    type Variance = ExplicitTyVar.Variance
    type ('a, 'b) sortedFinMap = ('a, 'b) SortedFinMap.map

    fun debug_pr_msg (msg: unit -> string): unit = ()
      (* if !Flags.DEBUG_REFOBJECTS then TextIO.output(TextIO.stdOut,msg ()) else () *)
    fun debug_refdec (msg: unit -> string): unit = ()
      (* if !Flags.DEBUG_REFDEC then TextIO.output(TextIO.stdOut,msg()) else () *)


    (********
    Sort variables   
       We overload sortvar and tyvar names.  A sort variable must 
       either have an associated type variable, or be bound in a 
       sort-scheme or sort-function.
    ********)

    type BoundSortVar = int

    datatype SortVar = SV of StatObject.TyVar
                     | BOUND_SV of BoundSortVar

    fun equal_SortVar (SV tv1, SV tv2) = StatObject.TyVar.eq (tv1, tv2)
      | equal_SortVar (BOUND_SV id1, BOUND_SV id2) = (id1 = id2)
      | equal_SortVar (_, _) = false


    (********
    Get the unique sortvar refining a tyvar
    ********)
    fun SVofTV tyvar = SV tyvar
    and TVofSV (SV tyvar) = SOME tyvar
      | TVofSV _ = NONE

    fun fresh_tv _ = 
      StatObject.TyVar.fresh_normal () 

    fun fresh_sv _ = SVofTV (fresh_tv ())


    (********
    Sort names
    ********)
    structure SortName = SortName
    type SortName = SortName.SortName

    (********
    Record labels
    ********)

    type lab  = Lab.lab

    (****
    Sorts
    ****)

    datatype RowVar = ROWVAR of int      (* key of Name *)
         and FieldVar = FIELDVAR of int

    (* Records copied from types, but are probably overkill here.  *)
    datatype RecSort = NILrec			(* Rigid. *)
	  	     | VARrec of RowVar	        (* Flexible - not used? *)
      		     | ROWrec of Lab.lab * Field * RecSort

	 and Field = ABSENTfield	  (* Not really necessary here *)
	           | PRESENTfield of Sort
	           | VARfield of FieldVar

         and Sort = SORTVAR of SortVar
		  | ARROW of Sort * Sort
		  | RECSORT of RecSort
		  | CONSSORT of Sort list * SortName.SortName
	          | CONJSORT of Sort * Sort
                  | BOGUSSORT

(* Unfinished.

    fun eqSyn_sort (SORTVAR sv1, SORTVAR sv2) = equal_SortVar (sv1, sv2) 
      | eqSyn_sort (ARROW (srt11, srt12), ARROW (srt21, srt22)) = 
          eqSyn_sort (srt11, srt21) andalso eqSyn_sort (srt12, srt22)
      | eqSyn_sort (RECSORT rs1, RECSORT rs2) = eqSyn_rsort (rs1, rs2)
*)

    type FunSort  = Sort
     and ConsSort = Sort
     and ConjSort = Sort

    fun freshRow() = VARrec(ROWVAR(Name.key (Name.new())))
    and freshField() = VARfield(FIELDVAR(Name.key (Name.new())))

   (* For prettyprinting and the like it's most convenient to be able
      to change a RecSort into a (lab, Field) SortedMap with optional
      RowVar. *)

    fun sanitiseRecSort r
          : (Lab.lab, Field) SortedFinMap.map * RowVar option =
      case r
	of NILrec => (SortedFinMap.empty, NONE)
	 | VARrec rv => (SortedFinMap.empty, SOME rv)
	 | ROWrec(lab, field, r') =>
	     let
	       val (map, rvOpt) = sanitiseRecSort r'
	     in
	       (SortedFinMap.add Lab.< (lab, field, map), rvOpt)
	     end

    fun recSortToMap r = 
      let 
	val m = #1(sanitiseRecSort r)
	fun f (PRESENTfield tau) = tau
	  | f _ = Crash.impossible "RefObject.recSortToMap"

        (*[ val composemap :> (Field -> Sort) -> (Lab.lab, Field) SortedFinMap.map 
	         -> (Lab.lab, Sort) SortedFinMap.map ]*)
        val composemap = SortedFinMap.composemap
      in
	composemap f m
      end

   (* Field insertion. The lab*field list must be ordered over the labs. *)

    fun insertFields(fields: (Lab.lab * Field) list, r): RecSort =
      case (fields, r)
	of (nil, _) => r
	 | ((l, f) :: rest, NILrec) =>
	     ROWrec(l, f, insertFields(rest, NILrec))

	 | ((l, f) :: rest, VARrec v) =>
	     ROWrec(l, f, insertFields(rest, VARrec v))

	 | ((l, f) :: rest, ROWrec(l', f', r')) =>
	     if l = l' then
	       Crash.impossible "RefObject.insertFields"
	     else if Lab.<(l, l') then
	       ROWrec(l, f, insertFields(rest, r))
	     else
	       ROWrec(l', f', insertFields((l, f) :: rest, r'))

    fun addField (lab, ty) recSort =
      insertFields([(lab, PRESENTfield ty)], recSort)

   (* A sort-of compose function: apply a ty->ty to a record. *)

    fun recSort_map (f: Sort -> Sort) (r: RecSort): RecSort =
      case r
	of NILrec => NILrec

	 | VARrec v => VARrec v

	 | ROWrec(lab, field, r') =>
	     ROWrec(lab,
		    case field
		      of ABSENTfield => ABSENTfield
		       | PRESENTfield ty => PRESENTfield(f ty)
		       | VARfield v => VARfield v,
		    recSort_map f r'
		   )

   (* A sort-of fold function: apply a ty*'b->'b along a RecSort,
      yielding a 'b. *)

    fun recSort_fold (f: Sort * 'b -> 'b) (x: 'b) (r: RecSort): 'b =
      case r
	of NILrec => x
	 | VARrec _ => x
	 | ROWrec(lab, field, r') =>
	     case field
	       of ABSENTfield => recSort_fold f x r'
	        | PRESENTfield ty => recSort_fold f (f(ty, x)) r'
	        | VARfield _ => recSort_fold f x r'

    fun recSort_list r = (* SortedFinMap.rangeSORTED (recSortToMap r) *)
	  recSort_fold (fn (sort, t) => sort :: t) [] r

(*
	ListSort.sort  (* This should already be sorted, but sometimes not due to a bug? *)
          (fn (lab1, _) => fn (lab2, _) => (*if lab1 = lab2 then EQUAL 
					else if Lab.< (lab1, lab2) then LESS
					else GREATER *)
					Lab.< (lab1, lab2) )
*)

    (* existsRecVarsSort t returns true iff there exists either a field or a 
       row variable in the sort t *)
    fun existsRecVarsSort s =
      case s of 
	(SORTVAR _) => false
      | (ARROW(s1, s2)) => (existsRecVarsSort s1) orelse (existsRecVarsSort s2)
      | (RECSORT r) => 
	  (case r of
	     NILrec => false
           | VARrec _ => true
           | ROWrec(_, f, r') => 
	       (case f of
		  ABSENTfield => false
		| PRESENTfield sort => existsRecVarsSort sort
		| VARfield _ => true) 
	       orelse 
	       (case r' of
		  NILrec => false
		| VARrec _ => true
		| ROWrec _ => existsRecVarsSort (RECSORT r')
	       )
	  )
      | CONSSORT(sortlist, _) => List.exists existsRecVarsSort sortlist
      | CONJSORT(s1, s2) => (existsRecVarsSort s1) orelse (existsRecVarsSort s2)
      | BOGUSSORT => Crash.impossible "RefObject.existsRecVarsSort"

    (* Find the type that a sort refines.  *)

    fun tyOfSort (SORTVAR(SV tv)) = StatObject.Type.from_TyVar tv
      | tyOfSort (ARROW(s1, s2)) = StatObject.Type.mk_Arrow(tyOfSort s1, tyOfSort s2)
      | tyOfSort (RECSORT rs) = StatObject.Type.from_RecType(tyOfRecSort rs)
      | tyOfSort (CONSSORT (sorts, sortname)) = 
          StatObject.Type.from_ConsType(StatObject.Type.mk_ConsType
				         (map tyOfSort sorts,
				          SortName.tyname sortname)
				       )
      | tyOfSort (CONJSORT (s1, s2)) = tyOfSort s1  (* s2 refines the same type *)
      | tyOfSort BOGUSSORT = StatObject.Type.from_TyVar (fresh_tv())
      | tyOfSort _ = Crash.impossible "RefObject.tyOfSort"

   and tyOfRecSort NILrec = StatObject.Type.RecType.empty
     | tyOfRecSort (VARrec x) = StatObject.Type.RecType.dotdotdot () (* Never used? *)
     | tyOfRecSort (ROWrec (lab, PRESENTfield s, rs)) =
         StatObject.Type.RecType.add_field (lab, tyOfSort s) (tyOfRecSort rs)
     | tyOfRecSort _ = Crash.impossible "RefObject.tyOfRecSort"

    fun compatible_Sort (sort1, sort2) = StatObject.Type.eq (tyOfSort sort1, tyOfSort sort2)

    fun covariant_sort (covariant_sortname : SortName -> bool) sort = 
      let
	fun covariant_sort (SORTVAR(SV tv)) = true
	  | covariant_sort (ARROW(s1, s2)) = false
	  | covariant_sort (RECSORT rs) = covariant_recsort rs
	  | covariant_sort (CONSSORT (sorts, sortname)) = 
	    (List.all covariant_sort sorts) andalso (covariant_sortname sortname)
	  | covariant_sort (CONJSORT (s1, s2)) = 
	    covariant_sort s1  (* s2 refines the same type *)
	  | covariant_sort _ = Crash.impossible "RefObject.covariant_sort"

	and covariant_recsort NILrec = true
	  | covariant_recsort (VARrec x) = true
	  | covariant_recsort (ROWrec (lab, PRESENTfield s, rs)) =
	      (covariant_sort s) andalso (covariant_recsort rs)
	  | covariant_recsort _ = Crash.impossible "RefObject.covariant_recsort"
      in
	covariant_sort sort
      end
							  

    (* Helper functions for conjunctions. *)
    fun firstConjunct (CONJSORT (sort1, sort2)) =
        firstConjunct sort1
      | firstConjunct sort = sort

    fun list_Conjuncts (CONJSORT (sort1, sort2)) = 
         (list_Conjuncts sort1) @ (list_Conjuncts sort2)
      | list_Conjuncts sort = [sort]

    (********
    Sort functions
    ********)

    datatype SortFcn = SORTFCN of {sortvars: (SortVar * Variance) list, sort: Sort}

    (********
    SortSchemes
    *********
    Sort variables must be in the order they appear in the Sort, so that
    we have a cannonical form, and thus equality is correct.
    ********)

    type SortScheme = SortFcn

    (********
    Toplevel printing and PrettyPrinter hooks
    ********)

   (* Overload unbound names with type variables *)

    type TVNames = StatObject.TVNames

    datatype SVNames = 
       NAMES of {sv: int, name:string} list ref * TVNames
     | NONAMES

    fun newSVNames () = NAMES (ref [], StatObject.newTVNames ())
    fun TVNamesOfSVNames (NAMES (_, TVNames)) = TVNames
      | TVNamesOfSVNames NONAMES = Crash.impossible "TVNamesOfSVNames"

    val ordA = Char.ord #"a" (* used below *)


    (* ---Copied from StatObject.sml--- *)
	(* Precedence levels of operators:      tycon : 4
	                                        *     : 3
                                                ->    : 2  
                                                {}(), : 1
		               &  : 0 *)
    fun parenthesize (operator_precedence : int, context_precedence, s : string) =
	if operator_precedence < context_precedence then
	    concat ["(", s, ")"] else s
    (* --- *)


    fun pr_SortVarPRETTY' (NAMES (_, tvNames)) (SV tyvar, _) = 
          StatObject.TyVar.pretty_string tvNames tyvar
      | pr_SortVarPRETTY' NONAMES (SV tyvar, _) = 
          StatObject.TyVar.string tyvar
      | pr_SortVarPRETTY' names (BOUND_SV id, variance) =
          let
	    val boring = (ExplicitTyVar.variancePrefix variance)
	                 ^ "R" ^ Int.toString(~id)
	  in
	    "'" ^ 
	      (case names
		 of NAMES(L as ref L', _) =>
		      if !Flags.DEBUG_SORTVARS then
			boring
		      else
			(case (List.find (fn {sv, ...} => sv = id) L')
                           of SOME {name, ...} => name
			    | NONE =>
			   let
			     val len = List.length L'
			     val name = 
			       (ExplicitTyVar.variancePrefix variance)
			       ^ Char.toString(Char.chr(ordA + len))
			     val _ = 
			       L := L' @ [{sv=id, name=name}]
			   in
			     name
			   end
			)

		  | NONAMES =>
		      boring)
		      
	  end

    fun pr_SortVarPRETTY names sv = pr_SortVarPRETTY' names (sv, ExplicitTyVar.MIXED)

    fun pr_SortVar' sv = pr_SortVarPRETTY' NONAMES sv
    fun pr_SortVar sv = pr_SortVarPRETTY NONAMES sv

    fun pr_RowVar(ROWVAR stamp) = "'r" ^ Int.toString stamp
    fun pr_FieldVar(FIELDVAR stamp) = "'f" ^ Int.toString stamp

    fun pr_SortPRETTY names sort =
	pr_SortPRETTY' names 0 sort

    and pr_SortPRETTY' names precedence sort =
      case sort
	of SORTVAR sv =>
	     pr_SortVarPRETTY names sv

	 | RECSORT r =>		(* See if we can print it as `a * b * ...'
				   rather than `{1: a, 2: b, ...}' *)
	     let
	       val (m, rv) = sanitiseRecSort r
	     in
	       case (SortedFinMap.matches
		       (fn (i, lab) => Lab.is_LabN(lab, i+1)) m,
		     rv
		    )
		 of (true, NONE) =>	(* A possible (t1 * t2 * ...) sort,
					   and no rowvar. *)
		   	(* Careful: "{1=x}" does *not* print as "(x)" (MacQueen
			   got this one wrong), and "{ }" should be "unit". We
			   don't do this folding at all if there's a row var. *)
		      let
			val fields = SortedFinMap.rangeSORTED m
		      in
			case fields
			  of nil => "unit"	(* Hard-wired *)
			   | [x] => "{1: " ^ pr_FieldPRETTY names x ^ "}"
			   | _ =>
				 parenthesize
				 (3, precedence, 
				  ListHacks.stringSep "" "" " * "
					      (pr_FieldPRETTY' names 4) fields)
		      end

		  | _ =>		(* Have to do the general print. *)
		      let
			val finish =
			  case rv of SOME v => " ... " ^ pr_RowVar v ^ "}"
			           | NONE => "}"
		      in
			PP.flatten1(
			  SortedFinMap.layoutMap
			    {start="{", eq=": ", sep=", ", finish=finish}
			    (PP.layoutAtom Lab.pr_Lab)
			    (PP.layoutAtom(pr_FieldPRETTY names))
			    m
			)
		      end
	     end

	 | ARROW(s1, s2) =>
	       parenthesize
	       (2, precedence,
		pr_SortPRETTY' names 3 s1
		^ " -> "
		^ pr_SortPRETTY' names 2 s2)

	 | CONSSORT(sorts, sortname) =>
	     let 
	       val prefix = 
		 case sorts
		   of nil => ""
(*		    | [sort] => "(" ^ (pr_SortPRETTY names sort) ^ ") " *)
		    | [sort] => pr_SortPRETTY' names 4 sort ^ " "
		    | _ =>
			  ListHacks.stringSep "(" ") " ", " (pr_SortPRETTY' names 1) sorts
	       val str = SortName.pr_SortName (prefix, sortname)
	     in
	       if length (SortName.conjuncts sortname) > 1 andalso not (SortName.isEmpty sortname) 
	         then parenthesize (0, precedence, str)
	         else str
	     end

	 | CONJSORT(s1, s2) =>
	       parenthesize
	       (0, precedence,
		 pr_SortPRETTY' names 0 s1
		 ^ " & "
		 ^ pr_SortPRETTY' names 0 s2)

	 | BOGUSSORT => "<invalid>"

    and pr_FieldPRETTY names field = pr_FieldPRETTY' names 0 field

    and pr_FieldPRETTY' names precedence field =
      case field
	of ABSENTfield => "<absent>"
	 | PRESENTfield sort => pr_SortPRETTY' names precedence sort
	 | VARfield fv => pr_FieldVar fv

    val pr_Sort = pr_SortPRETTY NONAMES

    fun pr_SortSchemePRETTY names (SORTFCN{sort, ...}) = pr_SortPRETTY names sort

    fun pr_SortScheme (SORTFCN{sort, ...}) = pr_Sort sort

    fun pr_SortFcnPRETTY names (SORTFCN{sortvars, sort}) =
      case sortvars
	of nil =>
	     {vars="", body=pr_SortPRETTY names sort}

	 | [sortvar] =>
	     {vars=pr_SortVarPRETTY' names sortvar, 
	      body=pr_SortPRETTY names sort}

	 | _ =>
	     {vars=ListHacks.stringSep "(" ")" ", " (pr_SortVarPRETTY' names) sortvars,
	      body=pr_SortPRETTY names sort
	     }


    fun layoutSortName sortname = StringTree.LEAF(SortName.pr_SortName ("", sortname))
    and layoutSortVar tv = StringTree.LEAF(pr_SortVar tv)


    and layoutSort(CONSSORT([], sortname)) =
	  layoutSortName sortname

      | layoutSort(CONSSORT(sort_list, sortname)) = 
	StringTree.NODE{start="(", finish=") (" ^ (SortName.pr_SortName ("", sortname)) ^ ")", 
		indent=1,
		children=(map layoutSort sort_list),
		childsep=StringTree.LEFT ", "
		}

      | layoutSort(RECSORT r) =
	  let
	    val (m, rv_opt) = sanitiseRecSort r

	    val finish = case rv_opt of SOME rv => " ... " ^ pr_RowVar rv ^ "}"
	      			      | NONE => "}"
	  in
	    SortedFinMap.layoutMap
	      {start="{", eq=" : ", sep=", ", finish=finish}
	      (PP.layoutAtom Lab.pr_Lab) layoutField m
	  end

      | layoutSort(ARROW(sort, sort')) =
	  StringTree.NODE{start="(", finish=")", indent=1,
		  children=[layoutSort sort, layoutSort sort'],
		  childsep=StringTree.LEFT " -> "
		 }

      | layoutSort(SORTVAR sv) = layoutSortVar sv

      | layoutSort(CONJSORT(sort, sort')) =
	  StringTree.NODE{start="(", finish=")", indent=1,
		  children=[layoutSort sort, layoutSort sort'],
		  childsep=StringTree.LEFT " & "
		 }
      | layoutSort BOGUSSORT = StringTree.LEAF "<invalid>"

    and layoutField ABSENTfield = StringTree.LEAF "<absent>"
      | layoutField(PRESENTfield sort) = layoutSort sort
      | layoutField(VARfield fv) = PP.layoutAtom pr_FieldVar fv

    fun layoutSortFcn(SORTFCN {sortvars, sort}) =
      StringTree.NODE{start=ListHacks.stringSep "LAMBDA (" "). " ", " pr_SortVar' sortvars,
	      finish="", indent=0, childsep=StringTree.NOSEP,
	      children=[layoutSort sort]
	     }

    fun layoutSortScheme(SORTFCN{sort, ...}) = layoutSort sort

    fun layoutSortNameSet sortname_list =
      StringTree.NODE{start="{", finish = "}", indent=1,
	      children=(map layoutSortName sortname_list),
	      childsep=StringTree.LEFT ", "
	      }

    (********
    Find free sort variables
    ********)

(*  fun union_eq eq (l1, l2) =
      l1 @ (List.filter (fn x2 => List.all (fn x1 => not (eq (x1, x2))) l1)
	             l2)
*)
    fun union_eq eq (l1, l2) =
      l1 @ (List.filter (fn x2 => List.all (fn x1 => not (eq (x1, x2))) l1)
	             l2)

    fun minus_eq eq (l1, l2) =
      List.filter (fn x1 => List.all (fn x2 => not (eq (x1, x2))) l2) l1

    (* Sortvars in the reverse order from StatObject.TyVar.tyvars.   *)
    fun sortvarsSort' (SORTVAR sortvar) = [sortvar]
      | sortvarsSort' (RECSORT r) =
	  recSort_fold (fn (sort, sortvars) => union_eq 
	                                         equal_SortVar
				                 (sortvarsSort' sort, sortvars)
		       )
	  	       nil r
      | sortvarsSort' (ARROW(sort,sort')) =
	  union_eq equal_SortVar (sortvarsSort' sort', sortvarsSort' sort)
      | sortvarsSort' (CONSSORT(sorts,_)) = 
	  List.foldr
	  (fn (sort, sortvars) => union_eq 
	                               equal_SortVar
	                               (sortvarsSort' sort, sortvars)
          )
	  [] sorts
      | sortvarsSort' (CONJSORT(sort,sort')) =
	  sortvarsSort' sort  (* Must be the same in both conjuncts.  *)
      | sortvarsSort' BOGUSSORT = []

    (* FIXED: uses StatObject.tyvars and StatObject.generic_tyvars ensuring that
       the ordering matches that during elaboration.  The previous code is used
       as a last resort.  *)
    fun sortvarsSort (sort : Sort) : SortVar list = 
        map SVofTV (StatObject.Type.tyvars (tyOfSort sort))
        (* handle _ =>  ListHacks.reverse (sortvarsSort' sort)  (* For robustness *)
                        before print "POSSIBLE BUG: Exception in RefObject.sortvarsSort.\n" *)

    fun sortvarsSortSch (SORTFCN{sortvars, sort}) =
      minus_eq (fn (sv1, (sv2, _)) => equal_SortVar (sv1, sv2))
               (sortvarsSort sort, sortvars)


    (********
    Construct and destruct sorts
    ********)

    fun mkSortSortVar sv = SORTVAR sv

    fun unSortSortVar(SORTVAR tv) = SOME(tv)
      | unSortSortVar _ = NONE

    val mkSortRecSort = RECSORT

    fun unSortRecSort(RECSORT t) = SOME t
      | unSortRecSort _ = NONE

    fun mkSortFunSort fsort = fsort
    and unSortFunSort(sort as (ARROW(sort',sort''))) = SOME(sort)
      | unSortFunSort(_) = NONE

    fun mkSortConsSort csort = csort
    and unSortConsSort(sort as (CONSSORT(sorts, sortname))) = SOME(sort)
      | unSortConsSort _ = NONE

    fun mkSortConjSort fsort = fsort
    and unSortConjSort(sort as (CONJSORT(sort',sort''))) = SOME(sort)
      | unSortConjSort(_) = NONE

    val bogusSort = BOGUSSORT
    fun isBogusSort BOGUSSORT = true
      | isBogusSort _ = false

    (********
    Construct and destruct record sorts
    ********)

    val emptyRecSort = NILrec		(* "{}" *)
    val emptyFlexRecSort = freshRow	(* "{...}" *)

    local
      val ONE = Lab.mk_IntegerLab 1
      and TWO = Lab.mk_IntegerLab 2
      and THREE = Lab.mk_IntegerLab 3
    in
      fun mkSortPair (sort,sort') =
	RECSORT(addField (ONE, sort) (addField (TWO, sort') emptyRecSort))

      and mkSortTriple (sort1,sort2,sort3) =
	RECSORT(addField (ONE, sort1) (addField (TWO, sort2) 
                                        (addField (THREE, sort3) emptyRecSort)))

      and unRecPair r =
	    case sanitiseRecSort r
	      of (m, NONE) =>
		   (case SortedFinMap.lookup m ONE
		      of NONE =>
			   Crash.impossible "RefObject.unRecPair(L=?)"

		       | SOME ABSENTfield =>
			   Crash.impossible "RefObject.unRecPair(L=a)"

		       | SOME(VARfield _) =>
			   Crash.impossible "RefObject.unRecPair(L=v)"

		       | SOME(PRESENTfield sortL) =>
			   (case (SortedFinMap.lookup m TWO)
			      of NONE =>
				   Crash.impossible "RefObject.unRecPair(R=?)"

			       | SOME ABSENTfield =>
				   Crash.impossible "RefObject.unRecPair(R=a)"

			       | SOME(VARfield _) =>
				   Crash.impossible "RefObject.unRecPair(R=v)"

			       | SOME(PRESENTfield sortR) => (sortL, sortR)
			   )
		   )

	       | (_, SOME _) =>		(* It's flexible: punt *)
		   Crash.impossible "RefObject.unRecPair(flexible)"
    end

    fun unRecSortSorted r = 
      let
	val m = #1(sanitiseRecSort r)
	fun f (PRESENTfield tau) = tau
	  | f _ = Crash.impossible "RefObject.unRecSort"
      in
	ListPair.zip (SortedFinMap.domSORTED m,
		      map f (SortedFinMap.rangeSORTED m))
      end

    fun sortedLabsOfRecSort r =
          (case sanitiseRecSort r
	     of (m, _) => SortedFinMap.domSORTED m
	  )

    val SortUnit = RECSORT emptyRecSort

    (********
    Construct and destruct function sorts
    ********)

    fun mkFunSort(sort,sort') = ARROW(sort,sort')

    and unFunSort(ARROW(sort,sort')) = SOME(sort,sort')
      | unFunSort(BOGUSSORT) = SOME (BOGUSSORT, BOGUSSORT)
      | unFunSort(_) = NONE

    (********
    Construct and destruct constructed sorts
    ********)

    fun mkConsSort(sortl, name) = CONSSORT(sortl,name)

    and unConsSort(CONSSORT(sortl,name)) = SOME(sortl,name)
      | unConsSort(_) = NONE

    (********
    Construct and destruct conjunction sorts
    ********)

    (* Helper function to syntactically compare two refinements *)
    fun sntx_eqsrt(SORTVAR _, _) = true (* optimization *)
      | sntx_eqsrt(ARROW (srt11, srt12), ARROW(srt21, srt22)) = 
        (sntx_eqsrt(srt11, srt21)) andalso (sntx_eqsrt(srt12, srt22))
      | sntx_eqsrt(RECSORT r1, RECSORT r2) = sntx_eqrsrt(r1, r2)
      | sntx_eqsrt(CONSSORT(srts1, sn1), CONSSORT(srts2, sn2)) = 
	(SortName.eq(sn1, sn2)) andalso (List.all sntx_eqsrt (ListPair.zip (srts1, srts2)))
      | sntx_eqsrt(CONJSORT(srt11, srt12), CONJSORT(srt21, srt22)) = 
        (sntx_eqsrt(srt11, srt21)) andalso (sntx_eqsrt(srt12, srt22))
      | sntx_eqsrt _ = false
    and sntx_eqrsrt(NILrec, NILrec) = true
      | sntx_eqrsrt(ROWrec(_, PRESENTfield sort1, r1), ROWrec(_, PRESENTfield sort2, r2)) =
        sntx_eqsrt(sort1, sort2) andalso sntx_eqrsrt(r1, r2)
      | sntx_eqrsrt _ = Crash.impossible "RefObject.sntx_eqrsrt"     

    (* Use this with caution! *)
    fun mkSyntacticConjSort(sort,sort') = CONJSORT(sort,sort')

    (* mkConjSort creates a conjunction.  It uses "conjFun" to intersect sortnames, creating
       syntactic intersections only if conjFun returns NONE (indicating "not created") or the
       result sortname is not covariant (in which case each conjunct will have this sortname, but
       different parameters).  The only other place that syntactic intersections can occur is
       between function sorts: intersections are commuted into record sorts, and are never needed
       for sortvars (since tyvars have a unique refining sortvar). *)
    fun mkConjSort (conjFun: SortName * SortName ->SortName option) 
                   (sort, sort') = 
      case (firstConjunct sort, sort')
	of (SORTVAR _, _) => sort      (* Only refinement *)
         | (ARROW _, _) => CONJSORT(sort, sort')
         | (RECSORT r, RECSORT r') => RECSORT (mkConjRecSort conjFun (r, r'))
         | (CONSSORT _, _) => mkConjConsSort conjFun (sort, sort')
         | (BOGUSSORT, _) => BOGUSSORT
         |  _ => Crash.impossible "RefObject.mkConjSort(2)"

    and mkConjRecSort (conjFun: SortName * SortName -> SortName option)
		      (ROWrec(lab, PRESENTfield sort, r), 
		       ROWrec(lab', PRESENTfield sort', r')) =
        if (lab<>lab') then Crash.impossible "RefObject.mkConjRecSort(1)" else
	  ROWrec(lab, PRESENTfield (mkConjSort conjFun (sort, sort')),
		      mkConjRecSort conjFun (r, r'))
      | mkConjRecSort conjFun (NILrec, NILrec) = NILrec
      | mkConjRecSort _ _ = Crash.impossible "RefObject.mkConjRecSort(2)"

    and mkConjConsSort (conjFun: SortName * SortName -> SortName option)
		       (sort  as CONSSORT (sorts, sortname), 
			sort' as CONSSORT (sorts', sortname')) =
	(case conjFun(sortname, sortname')
	   of NONE => 
	      (debug_pr_msg 
	        (fn () => "mkConjConsSort: syntactic "
	         ^ SortName.pr_SortName ("", sortname)
                 ^ " & " ^ SortName.pr_SortName ("", sortname') ^ "\n");
	       CONJSORT(sort, sort')
              )
	    | SOME (conj_sortname: SortName) => 
	      case ListPair.zip(sorts, sorts') of sort_pairs =>
	      if SortName.allVars_Covariant(conj_sortname) orelse 
                 List.all sntx_eqsrt sort_pairs		   
              then
		   let
		     val conj_sorts = map (fn (s1, s2) => mkConjSort conjFun (s1, s2)) sort_pairs
		   in
		     CONSSORT(conj_sorts, conj_sortname)		       
		   end
	      else
	         CONJSORT(CONSSORT(sorts, conj_sortname), CONSSORT(sorts', conj_sortname))
	)
      | mkConjConsSort conjFun (sort, sort') =
	let
	  val conjuncts = (list_Conjuncts sort) @ (list_Conjuncts sort')
          exception CANT_CONJOIN
	  fun conjFold (sortname, b_sortname) =
                (case conjFun(sortname, b_sortname) 
                  of SOME res_sortname => res_sortname
                   | NONE => raise CANT_CONJOIN)
	  val new_conjuncts = 
	      let 
		val sortnames = map (fn CONSSORT (_, sortname) => sortname 
                                      | BOGUSSORT => raise CANT_CONJOIN  (* FIX? - 24jan02 *)
		 		      | _ => Crash.impossible "RefObject.mkConjConsSort(1)")
	                            conjuncts
		val conj_sortname = ListHacks.foldl' conjFold sortnames
	      in
		map (fn CONSSORT(sorts, sortname) => CONSSORT(sorts, conj_sortname)
                      | _ => Crash.impossible "RefObject.mkConjConsSort(2)")
	            conjuncts
	      end 
	      handle CANT_CONJOIN => conjuncts
	in
	  ListHacks.foldl' (fn (sort, sort') => CONJSORT(sort, sort'))
	                   new_conjuncts
	end
	                 
    and unConjSort(CONJSORT(sort,sort')) = SOME(sort,sort')
      | unConjSort(_) = NONE

    (********
    Derived constructors and destructors
    ********)

    fun mkSortRef t = CONSSORT([t], SortName.sortName_REF)

    val SortExn = CONSSORT([], SortName.sortName_EXN)

    fun isSortExn(CONSSORT([], name)) = (SortName.eq(name,SortName.sortName_EXN))
      | isSortExn _ = false

    val mkSortArrow = ARROW

    fun isSortArrow(ARROW _) = true
      | isSortArrow _ = false

    fun unSortArrow(ARROW(t, t')) = SOME(t, t')
      | unSortArrow(BOGUSSORT) = SOME (BOGUSSORT, BOGUSSORT)
      | unSortArrow _ = NONE

    fun isSortCons(CONSSORT _) = true
      | isSortCons _ = false

    val mkSortCons = mkConsSort
    val unSortCons = unConsSort
    

    val mkSortConj = mkConjSort

    fun isSortConj(CONJSORT _) = true
      | isSortConj _ = false

    fun unSortConj(CONJSORT(t, t')) = SOME(t, t')
      | unSortConj _ = NONE



    (**********
     The ML sort corresponding to a type.
     **********)  
    fun MLSortOfTy (TNtoSN : TyName -> SortName) (ty : Type) : Sort = 
                case (StatObject.Type.to_TyVar ty) of SOME tv => mkSortSortVar(SVofTV tv)
      | NONE => case (StatObject.Type.un_Arrow ty) of SOME (ty1, ty2) => 
	        mkSortArrow (MLSortOfTy TNtoSN ty1, MLSortOfTy TNtoSN ty2)
      | NONE => case (StatObject.Type.to_RecType ty) of SOME rectype =>
                mkSortRecSort(List.foldl (fn ((lab, ty), acc) => 
					    addField (lab, MLSortOfTy TNtoSN ty) acc)
                                         NILrec
                                         (StatObject.Type.RecType.to_list rectype))
      | NONE => case (StatObject.Type.to_ConsType ty) of SOME consty =>
                (case (StatObject.Type.un_ConsType consty) 
		   of SOME (tys, tyname) =>
		      mkSortCons(map (MLSortOfTy TNtoSN) tys, TNtoSN tyname)
                    | NONE => Crash.impossible "RefObject.MLSortOfTy(1)")
      | NONE => Crash.impossible "RefObject.MLSortOfTy(2)"


    (* Types for conjunction functions - see signature *)
    type conjSN = SortName * SortName -> SortName
     and conjSN_opt = SortName * SortName -> SortName option

    val out_debug_ssn = ref (fn s => print (s ()))
			    (*(fn x:unit -> string => ()) *) (* updated from RefinedEnvironments *)

    val debug_push_ref = ref (fn s => (map print (s ()); ()))
    val debug_push_must_ref = ref (fn s => (map print (s ()); ()))
    val debug_pop_ref = ref (fn s => (map print (s ()); ()))
    val assert_ref = ref ((fn _ => ()) : (unit -> string list option) -> unit)

    val debug_funs_do_nothing = true (* false *)
    fun debug_push str = if debug_funs_do_nothing then () else !debug_push_ref str
    fun debug_pop str = if debug_funs_do_nothing then () else !debug_pop_ref str
    fun debug_push_must str = if debug_funs_do_nothing then () else !debug_push_must_ref str
    fun assert str = !assert_ref str


(*    fun pr_Sort _ = ""
      fun pr_SortPRETTY _ _ = ""
      fun pr_SortFcnPRETTY _ _ = {body="", vars = ""} *)

    (**********
     Apply a refinement of a PURE function type to another sort, via distributivity.
     This is only appropriate for the sorts of constructors.
     **********)  
    fun applySort' (subSN, conjSN) (ARROW(sort11, sort12), sort2) : Sort option = 
             if subSortList (subSN, conjSN) (sort2, [sort11]) then (SOME sort12) else NONE
      | applySort' (subSN, conjSN) (CONJSORT(sort11, sort12), sort2) =
             (case (applySort' (subSN, conjSN) (sort11, sort2), 
		    applySort' (subSN, conjSN) (sort12, sort2))
                of (NONE, rsort_opt) => rsort_opt
                 | (rsort_opt, NONE) => rsort_opt
                 | (SOME rsort1, SOME rsort2) => 
		   SOME (mkConjSort conjSN (rsort1, rsort2)))
      | applySort' _ _ = Crash.impossible "RefObject.applySort"

	   	   
    (**********
     Subsorting - assume that the sorts refine the same type.  
     **********)

    (* Compare a sort with a list of disjuncts.  *)
    and subSortList (subSNconjSN as (subSortName : SortName list * SortName list list -> bool,
                                     conjSortName : SortName * SortName -> SortName option))
                    (srt1, srts2)
                  : bool =
       let
         val _ = debug_refdec (fn () => "subSortList: " ^ pr_Sort srt1 ^ " <= " ^ 
			        (List.foldr (fn (srt, acc) => (pr_Sort srt) ^ " | " ^ acc)
				            "" srts2)
                               ^ "\n")
         val zip = ListPair.zip
         val mkConj = mkSortConj conjSortName
         fun all_pairs_disjoint [] = true
           | all_pairs_disjoint (h::t) = 
	       List.all (fn x => sub (mkConj (h, x), [])) t
	       andalso all_pairs_disjoint t
         and all_pairs_equal [] = true   (* This could call sub instead.  *)
           | all_pairs_equal (h::t) = List.all (fn x => sntx_eqsrt (h, x)) t

         and cduce_decomp (sort1::tsorts1, sorts_list2) = 
	     let val _ = debug_push (fn () => ["CDuce record expansion.  recordsize = " ^ 
						  Int.toString (length tsorts1 + 1) ^ 
						  "   union components = " ^ 
						  (Int.toString (length sorts_list2))])
		 val head_sorts2 = map hd sorts_list2
		 val result = 
		     sub (sort1, head_sorts2)  (* We've already checked the empty cases. *)
		     andalso List.all (fn (hs2::tsorts2) => 
					  sub(mkConj (sort1, hs2), []) orelse 
					  subRecSort0(tsorts1,[tsorts2]) )
		                      sorts_list2
		 val _ = debug_pop (fn () => ["CDuce record expan. END  recordsize = " ^ 
						  Int.toString (length tsorts1 + 1) ^ 
						  "   union components = " ^ 
						  (Int.toString (length sorts_list2)) 
						  ^ "  " ^ (Bool.toString result) ])

             in result
	     end handle Empty => (print "cduce_decomp\n"; raise Empty)
	 and subRecSort0 ([] : Sort list, [] : Sort list list) = false  (* unit < empty is false. *)
	   | subRecSort0 ([], _) = true   (* unit < anything-else is true: no other refinements. *)
	   | subRecSort0 (sorts1, []) = false (* already checked *)
					      (* List.exists (fn s1 => sub (s1, [])) sorts1 *)
	   | subRecSort0 (sort1::sorts1, sorts_list2) =
            let val head_sorts2 = map hd sorts_list2  in
	       if all_pairs_equal head_sorts2 orelse length sorts1 = 0 then
                   ((*(!out_debug_ssn) (fn () => "All equal record expansion.  \trecordsize = " ^ 
				        Int.toString (length sorts1 + 1) ^ 
				        "   \tunion components = " ^ 
					(Int.toString (length sorts_list2)) ^ "\n"  ); *)
		   sub (sort1, [hd head_sorts2]) 
		   andalso subRecSort0 (sorts1, map tl sorts_list2))
	       else if all_pairs_disjoint head_sorts2 then   (* This optimization adds 10% or so *)
		   cduce_decomp (sort1::sorts1, sorts_list2) (* for at least some examples. *)
	       else
                   (let fun srtsleq (srts1, srts2) = List.all (fn (s1, s2) => sub(s1, [s2]))
							      (ListPair.zip (srts1, srts2))
                        fun maxSortsList srts_list = foldl (ListHacks.addMax srtsleq) [] srts_list

                        val sorts_list22 = maxSortsList sorts_list2
                        fun pr_sort srt = pr_SortPRETTY (newSVNames()) srt
                        fun pr_sorts srts = 
			    ListHacks.stringSep "" "" "\t* " pr_sort srts
                        fun pr_sorts_list srts_list = map pr_sorts srts_list

(*	                val _ = debug_push_must 
				    (fn () =>("Exponential record expansion.     recordsize = " ^ 
					      Int.toString (length sorts1 + 1) ^ 
					      "   union components = " ^ 
					      (Int.toString (length sorts_list22))) ::
                                             (pr_sorts (sort1::sorts1)) ::
                                             pr_sorts_list sorts_list22 ) *)

			val result = subRecSort' (sort1, sorts1, sorts_list22, [], [])
(*			val _ = debug_pop 
				    (fn () =>["Exponential record expansion:END  recordsize = " ^ 
					      Int.toString (length sorts1 + 1) ^ 
					      "   union components = " ^ 
					      (Int.toString (length sorts_list22)) 
					      ^ "  " ^ Bool.toString result ]) *)
		    in  result
		    end
		   )
	    end  handle Empty => (print "subRecSort0\n"; raise Empty)

         and subRecSort' (sort: Sort, r1 : Sort list, rs2 : Sort list list, 
			  accums : Sort list, accumr : Sort list list) =
	   case rs2
	     of [] => (case accums of [] => false | _ => sub (sort, accums))
                      orelse (case accumr of [] => false | _ => subRecSort0 (r1, accumr))
				   (* [] checked in subRecSort *)
	      | (sort'::r')::rt2 => 
                  (* subRecSort' would (need to) have a critical precondition if we add
                     something like the following. *)
                  sub (sort, accums) orelse subRecSort0 (r1, accumr) orelse
                  (* sub (sort, sort'::accums) andalso subRecSort0 (r1, r'::accumr)
                  orelse  (* Checking inclusion first is an optimization.  *) *)
		  ( (* (sub (sort, (map hd rt2) @ sort'::accums)  orelse  (* Unoptimization. *)
                        subRecSort0 (r1, (map tl rt2)@r'::accumr)  ) andalso *)
                   subRecSort' (sort, r1, rt2, sort'::accums, accumr) andalso
		   subRecSort' (sort, r1, rt2, accums, r'::accumr) )
              | []::_ => Crash.impossible "RefObject.subRecSort'"

         and subRecSort (sorts1, sorts_list2) = 
             (* First check some common cases, which may greatly speed things up. *)
            ((* (!out_debug_ssn) (fn () => "subRecSort.  \trecordsize = " ^ 
				        Int.toString (length sorts1) ^ 
				        "   \tunion components = " ^ 
					(Int.toString (length sorts_list2)) ^ "\n"  ); *)
             (List.exists (fn srt1 => sub(srt1,[])) sorts1) orelse
           case (List.filter (fn sorts2 => not (List.exists (fn srt2 => sub(srt2, [])) sorts2))
		               sorts_list2) 
                of sorts_list2 =>
             (List.exists (fn srts2 => List.all (fn (srt1, srt2) => sub(srt1, [srt2])) 
		 	                           (ListPair.zip(sorts1, srts2)))
	                  sorts_list2 ) orelse
             (case sorts_list2 of _::_::_ => subRecSort0 (sorts1, sorts_list2)
		                | _ => false)  (* [], [srts2] handled by the above *)
	    )
	 and subConsArgs ([] : Sort list, [] : Sort list list) = false
	   | subConsArgs ([], _) = true
	   | subConsArgs (sort1::sorts1, sorts_list2) =
	     subConsArgs' (sort1, sorts1, sorts_list2, [], [])
         and subConsArgs' (sort: Sort, r1 : Sort list, rs2 : Sort list list, 
			  accums : Sort list, accumr : Sort list list) =
           let val _ = debug_refdec (fn () => "subConsArgs' \n")
               val result =
  	         case (rs2, accums)
		   of ([], []) => subConsArgs (r1, accumr)  (* Might not be strict *)
		    | ([], _) => sub (sort, accums) orelse subConsArgs (r1, accumr)
		    | ((sort'::r')::rt2, _) => 
		       subConsArgs' (sort, r1, rt2, sort'::accums, accumr) andalso
		       subConsArgs' (sort, r1, rt2, accums, r'::accumr)
		    | ([]::_,_) => Crash.impossible "RefObject.subConsArgs'"
               val _ = debug_refdec (fn () => "subConsArgs':END \n")
           in 
	       result
	   end

             (* subConsSort' checks subgoals similar to those for a union of products. *)
             (* It isn't used anymore because this approach doesn't work. *)
(*         and subConsSort' (sr1 as (sortnames1 : SortName list, r1 : Sort list), 
			   rs2 : (SortName list * Sort list) list, 
			   accums : SortName list list, accumr : Sort list list) =
           let val _ = debug_refdec (fn () => "subConsSort' \n")
               val result =
		   case rs2
		     of [] => subConsArgs (r1, accumr) orelse subSortName (sortnames1, accums)
		      | (sortnames2, r')::rt2 => 
			   subConsSort' (sr1, rt2, sortnames2::accums, accumr) andalso
			   subConsSort' (sr1, rt2, accums, r'::accumr)
               val _ = debug_refdec (fn () => "subConsSort':END \n")
           in 
	       result
	   end
*)

	 and sub (sort1, sorts2:Sort list) =   (* Conjunctions are only allowed at functions and *)
           let                         
	       fun pr_sort srt = pr_SortPRETTY (newSVNames()) srt
               fun pr_sorts srts = 
			    ListHacks.stringSep "" "" "\t |  " pr_sort srts
               val _ =  debug_push
                           (fn () => "sub START: " :: [pr_sort sort1 ^ "  <  " ^ pr_sorts sorts2])

               fun usorts2 () = foldl (fn (x, acc) => if List.exists (fn y => sntx_eqsrt (x, y)) acc
						      then acc
						      else x:: acc)
				      []
				      sorts2
               val sub_result = 

           (* The following line was critical before optimizing subRecSort'. *)
           List.exists (fn x => sntx_eqsrt (sort1, x)) sorts2 orelse
	   case (firstConjunct sort1, sorts2)       (* constructors.  (See mkConjSort) *)
             of (BOGUSSORT, _) => true       (* Error caught elsewhere *)
              | (_, [BOGUSSORT]) => true
	      | (SORTVAR _, []) => false
	      | (SORTVAR _, _) => true       (* There's only one refinement of a type variable. *)
              | (RECSORT r1, _) =>
                subRecSort (recSort_list r1, (* Order by labels. *)
	 		    map (fn RECSORT r2 => recSort_list r2 
	                          | srtErr => (assert (fn () => SOME ["BUG: RefObject.sub: " ^ pr_Sort sort1
                                                                ^ ", " ^ pr_Sort srtErr ^ "\n"]);
					       recSort_list r1) )
			        (usorts2())
		           )
              | (ARROW _, _) =>
                List.exists (fn sort2 =>
                   List.all (fn ARROW(s21, s22) =>
(*  The following line is the zero-ary case of a distributivity rule, and isn't so useful. *)
(*                      sub(s21, []) orelse         (* special case: s11 -> s12 < empty -> s22  *)*)
                      List.exists (fn ARROW(s11, s12) =>
                           (sub(s21, [s11]) andalso sub(s12, [s22]))  )
                         (list_Conjuncts sort1)  )
                    (list_Conjuncts sort2)  )
                  (usorts2())

              | (CONSSORT (_, sortname11), _) =>
	        let
		  val sorts2 = usorts2()
                  fun snames (CONJSORT (sort1, sort2)) =  (* Reduce to a single lattice element *)
                        (case (snames sort1, snames sort2) of            (*  in the simple case *)
                             ([sn1], [sn2]) => (case conjSortName (sn1, sn2) of
                                                  SOME sn => [sn]
                                                | NONE => [sn1, sn2])
                           | (sns1, sns2) => sns1 @ sns2)
                    | snames (CONSSORT (_, sn)) = [sn]
                    | snames srt = 
                     (assert (fn () => SOME
		        ["BUG: RefObject.sub:CONSSORT.snames: " ^ (pr_Sort srt)
                          ^ "  sort1= " ^ (pr_Sort sort1) ^ "  sorts2= "
                          ^ (foldl (fn (s2, acc) => acc ^ " , " ^ (pr_Sort s2)) "" sorts2)]);
                      [sortname11] (* dummy value *)
                      )

                  val sns1 = snames sort1
                  val variances1 = 
                      ListHacks.foldl' (ListPair.map ExplicitTyVar.join_variance) 
                                       (map SortName.variance sns1)

                  (*[ val removeIgnored : Variance list -> 'a list -> 'a list ]*)
		  fun removeIgnored [] [] = []
		    | removeIgnored (ExplicitTyVar.IGNORED::vs) (h::t) = removeIgnored vs t
		    | removeIgnored (_::vs) (h::t) = h :: (removeIgnored vs t)

	          fun argsConj sort : Sort list  = 
		    let 
                      fun nonIgnored (CONSSORT (sorts, _)) = 
			     removeIgnored variances1 sorts
		      val sorts_list = map nonIgnored (list_Conjuncts sort)
			  handle Match => map nonIgnored (list_Conjuncts sort1)
		    in
		      ListHacks.foldl' (ListPair.map (mkSortConj conjSortName))
		                       sorts_list		      
		    end
                  fun snames_argsConj sort = (snames sort, argsConj sort)

	        in
	          if (SortName.allVars_Covariant sortname11) then  (* Intersect parameters. *)
                    (* subConsSort' ((sns1, argsConj sort1), map snames_argsConj sorts2, [], []) *)
                    
                    (* Find all the parts of the RHS union that have larger parameters
                       than argsConj in all covariant positions.  Then, we check sortName
                       inclusion in the union for those parts of the RHS.  *)
		    let val args1 = argsConj sort1
			fun largerParams (_, args2) = ListPair.all (fn (s1, s2) => sub (s1, [s2])) 
			                                           (args1, args2)
                        val sns2_filt_list = map #1 (List.filter largerParams 
						                 (map snames_argsConj sorts2) )
		    in
                        subSortName (sns1, sns2_filt_list)
		    end

		  else
                        subSortName( sns1, []) orelse (* An empty sort constructor.  *)
                        List.exists (fn sort2 =>  (* Based on the code for arrow. *)
                          subSortName(sns1, [snames sort2]) andalso
                          List.all (fn CONSSORT(args2, _) =>
                             List.exists (fn CONSSORT(args1, _) =>
                               subVariance(variances1, args1, args2)   )
                            (list_Conjuncts sort1)  )
                          (list_Conjuncts sort2)  )
                        sorts2
                end
              | _ => Crash.impossible "RefObject.sub(3)"
           val _ =  debug_pop (fn () => ("sub   END: " ^ Bool.toString sub_result) 
					:: [pr_sort sort1 ^ "  <  " ^ pr_sorts sorts2])
	   in
	       sub_result
	   end

         and subVariance (variance::vrest, sort1::s1rest, sort2::s2rest) = 
               (ExplicitTyVar.covariant variance orelse sub (sort2, [sort1])) andalso
               (ExplicitTyVar.contravariant variance orelse sub (sort1, [sort2])) andalso
               subVariance (vrest, s1rest, s2rest)
           | subVariance ([], [], []) = true 

	 val result = sub(srt1, srts2)
         val _ = debug_refdec (fn () => "subSortList: RES" ^ Bool.toString result ^ "  " ^ 
			       pr_Sort srt1 ^ " <= " ^ 
			        (List.foldr (fn (srt, acc) => (pr_Sort srt) ^ " | " ^ acc)
				            "" srts2)
                               ^ "\n")

       in
	 result
       end


     (* For debugging *)
     fun pr_SortNames sns = 
       ListHacks.stringSep "(" ")" " & " (fn sn => SortName.pr_SortName ("", sn)) sns

    (* Simplified interfaces to subSortList, for use during sort-checking.
       Rely on the invariant of subSortList that the RHS never grows.
       Also, rely on all sortnames in a conjunction being the same
       (see mkConjSort).
    *)
    fun subSNs _ (sortname1::_, []) = SortName.isEmpty sortname1
      | subSNs conjSN (sortname1::_, [sortnames2]) =
        SortName.eq (sortname1, List.foldl (fn (a, b) => conjSN (a, b)) 
			            sortname1 
				    sortnames2)
      | subSNs _ (sns1, snss2) = 
          Crash.impossible 
            ("RefObject.subSNs" ^ 
             (pr_SortNames sns1) ^ " <= " ^ 
	     (ListHacks.stringSep "" "" " | " pr_SortNames snss2) ^ "\n")                  

    fun subSort (conjSortName : SortName * SortName -> SortName) (sort1, sort2) 
         : bool =
      subSortList (subSNs conjSortName, SOME o conjSortName)
                  (sort1, [sort2])

    fun superSort conjSN (sort1, sort2) = subSort conjSN (sort2, sort1)
      
    fun equal_Sort conjSortName (sort1, sort2) =  (* There is a faster algorithm for this.  *)
      (subSort conjSortName (sort1, sort2)) andalso 
      (subSort conjSortName (sort2, sort1))

    fun emptySort conjSN sort = 
      subSortList (subSNs conjSN, SOME o conjSN)
                  (sort, [])

    (* Functions related to subsorting.  *)

    fun applySort (conjSN : SortName * SortName -> SortName) (srt1, srt2) : Sort option =
      (debug_refdec (fn () => "applySort: " ^ (pr_Sort srt1) ^ "  TO  " ^ (pr_Sort srt2) ^ "\n") ;
       applySort' (subSNs conjSN, SOME o conjSN) (srt1, srt2))

    fun addSortToMaximals conjSN (srt1, srts2) =  (* add a sort to a list of maximal sorts *)
      ListHacks.addMax (subSort conjSN) (srt1, srts2)

    fun addSortToMinimals conjSN (srt1, srts2) =  (* add a sort to a list of minimal sorts *)
      ListHacks.addMax (superSort conjSN) (srt1, srts2)

    (********
    Special constants
    ********)

    type scon = SCon.scon

    (********
    Sorting of special constants
    ********)

    val SortBool = CONSSORT([], SortName.sortName_BOOL)
    and SortInt    = CONSSORT([], SortName.sortName_INT)
    and SortReal   = CONSSORT([], SortName.sortName_REAL)
    and SortString = CONSSORT([], SortName.sortName_STRING)
    val SortChar   = CONSSORT ([], SortName.sortName_CHAR)
    val SortWord8  = CONSSORT ([], SortName.sortName_WORD8)
    val SortWord   = CONSSORT ([], SortName.sortName_WORD)

    fun GetSortscon (scon, isWord8) =
      case scon
	of SCon.INTEGER _ => SortInt
	 | SCon.STRING _  => SortString
	 | SCon.REAL _    => SortReal
	 | SCon.CHAR _ => SortChar
	 | SCon.WORD _ => if isWord8 then SortWord8 else SortWord  

    (********
    Substitutions on Sorts.
    ********
    `Oldest' substitutions are at the head of the list
    ********)

    datatype SubstEntry = SORTsubst of SortVar * Sort
			| ROWsubst of RowVar * RecSort
			| FIELDsubst of FieldVar * Field

    datatype Substitution = SUBSTITUTION of SubstEntry list

    (********
    The identity substitution
    ********)

    val Id: Substitution = SUBSTITUTION nil

    val bogus_Subst = Id

    (********
    Make a (sort) substitution
    ********)

    fun sortSubstitution(sv: SortVar, sort: Sort): Substitution =
      SUBSTITUTION [SORTsubst(sv, sort)]

    (********
    Composition of substitutions.
    ********)

    infixr oo

    fun (S1: Substitution) oo (S2: Substitution): Substitution =
      case (S1, S2)
	of (SUBSTITUTION list1, SUBSTITUTION list2) =>
	     SUBSTITUTION(list2 @ list1)

    (********
    Apply a substitution to a sort (infix operator)
    ********)

    infixr on

   (* Application of substitutions (of which there are three kinds: sort,
      row, field). *)
   (* Note: the sort shouldn't contain  'a & 'a  which would require simplification
      of conjunctions (see mkConjSort) after substituting for 'a. *)
    fun (S: Substitution) on (sort: Sort): Sort =
      let
	fun sv_Subst (sv0, sort0) s =
	  let
	    val f = sv_Subst(sv0, sort0)
	  in
	    case s
	      of SORTVAR sv => 
		 (*(print ("BEFORE: " ^ pr_Sort sort0 ^ "  \t" ^ pr_Sort (SORTVAR sv) ^ 
			 "  \t" ^ pr_Sort (SORTVAR sv0)^ "\n"); *)
		  (if equal_SortVar(sv0,sv) then sort0 else SORTVAR sv)
(*		  before (print ("AFTER: " ^ pr_Sort sort0 ^ "  \t" ^ pr_Sort (SORTVAR sv) ^ 
			  "  \t" ^ pr_Sort (SORTVAR sv0) ^ "\n"))) *)
	       | ARROW(sort1, sort2) => ARROW(f sort1, f sort2)
	       | RECSORT r => RECSORT(recSort_map f r)
	       | CONSSORT(sorts, sortname) => CONSSORT(map f sorts, sortname)
	       | CONJSORT(sort1, sort2) => CONJSORT(f sort1, f sort2)
	       | BOGUSSORT => BOGUSSORT
	  end

	fun rv_Subst (rv, row) t =
	  let
	    val f = rv_Subst(rv, row)
	  in
	    case t
	      of SORTVAR _ => t
	       | ARROW(sort1, sort2) => ARROW(f sort1, f sort2)

	       | RECSORT r =>	(* I guess this is Remy's algorithm. *)
		   RECSORT(
		     case r
		       of NILrec => NILrec
			| VARrec rv' => if rv = rv' then row else r
			| ROWrec(lab, field, r') =>
			    let
			      fun R(NILrec, acc: (Lab.lab * Field) list) =
				    merge_Rows(acc, NILrec)

				| R(r as VARrec rv', acc) =
				    if rv = rv' then
				      merge_Rows(acc, row)
				    else
				      merge_Rows(acc, r)

				| R(ROWrec(lab', field', r''), acc) =
				    R(r'', acc @ [(lab', F field')])

			      and F ABSENTfield = ABSENTfield
				| F(PRESENTfield sort) = PRESENTfield(S on sort)
				| F(f as VARfield _) = f
			    in
			      R(r', [(lab, F field)])
			    end
		   )

	       | CONSSORT(sorts, sortname) => CONSSORT(map f sorts, sortname)
	       | CONJSORT(sort1, sort2) => CONJSORT(f sort1, f sort2)
	       | BOGUSSORT => BOGUSSORT
	  end

	and merge_Rows(nil: (Lab.lab * Field) list, row2) =
	      row2

	  | merge_Rows((l, f) :: rest, NILrec) =
	      ROWrec(l, f, merge_Rows(rest, NILrec))

	  | merge_Rows((l, f) :: rest, VARrec rv) =
	      ROWrec(l, f, merge_Rows(rest, VARrec rv))

	  | merge_Rows(fields1 as (l1, f1) :: rest,
		       row2 as ROWrec(l2, f2, row2')
		      ) =
	      if l1 = l2 then Crash.impossible "merge_Rows"
	      else if Lab.<(l1, l2)
	      then ROWrec(l1, f1, merge_Rows(rest, row2))
	      else ROWrec(l2, f2, merge_Rows(fields1, row2'))

	fun fv_Subst (fv, field) t =
	  let
	    val f = fv_Subst(fv, field)

	    fun fv_Subst_row row =
	      case row
		of NILrec => NILrec
		 | VARrec rv => VARrec rv
		 | ROWrec(lab, field', row') =>
		     ROWrec(lab,
			    case field'
			      of ABSENTfield => ABSENTfield
			       | PRESENTfield sort => PRESENTfield(f sort)
			       | VARfield fv' =>
				   if (fv = fv') then field else field',
			    fv_Subst_row row'
			   )
	  in
	    case t
	      of SORTVAR _ => t
	       | ARROW(sort1, sort2) => ARROW(f sort1, f sort2)
	       | RECSORT r => RECSORT(fv_Subst_row r)
	       | CONSSORT(sorts, sortname) => CONSSORT(map f sorts, sortname)
	       | CONJSORT(sort1, sort2) => CONJSORT(f sort1, f sort2)
	       | BOGUSSORT => BOGUSSORT
	  end
      in
	case S of SUBSTITUTION items =>
	  List.foldl
	  (fn (SORTsubst(sv, sort1), x) => (sv_Subst(sv, sort1) x) 
                                before (debug_refdec (fn () => "on : " ^ (pr_SortVar sv ) ^ " => "
					^ (pr_Sort sort1) ^ " in " ^ (pr_Sort x) ^ "\n" ))

	    | (ROWsubst(rv, row), x) => rv_Subst(rv, row) x
	    | (FIELDsubst(fv, field), x) => fv_Subst(fv, field) x
	  ) sort items
      end

    fun S_on_Row(S, row: RecSort): RecSort =
      case S on RECSORT row
	of RECSORT row' => row'
         | _ => Crash.impossible "S_on_Row"



    (********
    Sort Functions
    *********

    See StatObject for TySch_generalises_Type, TySch_generalises_TySch
        Also, TyNameSet, TyNamesTy, grounded_SortFcn
    ********)

    local
      (********
      Rename a list of SortVars
      *********
      This function is used to rename bound SortVars so as to obtain
      a normal form.
      ********)

      fun rename_SortVars (sortvar_list : (SortVar * Variance) list) 
	   : (SortVar * Variance) list * Substitution =
	let
	  fun rename [] x = []
	    | rename ((sv, variance) :: rest) id =
	      (sv, (BOUND_SV id, variance)) :: rename rest (id - 1)

	  val renaming_list = rename sortvar_list (~1)
	  val renamed_sortvars = map #2 renaming_list

	  val S = 
	    List.foldl
	      (fn ((sv1, (sv2, _)), S) => sortSubstitution
	                                       (sv1, mkSortSortVar sv2) oo S)
	      Id renaming_list
	in
	  (renamed_sortvars, S)
	end
    in
      fun mkSortFcn' (sortvar_list, sort) =
	let
	  val (renamed_sortvars, S) = rename_SortVars sortvar_list
	in
	  SORTFCN {sortvars = renamed_sortvars, sort = S on (sort: Sort)}
	end
    end

    fun mkSortFcn (sv_list, sort) =
      mkSortFcn' (map (fn sv => (sv, ExplicitTyVar.MIXED)) sv_list, sort)

    fun unSortFcn (SORTFCN {sortvars, sort}) = (map #1 sortvars, sort)
    fun varianceSortFcn (SORTFCN {sortvars, sort}) = map #2 sortvars

    fun applySortFcn (SORTFCN {sortvars, sort}, sort_list : Sort list) : Sort =
      let
	val pairs =
	  ListPair.zip(sortvars, sort_list)
(*	val S' =
	  List.foldl
	  (fn (((sv,_), sort1), S) => sortSubstitution(sv,sort1) oo S)
	  Id pairs
        val res = S' on sort
*)
        val res = List.foldl (fn (((sv, _), sort1), srt) => 
                                let val res = sortSubstitution (sv, sort1) on srt
(*				    val () = print ("SRT: " ^ pr_Sort srt ^ "\n")
				    val () = print ("ACC: " ^ pr_Sort res ^ "\n") *)
				in
				    res
				end)
		             sort
			     pairs
      in
        res
      end

    fun applySortFcnVars (sortfcn as SORTFCN{sortvars, sort}) = 
      let 
	val svs = map (fresh_sv) sortvars
	val sorts = map mkSortSortVar svs
      in
	(svs, applySortFcn(sortfcn, sorts))
      end
            
    fun arity_SortFcn (SORTFCN {sortvars, sort}) : int =
      List.length sortvars

    (* bound sort-variables have been renamed in order. *)
    fun equal_SortFcn conjSortName
                      (sf1 as SORTFCN {sortvars=svs1, sort=s1}, 
		       sf2 as SORTFCN {sortvars=svs2, sort=s2}) = 
      (arity_SortFcn sf1 = arity_SortFcn sf1) andalso      
      let
	val sorts = map (mkSortSortVar o fresh_sv) svs1
      in
	equal_Sort conjSortName (applySortFcn(sf1, sorts), applySortFcn(sf2, sorts))
      end

    fun TypeFcn_of_SortFcn (sortfcn as SORTFCN {sortvars, sort}) : TypeFcn =
      let
	val tyvars = map fresh_tv sortvars
        val sorts = map (mkSortSortVar o SVofTV) tyvars
        val _ = SO.Level.push()
        val ty = tyOfSort(applySortFcn(sortfcn, sorts))
        val _ = SO.Level.pop()
      in
        
	StatObject.TypeFcn.from_TyVars_and_Type
          (tyvars, ty)
      end

    fun compatible_SortFcn (sf1 as SORTFCN {sortvars=svs1, sort=s1}, 
			    sf2 as SORTFCN {sortvars=svs2, sort=s2}) = 
      (arity_SortFcn sf1 = arity_SortFcn sf2) andalso      
      let
	val sorts = map (mkSortSortVar o SVofTV o fresh_tv) svs1
      in
	compatible_Sort(applySortFcn(sf1, sorts), applySortFcn(sf2, sorts))
      end

    fun conjSortFcn (conjSortName : SortName * SortName -> SortName option)
                        (sf1 as SORTFCN{sortvars=svs1, sort=s1},
			 sf2 as SORTFCN{sortvars=svs2, sort=s2}) = 
	SORTFCN{sortvars=svs1, sort=mkConjSort conjSortName (s1, s2)}
     
    fun SortName_in_SortFcn (sortname : SortName) : SortFcn =
      let
	fun make_list 0 _ = []
	  | make_list n id =
	    let
	      val sortvar = BOUND_SV id 
	    in
	      sortvar :: make_list (n-1) (id-1)
	    end

	val sortvar_list = make_list (SortName.arity sortname) ~1	
	val sort_list  = map mkSortSortVar sortvar_list
	val conssort   = mkConsSort(sort_list, sortname)
	val sort = mkSortConsSort conssort
	val sortvars = ListPair.zip (sortvar_list, SortName.variance sortname)
      in
	SORTFCN {sortvars = sortvars, sort = sort}
      end

    (* Perhaps should check sortvars here *)
    fun SortFcn_to_SortName (SORTFCN {sortvars, sort=CONSSORT(sortvars2,sortname)}) =
        (  (if ListPair.all equal_SortVar
			  (map #1 sortvars, map (valOf o unSortSortVar) sortvars2) then 
                SOME sortname
 	    else  NONE)
	handle Option => NONE)

      | SortFcn_to_SortName _ = NONE


    (* Substitute sortfcns for sortnames.  Also simplifies all conjunctions
       by calling "mkConjSort" with "conjSN".  *)
    fun renSimpSort (fns as (renameSN : SortName -> SortFcn, conjSN)) sort =
        case sort of
         (s as SORTVAR(_)) => s
      |  (ARROW(s1, s2)) => ARROW (renSimpSort fns s1, renSimpSort fns s2)
      |  (RECSORT rs) => RECSORT (renSimpRecSort fns rs)
      |  (CONSSORT (sorts, sortname)) => 
            let (*val _ = print ("SORT: " ^ pr_Sort sort ^ "\n")
                val _ = map (fn s => print ("RENAMED:" ^ pr_Sort ((renSimpSort fns) s) ^ "\n"))
			    sorts *)
                val res = applySortFcn (renameSN sortname, map (renSimpSort fns) sorts)
(*		val _ = print ("RES: " ^pr_Sort res ^ "\n") *)
	    in 
	       res
	    end
      |  (CONJSORT (s1, s2)) => mkConjSort (SOME o conjSN) (renSimpSort fns s1, renSimpSort fns s2)
      |  BOGUSSORT => BOGUSSORT

    and renSimpRecSort (fns as (renameSN, conjSN)) recsort =
        case recsort of
          NILrec => NILrec
        | (ROWrec (lab, PRESENTfield s, rs)) =>
          ROWrec (lab, PRESENTfield (renSimpSort fns s), renSimpRecSort fns rs)
        | _ => Crash.impossible "RefObject.renSimpRecSort"

    fun renameSimpl_SFcn (renameSN : SortName -> SortName, conjSN) (SORTFCN{sortvars, sort}) =
	SORTFCN{sortvars=sortvars, 
                sort = renSimpSort (fn sn => SortName_in_SortFcn (renameSN sn), conjSN) sort}

  
      
    (********
    Sort schemes
    ********)
  
    (* mkSortScheme
         sortvars must be sorted by the order the type variables
         LAST appear in the sort.  This gives a cannonical form for
         easy equality checking, and gives the same order for refinements
         of the same type.
    *)

    val mkSortScheme = mkSortFcn
    val equal_SortScheme = equal_SortFcn

    (* There will be "BoundSortVar"s in the sort returned *)
    val unSortScheme = unSortFcn
    val compatible_SortScheme = compatible_SortFcn

    (********
    Make a sort into a sortscheme with no bound variables
    ********)

    fun Sort_in_SortScheme (sort : Sort) = SORTFCN{sortvars=[], sort=sort}

    val Sort_in_SortScheme =   (* resolved at compile time *)
	if !Flags.DEBUG_SORTS then
	  (fn sort =>
	     let
	       val sigma = Sort_in_SortScheme sort
		 
	       val tree =
		 StringTree.NODE{start="Sort_in_SortScheme: ", finish="", indent=0,
			 children=[layoutSort sort, layoutSortScheme sigma],
			 childsep=StringTree.LEFT " => "
			}
			   
	       val report = PP.reportStringTree tree
	     in
	       Report.print report;
	       sigma
	     end
	   )
	else
	  Sort_in_SortScheme

    fun Close_Sort (sort : Sort) : SortScheme = 
      mkSortScheme (sortvarsSort sort, sort)

    fun Close_SortScheme (SORTFCN{sortvars=_, sort}, svs : SortVar list) : SortScheme =
      let
	val actual_svs = List.filter (fn x => List.exists (fn y => equal_SortVar(x, y)) svs)
	                          (sortvarsSort sort)
      in
         mkSortScheme(actual_svs, sort)
      end
      
 
    (********
    Get an instance of a SortScheme.  Instantiations of sort variables
    must be provided.
    ********)

    val instance = applySortFcn
    val instance_vars = applySortFcnVars

    exception exNone  (* Used for errors in the three functions which follow.  *)
    (********
    plus_SortScheme - conjoin if sf1 and sf2 refine the same typescheme. If so, then svs1 = svs2.
    ********)
    fun plus_SortScheme (conjSortName : SortName * SortName -> SortName option)
                        (sf1 as SORTFCN{sortvars=svs1, sort=s1},
			 sf2 as SORTFCN{sortvars=svs2, sort=s2}) =
    
      let exception exNone
          fun conjSN (sn1, sn2) = case conjSortName (sn1, sn2) of SOME res_sn => res_sn 
                                                                | NONE => raise exNone
      in    (* The optimization below reduces the sizes of the sorts for constructors. *)
        (if compatible_SortScheme (sf1, sf2) then
	     let val srts1 = list_Conjuncts s1
                 val srts2 = list_Conjuncts s2
                 val srts = foldl (addSortToMinimals conjSN) srts1 srts2 (* [s1] [s2] *)
                 val conj = ListHacks.foldl' (mkConjSort conjSortName) srts
	     in
                SORTFCN{sortvars=svs1, sort=conj}
	     end
          else (debug_pr_msg (fn () => "plus_SortScheme1: " ^ pr_Sort s1 ^ 
                                          " plus " ^ pr_Sort s2 ^ "\n"); 
                sf1)
        ) handle exNone => (debug_pr_msg (fn () => "plus_SortScheme2: " ^ pr_Sort s1 ^ 
                                          " plus " ^ pr_Sort s2 ^ "\n"); 
                            SORTFCN{sortvars=svs1, sort=mkConjSort conjSortName (s1, s2)})
      end


    (***********
    Instantiation via upper and lower bounds, a la local type inference.
    ************)
    type LowerBounds = (SortVar * Sort) list and UpperBounds = (SortVar * Sort) list

    datatype TyVarVariance = datatype ExplicitTyVar.Variance  (* For isInstBounds. *)


    fun checkBounds conjSN sv (low, up) = 
        let fun matches_sv (sv2, srt) = equal_SortVar (sv2, sv)
            val low_srts = map (fn (_, srt) => srt) (List.filter matches_sv low)
            val up_srts = map (fn (_, srt) => srt) (List.filter matches_sv up)
        in
          case up_srts 
            of [] => true  (* Technically, could be false without top sorts.  *)
             | _ => let val up_srt = ListHacks.foldl' (mkConjSort (SOME o conjSN)) up_srts
                    in   List.all (fn low_srt => subSort conjSN (low_srt, up_srt)) low_srts
                    end
        end

    fun isInstance conjSN srt1 sortsch2 : bool =
      let
        fun debug_print x = () (*print (x^ "\n") *)

	(* Check whether srt1 is an instance of srt2, returning upper and lower bounds for svars. *)
	fun isInstBounds srt1 svars srt2 : (LowerBounds * UpperBounds) option =
(*          ( Report.print (PP.reportStringTree (StringTree.NODE{start="isInstBounds: ", finish="", indent=0,
			 children=(map layoutSortVar svars) @ [layoutSort srt1, layoutSort srt2],
			 childsep=StringTree.LEFT " , "}));  *)	       
	  case (srt1, srt2)
	    of (_, SORTVAR sv) => if List.exists (fn svar => equal_SortVar (sv, svar)) svars
				  then  SOME ([], [(sv, srt1)]) 
				  else (case srt1 of SORTVAR sv1 => 
						      if equal_SortVar (sv1, sv) then  SOME ([], [])
						      else (debug_print "isInstBounds: SORTVAR NONE"; NONE) 
						   | _ => (debug_print "isInstBounds: SORTVAR _"; NONE))

	     | (ARROW (s11, s12), ARROW (s21, s22)) => 
		 (case (isInstBounds s11 svars s21, isInstBounds s12 svars s22)
		    of (SOME (low1, up1), SOME (low2, up2)) =>  SOME (up1 @ low2, low1 @ up2)
		     | _ => NONE)
	     | (RECSORT r1, RECSORT r2) => 
		   List.foldl (fn ((s11, s12), bnds2) => 
				  case (isInstBounds s11 svars s12, bnds2)
				    of (SOME (low1, up1), SOME (low2, up2)) =>  
				        SOME (low1 @ low2, up1 @ up2)
				     | _ => NONE)
			      (SOME ([], []))
			      (ListPair.zip (recSort_list r1, recSort_list r2))

	     | (CONSSORT (srts1, sn1), CONSSORT (srts2, sn2)) => 
		 if not (SortName.eq (sn2, conjSN(sn1,sn2))) then (debug_print "isInstBounds: CONSSORT NONE";NONE (* check sn2 <= sn1 *) )
		 else List.foldl (fn (((s11, s12), variance), bnds2) => 
				    case (isInstBounds s11 svars s12, bnds2, variance)
				      of (_, res, IGNORED) => res
				       | (SOME (low1, up1), SOME (low2, up2), COVARIANT) => 
					    SOME (low1 @ low2, up1 @ up2)
				       | (SOME (low1, up1), SOME (low2, up2), CONTRAVARIANT) => 
					    SOME (up1 @ low2, low1 @ up2)
				       | (SOME (low1, up1), SOME (low2, up2), MIXED) => 
					    SOME (low1 @ up1 @ low2, low1 @ up2 @ up2)
				       | _ => NONE   )
				 (SOME ([], []))
				 (ListPair.zip (ListPair.zip (srts1, srts2), SortName.variance sn2))


               (* There's clearly an issue of completeness here - the goal must exactly match
                  aside from the sort-variable positions. *)
	     | (CONJSORT (srt11, srt12), CONJSORT (srt21, srt22)) => 
		 (case (isInstBounds srt11 svars srt21, isInstBounds srt12 svars srt22)
		    of (SOME (low1, up1), SOME (low2, up2)) =>  SOME (low1 @ low2, up1 @ up2)
		     | _ => NONE)

	     | (BOGUSSORT, _) => SOME ([], [])
	     | (_, BOGUSSORT) => SOME ([], [])
	     | _ => NONE



           val (svars, srt2) = instance_vars sortsch2
        in
           if compatible_Sort (srt1, srt2) then 
               (debug_print "isInstBounds: compatible"  ; 
	       subSort conjSN (srt2, srt1) )
	   else
            case isInstBounds srt1 svars srt2 
              of NONE => (debug_print "isInstBounds: NONE"  ; false)
               | SOME (low, up) =>  
                 (debug_print "isInstBounds: SOME"  ;
                  let val res = List.all (fn sv => checkBounds conjSN sv (low, up)) svars
                      val _ = debug_print (Bool.toString res)
                  in res
                  end )
        end

    fun sntx_eqsrtsch (SORTFCN{sortvars=svs1, sort=s1},
		       SORTFCN{sortvars=svs2, sort=s2}) = 
       sntx_eqsrt(s1, s2) (* Assume that the sortschemes refine the same type. *)
       andalso ListPair.all equal_SortVar (map #1 svs1, map #1 svs2)  (* Always be true? *)




    (********
    combineSortScheme - add constructor sortscheme sf2 to sf1, including conjuncts for
    distributivity.  Redundant conjuncts are removed.
    INCOMPLETE currently.
    ********)
    val DEBUG_combineSortSchemes = ref false
    val _ = Flags.add_flag ("DEBUG_combineSortSchemes",
                             "Debug combineSortSchemes?", DEBUG_combineSortSchemes)

    fun combineSortSchemes (conjSortName : SortName * SortName -> SortName option)
                        (sf1 as SORTFCN{sortvars=svs1, sort=s1},
			 sf2 as SORTFCN{sortvars=svs2, sort=s2}) =
    
      let
          val _ = if !DEBUG_combineSortSchemes then 
                    print ("combineSortSchemes0: " ^ pr_Sort s1 ^ 
                                          " plus " ^ pr_Sort s2 ^ "\n")
                  else ();
          val mkConj = mkConjSort conjSortName
          fun conjSN (sn1, sn2) = case conjSortName (sn1, sn2) of SOME res_sn => res_sn 
                                                                | NONE => raise exNone
          val conjuncts1 = list_Conjuncts s1
          val conjuncts2 = list_Conjuncts s2
          fun distribute (ARROW(s11, s12), ARROW(s21, s22)) = 
                ARROW(mkConj(s11, s21), mkConj(s12, s22))
            | distribute (s1, _) = s1  (* No need to distribute when not a function sort.  *)

          fun hasEmptyArg (ARROW(s1, _)) = emptySort conjSN s1 (* Not used currently. *)
            | hasEmptyArg _ = false

          fun distr_list sorts1 [] = sorts1
            | distr_list sorts1 (sort2 :: sorts3) = 
              let val sorts12 = addSortToMinimals conjSN (sort2, sorts1)
                  val distributed = map (fn srt1 => distribute (srt1, sort2)) sorts1
                  val sorts12plus = foldl (addSortToMinimals conjSN) sorts12 distributed
              in
                 distr_list sorts12plus sorts3
              end

          val res_conjuncts = distr_list conjuncts1 conjuncts2
          val res_sort = ListHacks.foldl' (mkConjSort conjSortName) res_conjuncts
      in
        if compatible_SortScheme (sf1, sf2)
          then SORTFCN{sortvars=svs1, sort=res_sort}
          else (debug_pr_msg (fn () => "combineSortSchemes1: " ^ pr_Sort s1 ^ 
                                          " plus " ^ pr_Sort s2 ^ "\n"); 
                sf1)
        
      end  handle exNone => (debug_pr_msg (fn () => "combineSortSchemes2: " ^ pr_Sort s1 ^ 
                                          " plus " ^ pr_Sort s2 ^ "\n"); 
                             SORTFCN{sortvars=svs1, sort=mkConjSort conjSortName (s1, s2)})

    (********
    Apply a substitution to a sort scheme
    ********)

    fun onScheme (S : Substitution, SORTFCN{sortvars, sort}) =
      SORTFCN{sortvars=sortvars, sort=S on sort}

    (*********
    Sort realisations
    *********)

    datatype SortFcn' = SORTNAME of SortName |  EXPANDED of SortFcn

    fun convert_sortfcn' (EXPANDED theta) = theta
      | convert_sortfcn' (SORTNAME t) = SortName_in_SortFcn t

    fun conjSortFcn' conjSN (SORTNAME sn1, SORTNAME sn2) = SORTNAME (conjSN (sn1, sn2))
      | conjSortFcn' conjSN (sf1, sf2) =
	EXPANDED (conjSortFcn (SOME o conjSN) (convert_sortfcn' sf1, convert_sortfcn' sf2))
    
    datatype realisation = 
        Not_Id of SortFcn' SortName.Map.map
      | Realisation_Id

    type trealisation = SO.realisation

    structure Realisation = struct

      type conjSN = SortName * SortName -> SortName

      fun dom Realisation_Id = SortName.Set.empty
	| dom (Not_Id m) = SortName.Set.fromList(SortName.Map.dom m)

      val Id = Realisation_Id

      fun is_Id Realisation_Id = true   (* conservative test *)
	| is_Id _ = false

      fun singleton (t,theta) = Not_Id (SortName.Map.singleton(t,EXPANDED theta))

      fun singleSN (sn1, sn2) = Not_Id (SortName.Map.singleton(sn1, SORTNAME sn2))

      fun from_T_and_sortname (T, t0) =  (* Not used? *)
	if SortName.Set.isEmpty T then Realisation_Id
	else Not_Id (SortName.Set.fold (fn t => fn acc => SortName.Map.add(t,SORTNAME t0,acc)) 
                                       SortName.Map.empty T )

      (* A couple of support functions for renaming, below. *)
      fun refreshSortName (tn:TyName) sn =   (* Make a new refinement of tn, based on sn. *)
          if SortName.isEmpty sn then  SortName.freshEmptySortName tn
          else let val sn' = SortName.freshSortName {conjuncts = SortName.conjuncts sn,
                                                     variance= SortName.variance sn,
				                     tyname= tn}
                   val _ = if SortName.allVars_Covariant sn then 
                             SortName.set_Covariance sn'
                           else ()
               in  sn'  end
(*
      fun refreshConjSortName (tn:TyName, conjs) sn =   (* Make a new refinement of tn, based on sn. *)
          if SortName.isEmpty sn then  SortName.freshEmptySortName tn
          else let val sn' = SortName.freshSortName {conjuncts = SortName.conjuncts sn,
                                                     variance= SortName.variance sn,
				                     tyname= tn}
                   val _ = if SortName.allVars_Covariant sn then 
                             SortName.set_Covariance sn'
                           else ()
               in  sn'  end
*)
      (* Find the image of a TyName in tphi, which should be another TyName.  *)
      fun tnRealiser tphi tn = case SO.TypeFcn.to_TyName (SO.Realisation.on_TyName tphi tn)
                                 of SOME tnNew => tnNew

      fun renaming' (tphi: trealisation) (T: SortName.Set.Set) : SortName.Set.Set * realisation =
	if SortName.Set.isEmpty T then (SortName.Set.empty, Id)
	else let 
                 (* Add a renaming of sortname sn to set T and renaming m if not present.  Adds
                    conjuncts of sn first, because they are needed. *)
                 fun addRenaming (sn_in, (Tin, m_in)) 
		       : SortName.Set.Set * SortFcn' SortName.Map.map = 
		     let val sn_cnjs = SortName.sortNameConjuncts sn_in
			 val tn = tnRealiser tphi (SortName.tyname sn_in)
                         fun debug_print x = () (* print (x^ "\n") *)
		     in
		       if length sn_cnjs > 1 then
                           let val _ = debug_print ("renaming': conj case: " ^ 
                                                    SortName.pr_SortName ("", sn_in))
			       val (T2, m2) = foldl addRenaming (Tin, m_in) sn_cnjs
                               fun lookup sn = (case SortName.Map.lookup m2 sn of 
						    SOME (SORTNAME sn) => sn)
                               val sn_new = SortName.newConjList 
						(map lookup sn_cnjs, SortName.isEmpty sn_in,
						 SortName.variance sn_in, tn)
                               val _ = if SortName.allVars_Covariant sn_in then 
					   SortName.set_Covariance sn_new
				       else ()
			   in
			     (SortName.Set.insert sn_new T2, 
			      SortName.Map.add (sn_in, SORTNAME sn_new, m2))
			   end
		       else if SortName.Set.member sn_in Tin then 
			   (debug_print "renaming': found\n";  (Tin, m_in))
		       else let  val sn_new = refreshSortName tn sn_in
				 val _ = debug_print "renaming': refresh case\n"
			    in  (SortName.Set.insert sn_in Tin, 
				 SortName.Map.add (sn_in, SORTNAME sn_new, m_in))
			    end
		     end

		 val (Tout, m_out) = 
		     SortName.Set.fold (fn sn : SortName => fn (T', m) => addRenaming (sn, (T', m)))
				       (SortName.Set.empty, SortName.Map.empty)
				       T
	     in (Tout,  Not_Id m_out)
	     end

      fun renaming tphi (T: SortName.Set.Set) : realisation = #2 (renaming' tphi T)

      fun restrict T Realisation_Id = Realisation_Id
	| restrict T (Not_Id m) =
	let val m' = SortName.Set.fold(fn t => fn acc =>
				     case SortName.Map.lookup m t
				       of SOME theta => SortName.Map.add(t,theta,acc)
					| NONE => acc) SortName.Map.empty T
	in if SortName.Map.isEmpty m' then Realisation_Id
	   else Not_Id m'
	end

      fun restrict_from T Realisation_Id = Realisation_Id
	| restrict_from T (Not_Id m) =
	let val m' = SortName.Map.Fold(fn ((t, theta), acc) =>
				     if SortName.Set.member t T then acc
				     else SortName.Map.add(t,theta,acc)) SortName.Map.empty m
	in if SortName.Map.isEmpty m' then Realisation_Id
	   else Not_Id m'
	end

      local exception Inverse
      in fun inverse Realisation_Id = SOME Realisation_Id
	   | inverse (Not_Id m) = 
	      (SOME(Not_Id(SortName.Map.Fold(fn ((t, theta), acc) => 
					   case theta
					     of SORTNAME t' => 
					       if SortName.Set.member t' 
                                                   (SortName.Set.fromList(SortName.Map.dom acc))
                                               then raise Inverse
					       else SortName.Map.add(t', SORTNAME t, acc)
					      | EXPANDED theta' => raise Inverse)
                                            SortName.Map.empty m))
	       handle Inverse => NONE)
      end

      fun on_SortName conjSN Realisation_Id t = SortName_in_SortFcn t
	| on_SortName conjSN (Not_Id m) t =
	  (case SortName.Map.lookup m t
	    of SOME(SORTNAME t) => SortName_in_SortFcn t
	     | SOME(EXPANDED theta) => theta
	     | NONE =>   (* Try expanding to conjuncts. *)
                (case SortName.sortNameConjuncts t of
                     [sn] => ((*debug_print ("on_SortName: ONE CONJUNCT: " ^ SortName.pr_SortName_noprefix t ^ "\n");*)
		              SortName_in_SortFcn t)
		   | conjuncts => 
                    ((*debug_print "on_SortName: CONJUNCTS\n"; 
		     map (print o SortName.pr_SortName_noprefix) conjuncts; *)
		     ListHacks.foldl' (conjSortFcn (SOME o conjSN))
				      (map (on_SortName conjSN (Not_Id m)) conjuncts) ) ))

(*
      fun on_SortName_set (rea : realisation) (T : SortName.Set.Set) =
	    if is_Id rea then T else
	      SortName.Set.fold
	        (fn t => fn T =>
		 SortName.Set.union (SortFcn.sortnames (on_SortName rea t)) T)
		  SortName.Set.empty T
*)
      fun on_SortName' _ Realisation_Id t : SortFcn' = SORTNAME t
	| on_SortName' conjSN (Not_Id m) t = 
	  (case SortName.Map.lookup m t
	    of SOME theta => theta
	     | NONE => 
                (case SortName.sortNameConjuncts t of
                     [sn] => SORTNAME t
		   | conjuncts => 
		     ListHacks.foldl' (conjSortFcn' conjSN)
				      (map (on_SortName' conjSN (Not_Id m)) conjuncts) ) )


      fun on_Sort _ Realisation_Id sort = sort
	| on_Sort conjSN phi sort = 
          renSimpSort (on_SortName conjSN phi, conjSN) sort 


      fun on_SortScheme _ Realisation_Id scheme = scheme
	  | on_SortScheme conjSN phi sigma = 
              let val (svs1, sort1) = instance_vars sigma
	      in
                  mkSortFcn (svs1, on_Sort conjSN phi sort1) (* Ordering the same as input. *)
	      end

      fun on_SortFcn _ Realisation_Id sfcn = sfcn
	  | on_SortFcn conjSN phi sigma = 
              let val (svs1, sort1) = applySortFcnVars sigma
	      in
                  mkSortFcn (svs1, on_Sort conjSN phi sort1) 
	      end

      val on_SortFcn = on_SortScheme

      fun on_SortFcn' _ Realisation_Id sortfcn' = sortfcn'
	| on_SortFcn' conjSN phi (SORTNAME t) = on_SortName' conjSN phi t
	| on_SortFcn' conjSN phi (EXPANDED theta) = EXPANDED (on_SortFcn conjSN phi theta)

      fun on_Realisation _ Realisation_Id phi = phi
	| on_Realisation _ phi Realisation_Id = Realisation_Id
	| on_Realisation conjSN phi (Not_Id m) =
	Not_Id(SortName.Map.Fold (fn ((t,theta), acc) =>
				     SortName.Map.add(t,on_SortFcn' conjSN phi theta,acc)) 
                                 SortName.Map.empty m)

      nonfix oo
      fun oo _ (Realisation_Id : realisation) (phi : realisation) : realisation = phi
	| oo _ phi Realisation_Id = phi
	| oo conjSN (phi1 as Not_Id m1) phi2 = (case on_Realisation conjSN phi1 phi2
					        of Realisation_Id => phi1
 					         | Not_Id m2 => Not_Id (SortName.Map.plus(m1, m2)))

      fun plus (Not_Id m1) (Not_Id m2) = Not_Id (SortName.Map.plus(m1, m2))
        | plus Realisation_Id phi = phi
        | plus phi Realisation_Id = phi

(*
      fun enrich (rea0, (rea,T)) =
	SortName.Set.fold (fn t => fn acc => acc andalso 
			 SortFcn.eq(on_SortName rea0 t, on_SortName rea t)) true T

      fun eq (Realisation_Id, Realisation_Id) = true       (* conservative check, 
                                                              thus eq is a bad word for it *)
	| eq (rea1,rea2) = 
	let val T = dom rea1
	in SortName.Set.eq T (dom rea2) andalso enrich (rea1,(rea2,T))
	end

      (* Unused currently.  *)
      fun match _ (Realisation_Id, rea0) = ()
	| match conjSN (Not_Id m, rea0) =
	in SortName.Map.Fold (fn ((t, theta),_) => 
			       SortFcn.match(convert_sortfcn' theta, on_SortName conjSN rea0 t)) 
                             () m
	end
*)
      fun layoutSortFcn' (SORTNAME t) = SortName.layout t
	| layoutSortFcn' (EXPANDED theta) = layoutSortFcn theta
      fun layout Realisation_Id = StringTree.LEAF "Id"
	| layout (Not_Id m) = SortName.Map.layoutMap {start="{",eq=" -> ", finish="}",sep=", "}
	SortName.layout layoutSortFcn' m

    end (*Realisation*)


  end;
