(* Sort names - ie elements of a refinement lattice *)

functor SortName(structure TyName: TYNAME
		 structure TyCon : TYCON
		   sharing type TyCon.tycon = TyName.tycon
                 structure TyVar : TYVAR
		 structure Name : NAME   (* Was Timestamp.  Check equality checks? *)
                 structure PP            : PRETTYPRINT
                 structure Report                 : REPORT
		 structure Flags: FLAGS
                 structure ListHacks: LIST_HACKS
                 structure IntFinMap              : MONO_FINMAP where type dom = int
		 structure Crash:CRASH
		): SORTNAME =
  struct

    type sortcon = TyCon.tycon
    type Variance = TyVar.Variance
    type TyName = TyName.TyName

    fun debug_print _= () (* Flags.debug_print *)

    datatype SortName = SORTNAME of sortcon list * SortName list * bool * Variance list ref
                                      * TyName * Name.name * bool ref
        (* components are: conjuncts, conjunctsSN, empty_Covariant, Variance, TyName, Stamp, 
                           allVars_Covariant *)

    fun pr_SortName (prefix: string, SORTNAME(conjuncts, _, empty, _, tyname, ts, _)): string =
      (if empty then 
 	   prefix ^ "<empty>"
(* 	   prefix ^ "<" ^ (TyName.pr_TyName tyname) ^ ">" *)
       else if (size prefix) > 0 andalso (length conjuncts > 1) then
           prefix ^ "(" ^ (ListHacks.foldl' (fn (a, b) => a ^ " & " ^ b)
                                      (map TyCon.pr_TyCon conjuncts) ) ^ ")"
       else
           prefix ^ (ListHacks.foldl' (fn (a, b) => a ^ " & " ^ b)
                                      (map TyCon.pr_TyCon conjuncts) )  )
      ^ (if !Flags.DEBUG_SORTS then "<" ^ (Int.toString (Name.key ts)) ^ ">" else "")

    val pr_SortName_parens = pr_SortName

    fun pr_SortName_noprefix sn = pr_SortName ("", sn)

    (* Should check length(variance)=arity(tyname). *)
    fun freshSortName {conjuncts: sortcon list, variance:Variance list,
		       tyname: TyName} =
      let val res =      
         SORTNAME (conjuncts, [], false, ref variance, tyname, Name.new(), ref false)
          val _ = debug_print ("freshSortName: " ^ pr_SortName_noprefix res ^ "\n")
      in res
      end 

    fun freshEmptySortName (tyname: TyName) : SortName = 
      SORTNAME([], [], true, ref (List.tabulate (TyName.arity tyname, fn n => TyVar.IGNORED)), 
	       tyname, Name.new(), ref false)

    fun freshTypeSortName (tyname, variance) = 
      freshSortName
        {conjuncts=[TyName.tycon tyname],         
	 variance=variance,
         tyname=tyname}


    fun sortNameConjuncts (sn as SORTNAME (_, [], _, _, _, _, _)) = [sn]
      | sortNameConjuncts (SORTNAME (_, sncnjs, _, _, _, _, _)) = sncnjs
  
    fun set_Covariance (SORTNAME(_, _, _, _, _, _, covar_ref)) = (covar_ref := true)

    fun isEmpty (SORTNAME (_, _, empty, _, _, _, _)) = empty

    fun tyname (SORTNAME (conjuncts, sncnjs, _, _, tyname, stamp, _)) = tyname
    fun arity (SORTNAME (conjuncts, sncnjs, _, _, tyname, stamp, _)) = TyName.arity(tyname)
    fun conjuncts (SORTNAME (conjuncts, sncnjs, _, _, tyname, stamp, _)) = conjuncts
    fun variance (SORTNAME (conjuncts, sncnjs, _, ref variance, _, stamp, _)) = variance
    fun leq (SORTNAME (_, _, _, _, _, stamp, _), SORTNAME (_, _, _, _, _, stamp', _)) : bool =
      Name.key stamp <= Name.key stamp'
    fun idnum (SORTNAME (_, _, _, _, _, stamp, _)) : int = Name.key stamp
    fun allVars_Covariant (SORTNAME (_, _, _, ref variance, tyname, _, ref cov)) = 
	cov orelse (List.all TyVar.covariant variance)

    fun eq (SORTNAME (_, _, _, _, _, stamp, _), SORTNAME (_, _, _, _, _, stamp', _)) : bool =
      Name.key stamp = Name.key stamp'

    fun compare (sn1, sn2) = let val id1 = idnum sn1
				 val id2 = idnum sn2
			     in
				 Int.compare (id1, id2)
			     end

    type name = Name.name
    fun name (SORTNAME (_, _, _, _, _, stamp, _)) : name = stamp

    fun join_variance (var1, var2) = 
	ListPair.map TyVar.join_variance (var1, var2)

    fun improve_variance (SORTNAME (_, _, _, varianceRef, _, _, _)) variance2 =
	varianceRef := join_variance (!varianceRef, variance2)

    fun layout sn = PP.layout_pair (PP.layout_list (fn sn2 => StringTree.LEAF (pr_SortName ("", sn2))))
                                   (fn () => StringTree.LEAF (pr_SortName ("", sn)))
                                    (sortNameConjuncts sn, ())

    fun newConjSortName (sn1, sn2, empty, var_bound: Variance list, tyname: TyName) = 
        let val var_new = join_variance (var_bound, join_variance (variance sn1, variance sn2))
            val conjuncts_new = (conjuncts sn1) @ (conjuncts sn2)
            val sn_conjs_new = (sortNameConjuncts sn1) @ (sortNameConjuncts sn2)
            val covar_new = ref ((allVars_Covariant sn1) orelse (allVars_Covariant sn1))
            val res = SORTNAME (conjuncts_new, sn_conjs_new, empty, ref var_new, tyname, 
				Name.new(), covar_new)
            val _ = debug_print ("newConjSortName: " ^ pr_SortName_noprefix res ^ " = ")
            val _ = map (debug_print o pr_SortName_noprefix) sn_conjs_new
            val _ = debug_print "\n"
        in
	    res
        end

    fun newConjList (sns, empty, var_bound: Variance list, tyname: TyName) = 
        let val var_new = foldl join_variance var_bound (map variance sns)
            val conjuncts_new = ListHacks.flatten (map conjuncts sns)
            val sn_conjs_new =  ListHacks.flatten (map sortNameConjuncts sns)
            val covar_new = ref (List.exists allVars_Covariant sns)
            val res = SORTNAME (conjuncts_new, sn_conjs_new, empty, ref var_new, tyname, 
				Name.new(), covar_new)
            val _ = debug_print ("newConjList: " ^ pr_SortName_noprefix res ^ " = ")
            val _ = map (debug_print o pr_SortName_noprefix) sn_conjs_new
            val _ = debug_print "\n"
        in
	    res
        end


    val sortName_BOOL = 
      freshTypeSortName(TyName.tyName_BOOL, [])
    val sortName_INT =
      freshTypeSortName(TyName.tyName_INT, [])
    val sortName_REAL =
      freshTypeSortName(TyName.tyName_REAL, [])
    val sortName_STRING =
      freshTypeSortName(TyName.tyName_STRING, [])
    val sortName_LIST = 
      freshTypeSortName(TyName.tyName_LIST, [TyVar.COVARIANT])
    val sortName_REF =
      freshTypeSortName(TyName.tyName_REF,[TyVar.MIXED])
    val sortName_EXN =
      freshTypeSortName(TyName.tyName_EXN, [])
    val sortName_CHAR = freshTypeSortName(TyName.tyName_CHAR, [])
    val sortName_WORD = freshTypeSortName(TyName.tyName_WORD, [])
    val sortName_WORD8 = freshTypeSortName(TyName.tyName_WORD8, [])

    val sortName_WORD_TABLE = freshTypeSortName(TyName.tyName_WORD_TABLE, 
                                                [TyVar.MIXED])

    structure QD : QUASI_DOM =
      struct
	type dom = SortName
	type name = Name.name
	val name = name
	val pp = pr_SortName_noprefix
      end

    structure Map = QuasiMap(structure IntFinMap = IntFinMap
			     structure QD = QD
			     structure Name = Name
			     structure Crash = Crash
			     structure PP = PP
			     structure Report = Report)

    structure Set = QuasiSet(structure IntFinMap = IntFinMap
			     structure QD = QD
			     structure Name = Name
			     structure Crash = Crash
			     structure PP = PP)

  end;
