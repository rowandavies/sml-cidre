(* Environments for refinement phase.
   See Environments, Module_Environments.    - Rowan
*)

(* SOMETHING WEIRD WAS MERGED IN HERE BY CVS (FROM DECADENCE).  20jan03 *)

functor RefinedEnvironments
		    (structure StrId: STRID
		     structure Ident: IDENT
		       sharing type Ident.strid = StrId.strid
		     structure TyCon: TYCON
		       sharing type TyCon.strid = StrId.strid
		     structure TyName: TYNAME
		       sharing type TyName.tycon = TyCon.tycon
		     structure SortName: SORTNAME
		       sharing type SortName.TyName = TyName.TyName
                       sharing type SortName.sortcon = TyCon.tycon
		     structure StatObject: STATOBJECT
		       sharing type StatObject.TyName = TyName.TyName
		     structure Environments : ENVIRONMENTS
		        sharing type Environments.Type = StatObject.Type
 		        sharing type Environments.ExplicitTyVar = StatObject.ExplicitTyVar
		     structure RefObject: REFOBJECT
  	                sharing type RefObject.Variance = SortName.Variance
		        sharing type RefObject.SortName = SortName.SortName
		        sharing type RefObject.TyName = TyName.TyName
		        sharing type RefObject.TypeFcn = StatObject.TypeFcn
		        sharing type RefObject.TyVar = StatObject.TyVar
                        sharing type RefObject.Type = StatObject.Type                        
                        sharing type RefObject.lab = StatObject.lab
                        sharing RefObject.SortName = SortName                       
		     structure PP: PRETTYPRINT
		       sharing type RefObject.StringTree = PP.StringTree
  	               sharing type Environments.FinMap.StringTree = PP.StringTree
                       sharing type SortName.StringTree = PP.StringTree
		     structure SortedFinMap: SORTED_FINMAP
		       sharing type SortedFinMap.StringTree = PP.StringTree
		     structure FinMapEq: FINMAPEQ
		       sharing type FinMapEq.StringTree = PP.StringTree
		     structure Flags : FLAGS
		     structure Timestamp: TIMESTAMP
  		     structure ListHacks: LIST_HACKS
		     structure Report: REPORT
		       sharing type SortedFinMap.Report
				    = Environments.FinMap.Report
                                    = FinMapEq.Report
                                    = PP.Report
				    = Report.Report                               
		     structure Crash: CRASH
		    ) : REFINED_ENVIRONMENTS =
  struct

    structure RO = RefObject and SO = StatObject (* local abbreviations *)
    structure Rea = RO.Realisation
    structure FinMap = Environments.FinMap

    structure SortName = SortName (* not an abbreviation *)

    type id           = Ident.id      (* was Var.var *)
     and longid       = Ident.longid  (* was Var.longvar *)
    type con          = id            (* was Con.con *)
     and excon        = id            (* was Excon.excon *)
     and strid        = StrId.strid
     and longstrid    = StrId.longstrid
     and tycon        = TyCon.tycon
     and sortcon      = TyCon.tycon
     and longtycon    = TyCon.longtycon
     and longsortcon  = TyCon.longtycon
     and TypeFcn      = StatObject.TypeFcn
     and TyName       = StatObject.TyName
     and SortFcn      = RO.SortFcn
     and SortScheme   = RO.SortScheme
     and Sort         = RO.Sort
     and SortName     = RO.SortName
     and SortVar      = RO.SortVar 
     and Type         = StatObject.Type
     and StringTree   = PP.StringTree
     and ExplicitTyVarEnv = Environments.ExplicitTyVarEnv
     and ExplicitTyVar = Environments.ExplicitTyVar
    type realisation = RO.realisation

    val add_unions_to_constructor_sorts = ref true
    val _ = Flags.add_flag ("add_unions_to_constructor_sorts",
                             "Add unions to constructor sorts?", add_unions_to_constructor_sorts)
    val improve_inversion_principles = ref true
    val _ = Flags.add_flag ("improve_inversion_principles",
                             "Improve inversion principles?", improve_inversion_principles)

    (* Print a conjunction for debugging *)
    fun pr_SortNames sns = 
       ListHacks.stringSep "(" ")" " & " (fn sn => SortName.pr_SortName ("", sn)) sns

    (* Printing of debugging messages *)
    val debug_indent = ref 0
    fun nspaces 0 = ""
      | nspaces n = " " ^ (nspaces (n-1))

    fun add_indent n =  if !Flags.DEBUG_REFOBJECTS then (debug_indent := !debug_indent + n)
                        else ()

    fun out_debug_maybe flag str = 
       if !flag 
         then TextIO.output(TextIO.stdOut,(nspaces (!debug_indent)) ^str() ^"\n")
         else ()

    fun out_debug str = out_debug_maybe Flags.DEBUG_REFOBJECTS str

    fun pr (msg : string, t : PP.StringTree) : unit =
          Report.print (Report.decorate (msg, PP.reportStringTree t))
  
    fun pr_debug msg t = if !Flags.DEBUG_ENVIRONMENTS then  pr (msg, t () )
			 else ()

    (* These should be used more.  *)
    fun impossible s = Crash.impossible ("RefinedEnvironments." ^ s)
    fun noSome NONE s = impossible s
      | noSome (SOME x) s = x

    exception No_match  (* For matching functions *)

   (********
    Structure names
    ********)

(*    type StrName = Timestamp.stamp
      val freshStrName = Timestamp.new
      val bogus_StrName = freshStrName()
*)

    (********
    Refinement Lattices
     represented by intersection map.
    ********)

    datatype RefLattice = REFLATTICE of (SortName * SortName, SortName) FinMapEq.map 

   (********
    The type of items associated with identifiers in a VarEnv
    ********)
    
    datatype VarEnvRange = LONGVAR   of SortScheme
			 | LONGCON   of SortScheme
			 | LONGEXCON of Sort

    (********
    The types of environments
    ********)

    (* Type goals for valdecs, including TyCon-SortName pairs that may be "realised"
       at the actual valdec, but are opaque where the tygoal appears (e.g. in a signature) *)
    type ToRealise  = (tycon, SortName) FinMap.map
    structure ToRea = 
    struct
       val empty : ToRealise = FinMap.empty
       val singleton : tycon * SortName -> ToRealise = FinMap.singleton
       val add = FinMap.add
       val plus = FinMap.plus
       val map = FinMap.composemap
       val layout = FinMap.layoutMap {start="", finish="",sep=", ", eq=" => "} 
				     (PP.LEAF o TyCon.pr_TyCon) SortName.layout
    end

    datatype TyGoals = TYGOALS of (id, ToRealise * SortScheme) FinMap.map

    structure TG = 
      struct
	  val empty : TyGoals = TYGOALS FinMap.empty
	  val singleton : id * (ToRealise * SortScheme) -> TyGoals = 
	      TYGOALS o FinMap.singleton
          fun singleton0 (id, sortsch) = singleton (id, (ToRea.empty, sortsch))
	  fun add (id, (torea, sortsch), TYGOALS s) = TYGOALS (FinMap.add (id, (torea,sortsch), s))
          fun add0 (id, sortsch, TG) = add (id, (ToRea.empty, sortsch), TG)
	  fun plus (TYGOALS s, TYGOALS s') : TyGoals = TYGOALS (FinMap.plus(s, s'))
          fun map f (TYGOALS m) = TYGOALS (FinMap.composemap f m)
	  fun lookup (TYGOALS map) (id : id) : (ToRealise * SortScheme) option = 
            FinMap.lookup map id
	  fun layout (TYGOALS m) = 
 	    FinMap.layoutMap {start="", finish="",sep=", ", eq=" : "} 
		     (PP.LEAF o Ident.pr_id) (PP.layout_pair ToRea.layout RO.layoutSortScheme) m
      end

    datatype Context = CONTEXT of {T: TyNameEnv, E: Env, U: ExplicitTyVarEnv}
         and Env = ENV of {SE: StrEnv, TE: TyEnv, RE: SortEnv, 
			   VE: VarEnv, EE: ExConEnv, TG: TyGoals}
	 and StrEnv = STRENV of (strid, Env) FinMap.map
	 and TyEnv = TYENV of (tycon, SortFcn) FinMap.map
	 and TyNameEnv = TYNAMEENV of (TyName, TyStr) FinMapEq.map
  	 and TyStr = TYSTR of {CE: ConEnv, R:SortNameEnv, mlSN : SortName, 
			       covariant:bool, RL: RefLattice}
	 and SortNameEnv = SORTNAMEENV of (SortName, SortCons) FinMapEq.map
	 and SortCons = SORTCONS of (con, SortFcn list option) SortedFinMap.map (* "of" sorts *)
         and SortEnv = SORTENV of (tycon, SortFcn) FinMap.map
	 and VarEnv = VARENV of (id, VarEnvRange) FinMap.map
	 and ConEnv = CONENV of (con, SortScheme) SortedFinMap.map
	 and ExConEnv = EXCONENV of (excon, Sort) FinMap.map
(*	 and Str = STR of {m: StrName, E: Env} *)


    (* Unused now?  (was: Fix this for modules!) *)
    type Basis = Context

    (*** Layout functions **)
       (* local (pr_srtfcn for layout fns) *)
    fun pr_srtfcn svnames sfcn = #body (RO.pr_SortFcnPRETTY svnames sfcn)


    fun layoutRL (REFLATTICE m) = 
      FinMapEq.layoutMap {start="intersect ", finish="",sep="", eq=" = "} 
        (fn (sn1, sn2) => PP.NODE
               {start = "(", finish = ")", childsep = PP.RIGHT ") with (", indent=1,
		children = [SortName.layout sn1, SortName.layout sn2]})
        SortName.layout m
    fun layoutCE (CONENV m) =
      SortedFinMap.layoutMap {start="", finish="",sep=", ", eq=" : "} 
      (fn id => PP.LEAF ("con " ^ Ident.pr_id id)) RO.layoutSortScheme m

    fun layoutRCsvn svnames (SORTCONS m) =
      let
        fun layout_sfcns sfcns = 
            PP.NODE{start="", finish="", indent=0, childsep=PP.LEFT " | ", 
                    children=map (PP.LEAF o (pr_srtfcn svnames)) sfcns}

	fun doit (id, SOME sfcns) = 
            PP.NODE{start="", finish="", indent=0, childsep=PP.RIGHT " of ",
		    children=[PP.LEAF (Ident.pr_id id), layout_sfcns sfcns] }
          | doit (id, NONE) = PP.LEAF (Ident.pr_id id)

        val RClist = SortedFinMap.Fold (op ::) [] m
        val childnodes = case RClist of [] => [PP.LEAF "<EMPTY>"]
                                      |  _ => map doit RClist
      in
        PP.NODE{start="", finish="", indent=2,
		childsep=PP.LEFT " | ", children=childnodes
	       }
      end
      (* WAS:  (until 14jan03)
      SortedFinMap.layoutMap {start="", finish="",sep=" | ", eq=" of "} 
        (fn id => PP.LEAF ("  " ^ Ident.pr_id id)) 
        (PP.layout_opt (PP.layout_list RO.layoutSortFcn))
	m  *)

    val layoutRC = layoutRCsvn (RO.newSVNames ())

    fun layoutRCsvnIndented svnames RC =
        PP.NODE{start="", finish="", indent=2,
		childsep=PP.NOSEP, children=[layoutRCsvn svnames RC]
	       }

    fun layoutR (SORTNAMEENV m) =  (* USED BY TopLevelReport! *)
      let val svnames = RO.newSVNames ()   (* FIX:   RefObject.TVNamesOfSVNames SVnames *)
          fun pr_sn sn = pr_srtfcn svnames (RO.SortName_in_SortFcn sn)
      in
        FinMapEq.layoutMap {start="", finish="",sep="  ", eq=" = "}
                           (fn sn => PP.LEAF (pr_sn sn))
                           (layoutRCsvnIndented svnames)
                           m
      end

    fun layoutVE (VARENV m) =
          let 
	    fun layout_id id = 
	      (fn s => PP.LEAF (s ^ Ident.pr_id id))
	      (case FinMap.lookup m id 
		 of SOME(LONGVAR _) => "val " 
		  | SOME(LONGCON _) => "con "
		  | SOME(LONGEXCON _) => "excon "
		  | NONE => Crash.impossible "RefinedEnvironments: layoutVE.format_id>")
		  
	    fun layoutRng(LONGVAR sigma) = RO.layoutSortScheme sigma
	      | layoutRng(LONGCON sigma) = RO.layoutSortScheme sigma
	      | layoutRng(LONGEXCON tau) = RO.layoutSort tau
	  in
	    FinMap.layoutMap {start="", finish="",sep=", ", eq=" : "} layout_id layoutRng m
	  end

    fun layoutSE (STRENV m) = 
      FinMap.layoutMap {start="", finish="",sep=", ", eq=" : "} 
        (fn s => PP.LEAF ("structure " ^ StrId.pr_StrId s)) layoutEnv m
    and layoutTE (TYENV m) = 
      FinMap.layoutMap {start="", finish="",sep=", ", eq=" : "} 
        (fn t => PP.LEAF ("tycon " ^ TyCon.pr_TyCon t)) RO.layoutSortFcn m
    and layoutRE (SORTENV m) = 
      FinMap.layoutMap {start="", finish="",sep=", ", eq=" : "} 
        (fn t => PP.LEAF ("sortcon " ^ TyCon.pr_TyCon t)) RO.layoutSortFcn m
    and layoutEE (EXCONENV m) = 
      FinMap.layoutMap {start="", finish="",sep=", ", eq=" : "} 
        (fn id => PP.LEAF ("excon " ^ Ident.pr_id id)) RO.layoutSort m
    and layoutTG TG = TG.layout TG
    and layoutEnv (ENV{SE, TE, RE, VE, EE, TG}) =
          PP.NODE {start="{", finish="}", indent=1,
		   children=[layoutSE SE, layoutTE TE, layoutRE RE, layoutVE VE, 
			     layoutEE EE, layoutTG TG],
		   childsep = PP.NOSEP}

    fun layoutTyStr (TYSTR {CE, R, mlSN, covariant, RL}) =
          PP.NODE {start="(", finish=")", indent=1,
		   children=[layoutCE CE, layoutR R,
			     PP.LEAF ("defaultRefinement " ^ SortName.pr_SortName ("", mlSN)),
                             PP.LEAF ("covariant " ^ Bool.toString covariant),
			     layoutRL RL], 
		   childsep=PP.RIGHT ", "}
    fun layoutT (TYNAMEENV m) = 
        FinMapEq.layoutMap {start="", finish="",sep=", ", eq=" => "} 
          (fn t => PP.LEAF ("tyname " ^ TyName.pr_TyName t)) layoutTyStr m
    fun layoutC (CONTEXT {T, E, U}) = 
        PP.layout_pair layoutT layoutEnv (T, E)


    (********
    Refinement Lattices
    ********)

     fun sn2_eq ((sn11, sn12), (sn21, sn22)) = 
	 (SortName.eq (sn11, sn21)) andalso (SortName.eq (sn12, sn22))

     val emptyRL = REFLATTICE FinMapEq.empty (* Not in signature, as yet.  Only used in "on_RL". *)
     fun isSingleRL (REFLATTICE m) = FinMapEq.isEmpty m  (* Not in signature, as yet. *)

     fun singleRL sortname = REFLATTICE FinMapEq.empty

     fun conjoinRL' (REFLATTICE map) (sn1, sn2) =
       case SortName.eq (sn1, sn2)
	 of true => SOME sn1 
          | false => case SortName.leq (sn1, sn2)
		       of true => FinMapEq.lookup sn2_eq map (sn1, sn2)
		        | false => FinMapEq.lookup sn2_eq map (sn2, sn1)

     fun prRL (REFLATTICE m) = 
       String.concat 
              (map (fn ((sn1, sn2), sn3) => 
		      "(" ^ (SortName.pr_SortName ("", sn1)) ^ " ^ " ^
		            (SortName.pr_SortName ("", sn2)) ^ " = " ^
		            (SortName.pr_SortName ("", sn3)) ^ ")")
                   (FinMapEq.list m))

     fun conjoinRL RL (sn1, sn2) =
       case conjoinRL' RL (sn1, sn2)
         of SOME sortname => sortname
          | NONE => Crash.impossible ("RefinedEnvironments.conjoinRL " ^ 
	                              (SortName.pr_SortName ("", sn1)) ^ ", " ^
	                              (SortName.pr_SortName ("", sn2)) ^ 
				      " RL = " ^ (prRL RL))


     fun conjoinListRL' _  [] = Crash.impossible "RefinedEnvironments.conjoinListRL'"
       | conjoinListRL' RL (sortname::sortnames) =
         List.foldl (fn (x, SOME r) => conjoinRL' RL (x, r)
                      | (_, NONE) => NONE)
                     (SOME sortname)
                     sortnames

     fun conjoinListRL RL sortnames =
       case conjoinListRL' RL sortnames
	 of SOME sortname => sortname
          | NONE => Crash.impossible "RefinedEnvironment.conjoinListRL"

     fun subSortNameRL RL (sortname, sortname') =
       SortName.eq (conjoinRL RL (sortname, sortname'), sortname)

     (* warning - this doesn't complete the lattice *)
     fun RL_plus_RL (REFLATTICE m, REFLATTICE m') = 
	 REFLATTICE (FinMapEq.plus sn2_eq (m, m'))

     fun addRL (REFLATTICE m, (sn1, sn2), sn3) =
       case SortName.eq (sn1, sn2)
	 of true => REFLATTICE m
          | false => case SortName.leq (sn1, sn2)
	               of true => REFLATTICE (FinMapEq.add sn2_eq ((sn1, sn2), sn3, m))
		        | false => REFLATTICE (FinMapEq.add sn2_eq ((sn2, sn1), sn3, m))
       
     val bogusRL = REFLATTICE FinMapEq.empty

(*     fun RLmap (f : (SortName * SortName) * SortName -> (SortName * SortName) * SortName)
               (REFLATTICE m) =
          FinMapEq.Fold (fn (((sn1, sn2), sn3), m) => 
                             f ((sn1, sn2), sn3, m)
                              ((on_SortName sn1, on_SortName sn2), onSortName sn3)
              FinMapEq.empty
              m
*)



    (********
    TypeName Environments
    ********)
    
    val emptyT: TyNameEnv = TYNAMEENV FinMapEq.empty

    val singleT: TyName * TyStr -> TyNameEnv = TYNAMEENV o FinMapEq.singleton

    (* The TyStr's in t' should be strictly "richer" than t *)
    fun T_plus_T(TYNAMEENV t, TYNAMEENV t'): TyNameEnv =
      TYNAMEENV(FinMapEq.plus TyName.eq (t, t'))

   fun Tmap1 (f: TyStr -> TyStr) (TYNAMEENV m): TyNameEnv =
      TYNAMEENV(FinMapEq.composemap f m)

    fun Tfold (f: (TyStr * 'a) -> 'a) (start: 'a) (TYNAMEENV map): 'a = 
      FinMapEq.fold f start map

    fun TFold (f: ((TyName * TyStr) * 'a) -> 'a) (start: 'a) (TYNAMEENV map): 'a = 
      FinMapEq.Fold f start map

   fun Tmap (f: TyName * TyStr -> TyName * TyStr) T : TyNameEnv =
      TFold (fn (from_to, acc) => T_plus_T (singleT (f from_to), acc)) emptyT T

    fun Tdom(TYNAMEENV map) =
      Set.list(FinMapEq.dom TyName.eq map)	(* MEMO: TIDY UP! *)

    fun lookupT(TYNAMEENV m, tyname: TyName.TyName): TyStr option =
      FinMapEq.lookup TyName.eq m tyname

(*    fun removeT(TYNAMEENV m, tynames: TyName.TyName list): TyNameEnv =
        FinMapEq.remove TyName.eq m tynames
*)

    (* Conjoin using T.  Return NONE if conjunction not defined in T. *)
    fun conjSortNameT' T (sortname, sortname') =
       case SortName.eq (sortname, sortname')
         of true => SOME sortname
          | false => 
            (case lookupT (T, SortName.tyname sortname)
	       of SOME (TYSTR {RL, ...}) => conjoinRL' RL (sortname, sortname')
	        | NONE => NONE
            )

    fun conjSortNameT T (sortname, sortname') =
       case conjSortNameT' T (sortname, sortname')
         of SOME conj_sortname => conj_sortname
          | NONE => Crash.impossible ("RefinedEnvironments.conjSortNameT: " 
	                              ^ (pr_SortNames [sortname, sortname']))

    fun subSortNameT T (sortname, sortname') =
       SortName.eq (conjSortNameT T (sortname, sortname'), sortname)

    (********
    SortCons Environments
    ********)
    
    val emptyRC: SortCons = SORTCONS SortedFinMap.empty

    fun singleRC (con, NONE) = SORTCONS (SortedFinMap.singleton (con, NONE))
      | singleRC (con, SOME sortfcn) = 
        SORTCONS (SortedFinMap.singleton (con, SOME [sortfcn]))

    fun RC_plus_RC (SORTCONS m, SORTCONS m'): SortCons =
      let
	fun conjoin (NONE, NONE) = NONE
	  | conjoin (SOME sfl1, SOME sfl2) = SOME (sfl1 @ sfl2)
	  | conjoin _ = Crash.impossible "RefinedEnvironment.RC_plus_RC"
      in
	SORTCONS(SortedFinMap.mergeMap conjoin m m')
      end

    fun RCmap (f: SortFcn list option -> SortFcn list option) (SORTCONS m)
            : SortCons =
      SORTCONS(SortedFinMap.composemap f m)

    fun RCfold (f: (SortFcn list option * 'a) -> 'a) (start: 'a) (SORTCONS map): 'a = 
      SortedFinMap.fold f start map

    fun RCFold (f: ((con * SortFcn list option) * 'a) -> 'a) (start: 'a) 
              (SORTCONS map): 'a = 
      SortedFinMap.Fold f start map

    fun RCdom(SORTCONS map) = SortedFinMap.domSORTED map

    fun lookupRC(SORTCONS m, con: id): SortFcn list option option =
      SortedFinMap.lookup m con

    (* For use in intersectRC below *)
    fun productConj _ ([], _) = []
      | productConj conjSN (sortfcn::sortfcns, sortfcns') =
        (map (fn ss => RO.conjSortFcn conjSN (sortfcn, ss)) sortfcns')
        @ (productConj conjSN (sortfcns, sortfcns'))

    (* Create RC for an intersection of two RCs.  May include non-maximal elements,
       and may create syntactic conjunctions.  *)
    fun intersectRC conjSN (SORTCONS m, SORTCONS m') : SortCons =
      SORTCONS
        (SortedFinMap.Fold
	  (fn ((con, sortfcns_opt), mb) =>
	    case SortedFinMap.lookup m' con
	      of NONE => mb
	       | SOME NONE => SortedFinMap.add Ident.< (con, NONE, mb)
	       | SOME (SOME sortfcns') =>
		 (case sortfcns_opt
		    of NONE => Crash.impossible "RefinedEnvironments.intersectRC"
		     | SOME sortfcns =>
		       SortedFinMap.add Ident.< 
		         (con, SOME (productConj conjSN (sortfcns, sortfcns')), mb)
                 )
          )
	  SortedFinMap.empty
	  m
	)

    (* Intersect all in sorts1 with all in sorts2, removing non-maximal elements. *)
    fun productConj_sorts _ ([], _) = []
      | productConj_sorts conjSN (head_sort1::tail_sorts1, sorts2) =
          let val res_tail = productConj_sorts conjSN (tail_sorts1, sorts2)
              val conjSN2 = (SOME o conjSN)
              val mkconj_sort = RO.mkSortConj conjSN2
              val res_head = map (fn s2 => mkconj_sort (head_sort1, s2)) sorts2
              fun conjSNprint args = (print "HERE"; conjSN args)
              val res_head_non_empty = List.filter (fn s => not (RO.emptySort conjSNprint s)) res_head
          in
              foldl (RO.addSortToMaximals conjSN) res_tail res_head_non_empty
          end

    (* Intersect all in sortfcns1 with all in sortfcns2, removing non-maximal elements. *)
    fun productConj_sortfcns _ ([], _) = []
      | productConj_sortfcns conjSN (h_sfcn1::t_sfcns1, sfcns2) =  
        let val (svs, h_srt1) = RO.applySortFcnVars h_sfcn1
	    val svsorts = map RO.mkSortSortVar svs
            fun sfcn_to_sort sf = RO.applySortFcn (sf, svsorts)
            val sorts1 = h_srt1::(map sfcn_to_sort t_sfcns1)
            val sorts2 = map sfcn_to_sort sfcns2
            val prod_sorts = productConj_sorts conjSN (sorts1, sorts2)
        in
            map (fn srt => RO.mkSortFcn(svs, srt)) prod_sorts
        end

    (* Create the RC for an intersection of two existing RCs, removing non-maximal elements. *)
    (* It is also used to improve inv. principles when one RC is contained in the other. *)
    fun intersectRCmax conjSN (SORTCONS m, SORTCONS m') : SortCons =
      SORTCONS
        (SortedFinMap.Fold  (* fold over m, build new map *)
	  (fn ((con, sortfcns_opt), mb) =>
	    case SortedFinMap.lookup m' con
	      of NONE => mb
	       | SOME NONE => SortedFinMap.add Ident.< (con, NONE, mb)
	       | SOME (SOME sortfcns') =>
		 (case sortfcns_opt
		    of NONE => Crash.impossible "RefinedEnvironments.intersectRCnmax"
		     | SOME sortfcns =>
		       SortedFinMap.add Ident.< 
		         (con, SOME (productConj_sortfcns conjSN (sortfcns, sortfcns')), mb)
                 )
          )
	  SortedFinMap.empty
	  m
	)

    fun RC_to_CE T (sortname, SORTCONS sortcons) =
      let
	val (sortvars, sort) = 
	  RO.unSortFcn (RO.SortName_in_SortFcn sortname)
	val sortscheme = RO.mkSortScheme(sortvars, sort)
        val sortsvs = map RO.mkSortSortVar sortvars
	fun mk_arrow sfcn = RO.mkSortArrow (RO.applySortFcn (sfcn, sortsvs), sort)
        fun mkSortConj (sort1, sort2) = RO.mkSortConj (conjSortNameT' T) (sort1, sort2)
      in
	CONENV (SortedFinMap.composemap
		  (fn NONE => sortscheme  (* No "of" argument for this constructor *)
		    | SOME sfcns =>       (* c :> All sortvars. sort1 -> sortname & ...  *)
		      RO.mkSortScheme (sortvars,  
				       ListHacks.foldl' mkSortConj (map mk_arrow sfcns)))
		  sortcons
	       )
      end

    (********
    SortName Environments
    ********)
    
    val emptyR: SortNameEnv = SORTNAMEENV FinMapEq.empty

    val singleR: SortName * SortCons -> SortNameEnv = SORTNAMEENV o FinMapEq.singleton

    fun R_plus_R(SORTNAMEENV t, SORTNAMEENV t'): SortNameEnv =
      SORTNAMEENV(FinMapEq.plus SortName.eq (t, t'))

    fun addR(SORTNAMEENV t, sortname, sortcons): SortNameEnv =  (* Not in REFINED_ENV yet. *)
      SORTNAMEENV(FinMapEq.add SortName.eq (sortname, sortcons, t))

    fun Rmap (f: SortCons -> SortCons) (SORTNAMEENV m): SortNameEnv =
      SORTNAMEENV(FinMapEq.composemap f m)

    fun Rfold (f: (SortCons * 'a) -> 'a) (start: 'a) (SORTNAMEENV map): 'a = 
      FinMapEq.fold f start map

    fun RFold (f: ((SortName * SortCons) * 'a) -> 'a) (start: 'a) 
              (SORTNAMEENV map): 'a = 
      FinMapEq.Fold f start map

    fun Rdom(SORTNAMEENV map) =
      Set.list(FinMapEq.dom SortName.eq map)

    fun lookupR(SORTNAMEENV m, SortName: SortName.SortName): SortCons option =
      FinMapEq.lookup SortName.eq m SortName

    (* Match R1 and R2: check if they have equivalent elements, judged by 
       comparing the conjuncts of the SortNames.
       This isn't quite right: a sortname may have a different conjuncts field
       depending on the order the lattice is created.  *)
    fun matchR (R1, R2) = 
      let val Rdom1 = Rdom R1
	  val Rdom2 = Rdom R2   (* The RC's in the range should be empty.  Check this? *)
          val _ = if List.length Rdom1 <> List.length Rdom2 then raise No_match else ()
          fun match_sortname (sn1, sn2) =
              let val cnjs1 = SortName.conjuncts sn1
                  val cnjs2 = SortName.conjuncts sn2
                  fun tycon_of sortname = TyName.tycon (SortName.tyname sortname)
              in
                (case (cnjs1, cnjs2) of ([sortcon1], [sortcon2]) =>  (* match default sortnames *)
                                          (sortcon1 = tycon_of sn1 andalso sortcon2 = tycon_of sn2)
                                      | _ => false)
                orelse ListHacks.eqSet (cnjs1, cnjs2)
              end
          fun dummyConjSN _ = Crash.impossible "RefinedEnvironments.matchR:dummyConjSN"
      in
	foldl (fn (sn1, phi) => case List.find (fn sn2 => match_sortname (sn1, sn2)) Rdom2
		                 of NONE => let val _ = pr_debug "No_match(2): R1 = "
                                                                 (fn () => layoutR R1)
                                                val _ = pr_debug "No_match(2): R2 = "
                                                                 (fn () => layoutR R2)
                                            in   raise No_match
                                            end
		                  | SOME sn3 => Rea.oo dummyConjSN (* only SortNames in phi's *)
                                                       phi (Rea.singleSN (sn1, sn3))  )
	      Rea.Id Rdom1
      end
	     
	    
    (********
    Structures
    ********)

(*    fun mkStr(m: StrName, E: Env): Str = STR {m = m, E = E}
      fun unStr(STR {m, E}): StrName * Env = (m, E)
*)
    (********
    Variable environments
    ********)

    val emptyVE: VarEnv = VARENV FinMap.empty

    fun VEdom (VARENV m) = FinMap.dom m

    val bogus_VE = emptyVE

    val singleVE: id * VarEnvRange -> VarEnv = VARENV o FinMap.singleton

    fun singleVarVE(id: id, sortscheme: SortScheme): VarEnv =
      VARENV(FinMap.singleton(id, LONGVAR sortscheme))

    fun removeVE (VARENV v,id) = 
      VARENV(case FinMap.remove(id,v) of NONE => v
                                       | SOME v' => v')

    fun VE_plus_VE (VARENV v, VARENV v'): VarEnv =
      VARENV(FinMap.plus (v, v'))

    (* Need T here to do conjunctions in constructor sorts.  *)
    fun VE_plus_VE_conjCons (T : TyNameEnv) (VARENV v, VARENV v'): VarEnv =  (* Slow *)
      let
        val conjSortName = conjSortNameT' T
	fun conjoin (LONGCON s1, LONGCON s2) =
	    LONGCON (RO.plus_SortScheme conjSortName (s1, s2))
	  | conjoin (vr1, vr2) = vr2
      in
	VARENV(FinMap.mergeMap conjoin v v')
      end

    (* Conjoin sort schemes for variables in two VEs *)
    fun VE_conjoin_vars (T : TyNameEnv) (VARENV v, VARENV v'): VarEnv =  (* Slow *)
      let
        val conjSortName = conjSortNameT' T
	fun conjoin (LONGVAR ss1, LONGVAR ss2) =
	      LONGVAR (RO.plus_SortScheme conjSortName (ss1, ss2))
	  | conjoin (vr1, vr2) = vr2
      in
	VARENV(FinMap.mergeMap conjoin v v')
      end

    fun VEmap (f: VarEnvRange -> VarEnvRange) (VARENV m): VarEnv =
      VARENV(FinMap.composemap f m)

    fun VEfold (f: (VarEnvRange * 'a) -> 'a)
      	       (start: 'a)
	       (VARENV map): 'a = 
      FinMap.fold f start map

    fun VEFold (f: ((id * VarEnvRange) * 'a) -> 'a)
      	       (start: 'a)
	       (VARENV map): 'a = 
      FinMap.Fold f start map

    fun lookupVE(VE: VarEnv, id: id): VarEnvRange option =
      let
	val VARENV m = VE
      in
	FinMap.lookup m id
      end

    fun closVE(VE, sortvars) = 
      VEmap (fn (LONGVAR sscheme) => LONGVAR (RO.Close_SortScheme(sscheme, sortvars))
	      | _ => Crash.impossible "RefinedEnvironments.closVE")
	    VE

    fun subVE T (VE1, VE2) =   (* sorts of variables in VE1 all subsorts of those in VE2 *)
      VEFold (fn ((id, LONGVAR sscheme1), r) =>     (* DOESN'T check emptiness *)
		r andalso                           (* DOESN'T assume vars in same order *)
		(case lookupVE(VE2, id) 
		   of SOME(LONGVAR sscheme2) =>
		      let
			val (svs, srt1) = RO.instance_vars sscheme1
			val srt2 = RO.instance(sscheme2, map RO.mkSortSortVar svs)
		      in
			RO.subSort (conjSortNameT T) (srt1, srt2)
		      end
		    | _ => Crash.impossible "RefinedEnvironments.subVE")
	       | _ => true)
	     true
	     VE1

    (* Inefficient! *)
    fun eqVE T (VE1, VE2) = subVE T (VE1, VE2) andalso subVE T (VE2, VE1)
		      

    (********
    Constructor environments
    ********)

    val emptyCE: ConEnv = CONENV SortedFinMap.empty
    val singleCE: con * SortScheme -> ConEnv = CONENV o SortedFinMap.singleton

    fun CE_plus_CE (CONENV m, CONENV m'): ConEnv =
        CONENV(SortedFinMap.plus Ident.< (m, m'))

    fun CE_plus_CE_conjCons (T : TyNameEnv) (CONENV m, CONENV m'): ConEnv =
      let
	val conjSortName = conjSortNameT' T
        val plusSortScheme = RO.plus_SortScheme conjSortName
      in
	CONENV(SortedFinMap.mergeMap plusSortScheme m m')
      end

    fun domCE(CONENV m): con list =
      SortedFinMap.domSORTED m

    fun CEmap (f: SortScheme -> SortScheme) (CONENV m): ConEnv =
      CONENV(SortedFinMap.composemap f m)

    fun CEfold (f: SortScheme * 'a -> 'a) (start: 'a) (CONENV map): 'a =
      SortedFinMap.fold f start map

    fun CEFold (f: (con * SortScheme) * 'a -> 'a) (start: 'a) (CONENV map): 'a =
      SortedFinMap.Fold f start map

    fun isemptyCE(CONENV m): bool =
      SortedFinMap.isEmpty m

    fun equalCE (T :TyNameEnv) (CONENV m1, CONENV m2): bool =
      let
	val dom1   = SortedFinMap.domSORTED m1
	and dom2   = SortedFinMap.domSORTED m2
	and range1 = SortedFinMap.rangeSORTED m1
	and range2 = SortedFinMap.rangeSORTED m2
      in
	(dom1 = dom2) andalso 
	List.all
	  (fn (sigma1,sigma2) => 
	      RO.equal_SortScheme (conjSortNameT T) (sigma1,sigma2)) 
	  (ListPair.zip(range1,range2))
      end

    fun isEmptyCE(CONENV map) = SortedFinMap.isEmpty map


    (********
    Type structures
    ********)

    fun mkTyStr(CE: ConEnv, R:SortNameEnv, mlSN: SortName,
		covariant:bool, RL : RefLattice): TyStr =
      TYSTR {CE = CE, R = R, mlSN = mlSN, covariant=covariant, RL = RL}

    fun unTyStr(TYSTR {CE, R, mlSN, covariant, RL}) 
         : ConEnv * SortNameEnv * SortName * bool * RefLattice = 
      (CE, R, mlSN, covariant, RL)

    fun CE_of_TyStr(TYSTR {CE, ...}): ConEnv = CE

    fun R_of_TyStr(TYSTR {R, ...}): SortNameEnv = R

    fun mlSN_of_TyStr(TYSTR {mlSN, ...}): SortName = mlSN

    fun RL_of_TyStr(TYSTR {RL, ...}): RefLattice = RL

    fun covariant_of_TyStr(TYSTR{covariant, ...}):bool = covariant

    (* generate realiser that turns tystr1 into tystr2 *)
    fun matchTyStr (tystr1, tystr2) : realisation = 
      let val (CE1, R1, mlSN1, covariant1, RL1) = unTyStr tystr1
	  val (CE2, R2, mlSN2, covariant2, RL2) = unTyStr tystr2
          val _ = if covariant1 <> covariant2 then raise No_match else ()
          val phi = matchR (R1, R2)  (* might raise No_match *)
          (* FIX: should apply realiser to RLs, and compare them (and CE if not empty?) *)
          (* also check  rea mlSN1 = mlSN2 *)
      in
	phi
      end

(*    val bogus_TyStr = TYSTR{CE=CONENV SortedFinMap.empty,
			    R=SORTNAMEENV FinMap.empty,
                            mlSN = 
			    covariant=false,
			    RL=bogusRL
			   }
*)
    (********
    Exception constructor environments
    ********)

    val emptyEE: ExConEnv = EXCONENV FinMap.empty

    fun EEdom(EXCONENV m) = FinMap.dom m
      
    fun singleEE(excon: excon, sort: Sort): ExConEnv =
      EXCONENV(FinMap.singleton(excon, sort))

    fun EE_plus_EE(EXCONENV e, EXCONENV e'): ExConEnv =
      EXCONENV(FinMap.plus(e, e'))

    fun EEmap (f: Sort -> Sort) (EXCONENV m): ExConEnv =
      EXCONENV(FinMap.composemap f m)

    fun EEfold (f: (Sort * 'a) -> 'a) (start: 'a) (EXCONENV map): 'a = 
      FinMap.fold f start map

    fun EEFold (f: ((excon * Sort) * 'a) -> 'a) (start: 'a) (EXCONENV map): 'a = 
      FinMap.Fold f start map

    fun VE_of_EE(EE: ExConEnv): VarEnv =
      let
	fun f((excon, sort), m) =
	  FinMap.add(excon, LONGEXCON sort, m)
      in
	VARENV(EEFold f FinMap.empty EE)
      end

    (* not used by RefDec anymore - could remove EE, following ML/Kit 3.0.  *)
    fun lookupEE(EXCONENV m, excon: excon): Sort option =
      FinMap.lookup m excon

    (********
    Type environments
    ********)

    val emptyTE: TyEnv = TYENV FinMap.empty

   val singleTE: tycon * SortFcn -> TyEnv = TYENV o FinMap.singleton

    fun TE_plus_TE(TYENV t, TYENV t'): TyEnv =
      TYENV(FinMap.plus(t, t'))

    fun TEmap (f: SortFcn -> SortFcn) (TYENV m): TyEnv =
      TYENV(FinMap.composemap f m)

    fun TEfold (f: (SortFcn * 'a) -> 'a) (start: 'a) (TYENV map): 'a = 
      FinMap.fold f start map

    fun TEFold (f: ((tycon * SortFcn) * 'a) -> 'a) (start: 'a) (TYENV map): 'a = 
      FinMap.Fold f start map

    fun TEdom(TYENV map) =
      EqSet.list(FinMap.dom map)	(* MEMO: TIDY UP! *)

    fun lookupTE(TYENV m, tycon: TyCon.tycon): SortFcn option =
      FinMap.lookup m tycon

    (********
    Sort environments
    ********)

    val emptyRE: SortEnv = SORTENV FinMap.empty

    val singleRE: sortcon * SortFcn -> SortEnv = SORTENV o FinMap.singleton

    fun RE_plus_RE(SORTENV t, SORTENV t'): SortEnv =
      SORTENV(FinMap.plus(t, t'))

    fun REmap (f: SortFcn -> SortFcn) (SORTENV m): SortEnv =
      SORTENV(FinMap.composemap f m)

    fun REfold (f: (SortFcn * 'a) -> 'a) (start: 'a) (SORTENV map): 'a = 
      FinMap.fold f start map

    fun REFold (f: ((sortcon * SortFcn) * 'a) -> 'a) 
               (start: 'a) (SORTENV map): 'a = 
      FinMap.Fold f start map

    fun REdom(SORTENV map) =
      EqSet.list(FinMap.dom map)	(* MEMO: TIDY UP! *)

    fun lookupRE(SORTENV m, sortcon: sortcon): SortFcn option =
      FinMap.lookup m sortcon

    fun convertREtoTE (SORTENV m) = TYENV m


    (********
    Structure environments
    ********)

    val emptySE: StrEnv = STRENV FinMap.empty

    val singleSE: strid * Env -> StrEnv = STRENV o FinMap.singleton

    fun SE_plus_SE (STRENV s, STRENV s'): StrEnv =
      STRENV(FinMap.plus(s, s'))

    fun SEfold (f: Env * 'a -> 'a) (start: 'a) (STRENV map): 'a = 
      FinMap.fold f start map

    fun SEFold (f: ((strid * Env) * 'a) -> 'a) (start: 'a) (STRENV map): 'a = 
      FinMap.Fold f start map

    fun SEdom (STRENV map) =
      EqSet.list(FinMap.dom map)	(* MEMO: TIDY UP! *)

    fun SEmap (f: Env -> Env) (STRENV map) =
      STRENV(FinMap.composemap f map)

    fun lookupSE (STRENV map, strid: strid): Env option =
      FinMap.lookup map strid


    (********
    Environments
    ********)

    val emptyE: Env = ENV {SE=emptySE, TE=emptyTE, RE=emptyRE, 
			   VE=emptyVE, EE=emptyEE, TG=TG.empty}

    val bogus_Env = emptyE

   (* and now the bogus structure: *)

(*    val bogus_Str = mkStr(freshStrName(), bogus_Env) *)

    fun VE_in_E (VE: VarEnv): Env =
      ENV{SE = emptySE, TE = emptyTE, RE = emptyRE, VE = VE, EE = emptyEE, TG=TG.empty}

    fun E_plus_E (ENV{SE, TE, RE, VE, EE, TG}, 
		  ENV{SE=SE', TE=TE', RE=RE', VE=VE', EE=EE', TG=TG'}) =
      ENV{SE=SE_plus_SE(SE, SE'), TE=TE_plus_TE(TE, TE'),
	  RE=RE_plus_RE(RE, RE'), VE=VE_plus_VE (VE, VE'), 
	  EE=EE_plus_EE(EE, EE'), TG=TG.plus (TG, TG')
	 }

    fun SE_in_E (SE: StrEnv): Env =
      ENV {SE = SE, TE = emptyTE, RE = emptyRE, VE = emptyVE, EE = emptyEE, TG=TG.empty}

    fun TE_in_E (TE: TyEnv): Env =
      ENV {SE = emptySE, TE = TE, RE = emptyRE, VE = emptyVE, EE = emptyEE, TG=TG.empty}

    fun TE_of_E (ENV{TE,...}) = TE

    fun RE_in_E (RE: SortEnv): Env =
      ENV {SE = emptySE, TE = emptyTE, RE = RE, VE = emptyVE, EE = emptyEE, TG=TG.empty}

    fun RE_of_E (ENV{RE,...}) = RE

    fun VE_and_TE_in_E (VE: VarEnv, TE: TyEnv): Env =
      ENV {SE = emptySE, TE = TE, RE = emptyRE, VE = VE, EE = emptyEE, TG=TG.empty}

    fun VE_and_EE_in_E (VE: VarEnv, EE: ExConEnv): Env =
      ENV {SE = emptySE, TE = emptyTE, RE = emptyRE, VE = VE, EE = EE, TG=TG.empty}

    fun TG_in_E (TG: TyGoals): Env =
      ENV {SE=emptySE, TE=emptyTE, RE=emptyRE, VE=emptyVE, EE=emptyEE, TG=TG}
    fun TG_of_E (ENV {TG, ...}) = TG

    fun mkEnv (SE, TE, RE, VE, EE): Env =
      ENV {SE = SE, TE = TE, RE = RE, VE = VE, EE = EE,  TG=TG.empty}

    fun unEnv (ENV {SE, TE, RE, VE, EE, TG}) =
      (SE, TE, RE, VE, EE)

    fun VE_of_E (ENV{VE,...}) = VE

    fun lookupE_strid (ENV {SE, ...}, strid: strid): Env option =
      lookupSE(SE, strid)

    fun lookupE_tycon (ENV {TE, ...}, tycon: tycon): SortFcn option =
      lookupTE(TE, tycon)

    (********
    Contexts
    ********)

    val emptyU = Environments.C.U_empty
    val emptyC = CONTEXT {T=emptyT, E=emptyE, U=emptyU}

    fun ExplicitTyVar_lookup (CONTEXT {U, ...}) ExplicitTyVar : Type
      = Environments.C.ExplicitTyVar_lookupU U ExplicitTyVar

    fun mkC (T, E): Context =
      CONTEXT {T=T, E=E, U=emptyU}

    fun C_plus_U (CONTEXT {T, E, ...}, U : ExplicitTyVarEnv): Context = 
      CONTEXT {T = T, E = E, U = U}

    fun U_of_C (CONTEXT {U=Environments.EXPLICITTYVARENV m, ...}) = 
        map #1 (FinMap.list m)

    fun C_plus_VE (CONTEXT {T, E, U}, VE': VarEnv): Context =
      let
	val ENV {SE, TE, RE, VE, EE, TG} = E
      in
	CONTEXT
	{T = T, U = U, E = ENV {SE = SE, TE = TE, RE = RE, 
			        VE = VE_plus_VE (VE, VE'), EE = EE, TG = TG} }
      end

    fun C_plus_E (CONTEXT {T, E, U}, E': Env): Context =
      CONTEXT {T = T, E = E_plus_E (E, E'), U=U }

    fun C_plus_U' (CONTEXT {U=Environments.EXPLICITTYVARENV U1, E, T},
		   ExplicitTyVars : ExplicitTyVar list) : SortVar list * Context =
	    let val (l,U2) = foldr (fn (ExplicitTyVar, (l,m)) => 
				   let val tv = SO.TyVar.from_ExplicitTyVar ExplicitTyVar
				       val ty = SO.Type.from_TyVar tv
				   in ((RO.SVofTV tv)::l, FinMap.add (ExplicitTyVar, ty, m))
				   end) ([], U1) ExplicitTyVars
	    in
	      (l, CONTEXT {U=Environments.EXPLICITTYVARENV U2, E=E, T=T})
	    end


    fun eraseTG (CONTEXT {T, E, U}): Context =
      let
	val ENV {SE, TE, RE, VE, EE, TG} = E
      in
	CONTEXT
	{T = T, E = ENV {SE = SE, TE = TE, RE = RE, 
			 VE = VE, EE = EE, TG = TG.empty}, U=U }
      end

    fun C_plus_C (CONTEXT {T, E, U}, CONTEXT{T=T', E=E', U=U'}): Context =
      let 
	val newT = T_plus_T(T,T')
      in
	CONTEXT {T = newT, E = E_plus_E (E, E'), U=U'}
      end

    fun C_plus_T (CONTEXT {T, E, U}, T'): Context =
      CONTEXT {T = T_plus_T(T,T'), E = E, U = U}

    fun T_of_C (CONTEXT {T, ...}) = T

    fun E_in_C E =
      CONTEXT{T = emptyT, E = E, U=emptyU}

    fun E_of_C (CONTEXT {E, ...}) = E

    fun VE_of_C C = VE_of_E (E_of_C C)

    fun conjSortNameC C = conjSortNameT (T_of_C C)



(*    fun onC (S: Substitution, CONTEXT {T, E}) =
        CONTEXT {T = T, E = onE(S, E)}
*)
    (********
    Function which traverses down structure environments
    ********)

    fun Traverse E [] = SOME(E)
      | Traverse (ENV{SE = STRENV m, ...}) (strid :: rest) =
	case (FinMap.lookup m strid) of
	  SOME E => Traverse E rest
	| NONE => NONE

   (********
    Functions to lookup in a context
    ********)

    fun Lookup_longid (CONTEXT{E, ...}, longid) =
	let
	  val (strid_list, id) = Ident.decompose longid
	in
	  case Traverse E strid_list
	    of NONE => NONE
	     | SOME (ENV {VE, ...}) => lookupVE(VE, id)
	end

    fun Lookup_longtycon(CONTEXT{E, ...}, longtycon) =
      let
	val (strid_list, tycon) = TyCon.explode_LongTyCon longtycon
      in
	case (Traverse E strid_list) of
	  NONE => NONE
	| SOME(ENV {TE, ...}) => lookupTE(TE, tycon)
      end

    fun Lookup_longsortcon(CONTEXT{E, ...}, longsortcon) =
      let
	val (strid_list, sortcon) = TyCon.explode_LongTyCon longsortcon
      in
	case (Traverse E strid_list) of
	  NONE => NONE
	| SOME(ENV {RE, ...}) => lookupRE(RE, sortcon)
      end

    fun Lookup_lstrid (CONTEXT {E, ...}, lstrid) =
      let
	val (strid_list, strid) = StrId.explode_longstrid lstrid
      in
	case (Traverse E strid_list) of
	  SOME E1 => lookupE_strid(E1, strid)
	| NONE => NONE
      end

    fun Lookup_tyname(CONTEXT{T, ...}, tyname) = lookupT (T, tyname)

    fun Lookup_sortcon(CONTEXT{T, E=ENV{RE, ...}, ... }, sortcon) = 
      lookupRE (RE, sortcon)

(*    fun Rea_from_ToRea_elt TE (tycon, tyname) =   (* used in lookup_tygoal *)
	  case TE.lookup TE tycon of NONE => raise Match
             | SOME tystr => Realisation.singleton (tyname, TyStr.to_theta tystr)
*)
    fun Lookup_tygoal(CONTEXT{E=ENV{TG, RE, ...}, ...}, id) =
	  case TG.lookup TG id of NONE => NONE
             | SOME (ToRealise, sortsch) => SOME sortsch (* FIX once realisations are done *)
		(* lookup each in ToRealise and subst, raise Match if not found *)
(*                SOME (Realisation.on_TypeScheme
		        (FinMap.fold Realisation.oo Realisation.Id 
		                     (FinMap.ComposeMap (Rea_from_ToRea_elt TE) ToRealise)  )
		        tyscheme  )
*)

    (* convert TyName to SortName *)
    (*[ val TNtoSN : Context -> TyName -> SortName ]*)
    fun TNtoSN C tyname = mlSN_of_TyStr (case lookupT(T_of_C C, tyname)
                                          of SOME tystr => tystr
                                           | NONE => Crash.impossible "rEnv.TNtoSN")

    fun ty_to_srt C ty = RO.MLSortOfTy (TNtoSN C) ty

    (********
    CE_to_VE converts a CE to a VE.  This could trim redundant conjuncts.
    ********)
    fun CE_to_VE(CE: ConEnv): VarEnv =
      let	
	fun f((con, sortscheme), VE) =
	  VE_plus_VE (singleVE(con, LONGCON sortscheme), VE)
      in
	CEFold f emptyVE CE
      end

    (********
    Generate all refinements of a type - this could be optimized.
     - Only uses (Rdom R) and RL from C.
    ********)

    fun allRefinements (C : Context) (ty : Type) : Sort list =
     let
      (* Generate all combinations by selecting one from each list in a list of lists *)
      fun product_lists ([] : 'a list list) (acc : 'a list) = [acc]
	| product_lists (h::t) acc = 
          List.foldr (fn (x, b) => b @ (product_lists t (acc @ [x]))) [] h
      fun product_pair (l1, l2) = 
	List.foldr (fn (x1, b) => b @ (map (fn x2 => (x1, x2)) l2)) [] l1
      fun nnilSubsets [] = []
        | nnilSubsets (h::t) = case (nnilSubsets t) of sst => 
                                 [h] :: ((map (fn s => h::s) sst) @ sst)
      val conjSorts = 
        ListHacks.foldl' (fn (srt, acc) => RO.mkSortConj (conjSortNameT' (T_of_C C)) (srt, acc))
      fun subsetConj srts = map conjSorts (nnilSubsets srts)
      fun recSort_from_list flds =
        RO.mkSortRecSort (List.foldr (fn (a, b) => RO.addField a b) RO.emptyRecSort flds)
     in
        case (SO.Type.un_Arrow ty) 
(*arrow*) of SOME(ty1, ty2) =>   (* We could remove some equivalent sorts here *)
	     subsetConj (map RO.mkSortArrow (product_pair (allRefinements C ty1, 
							   allRefinements C ty2)))
(*cons*)   | NONE => case (SO.Type.to_ConsType ty) of SOME(constype) =>
             (case (SO.Type.un_ConsType constype) 
		of NONE => Crash.impossible "RefinedEnvironments.allRefinements(1)"
                 | SOME(tys, tyname) =>
                   case Lookup_tyname(C, tyname)
                     of NONE => Crash.impossible "RefinedEnvironments.allRefinements(2)"
                      | SOME (TYSTR{R, ...}) =>
                        let
			  val Rd as (hSrt:: _) = Rdom R  (* at least one refinement *)
                          val allArgsRefs = product_lists (map (allRefinements C) tys) []
			in    (* Note: refinements all have same variance *)
                          if (SortName.allVars_Covariant hSrt) then
			    map RO.mkSortCons (product_pair(allArgsRefs, Rd))
                          else (* conjoin subsets of arguments *)
                            ListHacks.flatten
                             (map (fn args_lst =>
                                     map (fn sn => 
					   conjSorts (map (fn args => RO.mkSortCons(args, sn)) 
					                  args_lst))
					 Rd)
				   (nnilSubsets allArgsRefs))
			end)
(*tyvar*)  | NONE => case (SO.Type.to_TyVar ty) of (SOME tv) => 
             [RO.mkSortSortVar (RO.SVofTV tv)]
(*record*) | NONE => case (SO.Type.to_RecType ty) of (SOME rectype) =>
	     (map recSort_from_list
	          (product_lists (map (fn (lab, ty) => map (fn x => (lab, x))
			 	                           (allRefinements C ty))
	                              (SO.Type.RecType.to_list rectype))
		                 []))
	   | _ => Crash.impossible "RefinedEnvironment.allRefinements(3)"
    end
	  
    (********
    Lattice Creation
    ********)

     (* memo table for subSortName below *)
     fun eqsns (sns1, sns2) = 
         (List.length sns1 = List.length sns2) andalso
         List.all SortName.eq (ListPair.zip (sns1, sns2))

     fun eqsnsl (snsl1, snsl2) = 
         (List.length snsl1 = List.length snsl2) andalso
         List.all eqsns (ListPair.zip (snsl1, snsl2))

     fun eqSSN ((sns11, snsl21), (sns12, snsl22)) = 
         eqsns(sns11, sns12) andalso eqsnsl(snsl21, snsl22)
         
     fun hashWords (x, y) = (x mod 0w777337) + ((y mod 0w444337) * 0w1111) + 0w4443337
     fun hashsns sns = List.foldl hashWords 0w77777 (map (Word.fromInt o SortName.idnum) sns)
     fun hashSSN (sns1, sns_list2) = 
       hashWords (hashsns sns1, List.foldl hashWords 0w11111 (map hashsns sns_list2))

     exception memoHashExn;

     val memoSSN : (SortName list * SortName list list, bool) HashTable.hash_table
       = HashTable.mkTable (hashSSN, eqSSN) (65533, memoHashExn)

     (* LHS is a conjunction, RHS is a disjunction of conjunctions. *)
     fun subSortName0 _ ([], _) = Crash.impossible "RefinedEnvironments.subSortName0(1)"
       | subSortName0 (T, memo_pos:bool,
		       assumptions : (SortName list * SortName list list) list)
                     (sortnames1 as (sn1::_), sortnames_list2) =
       (out_debug(fn ()=> "subSortName0: " ^ (pr_SortNames sortnames1) ^ " < " ^
		  (ListHacks.stringSep "" "" " | " pr_SortNames sortnames_list2));
        add_indent 4;
       case sortnames_list2 of [sortnames2 as (hsortname21::hsortname22::tsortnames2)] => 
              List.all (fn sn2=> subSortName0 (T, memo_pos, assumptions) (sortnames1, [[sn2]]))
                       sortnames2
       | _ =>
       case lookupT (T, SortName.tyname sn1)
	 of NONE => Crash.impossible "RefinedEnvironments.subSortName0(2)"
          | SOME (TYSTR {CE, R, RL, covariant, ...}) =>
       if isEmptyCE CE then    (* Not datatype *)
	  let val conj1 = conjoinListRL RL sortnames1 in
            add_indent ~4; out_debug (fn ()=>"NOT DATATYPE");
	    List.exists (fn x => subSortNameRL RL (conj1, x))
	                (map (conjoinListRL RL) sortnames_list2)
          end
       else
       let
         (* Possible optimizations:
             - attempt to lookup in lattice, particularly if only one disjunct on RHS.
             - return extra subSorting information (dis)proved, with assumptions 
               used for each?
         *)
         val expandSNs =   (* Expand sortnames to their conjuncts *)
	   List.foldl (fn (x, r) => (SortName.sortNameConjuncts x) @ r) []
         fun leq_snlist [] _ = true
           | leq_snlist _ [] = false
           | leq_snlist (h1::t1) (h2::t2) = 
             SortName.leq(h1, h2) andalso (not (SortName.eq (h1, h2)) orelse leq_snlist t1 t2)
         fun uniqSorted eq [] = []
           | uniqSorted eq [h] = [h]
           | uniqSorted eq (h1::(t as h2::_)) = if eq(h1, h2) then (uniqSorted eq t)
						else h1::(uniqSorted eq t)
         fun leq a b = SortName.leq (a, b)
         fun uniqAndSort sortnames = 
           uniqSorted SortName.eq (ListSort.sort leq (expandSNs sortnames))

	 val sortnames1' = uniqAndSort sortnames1
         val sortnames_list2' =
	   uniqSorted eqsns (ListSort.sort leq_snlist (map uniqAndSort sortnames_list2))
         val sns1_snsl2' = (sortnames1', sortnames_list2')
       in
	 case HashTable.find memoSSN sns1_snsl2'
	   of SOME b => (add_indent ~4; out_debug (fn ()=>"MEMO"); b)(* return the memoized value *)
            | NONE =>
       let  
         (*  Lookup in assumptions - could make use of sortedness here.  *)
         fun subset ([], _) = true
           | subset (_, []) = false
           | subset (l1 as h1::t1, h2::t2) = 
             SortName.leq (h2, h1) andalso   (* FIXED: was in wrong order!  9aug02 *)
             (if SortName.eq (h1, h2) then subset(t1, t2) else subset(l1, t2))
         fun matches (sortnames, sortnames_list) =
	     subset (sortnames, sortnames1') andalso 
	     List.all (fn x => List.exists (fn y => subset (y, x)) sortnames_list2')
                         sortnames_list
         val conjSN = conjSortNameT' T
         (* Helper function with accumulator for expandConj *)
         fun expC (con, [], sortfcn_opt) = [sortfcn_opt]
	   | expC (con, sortname::t, sortfcn_opt) =
	     case lookupR (R, sortname)
	       of NONE => Crash.impossible "RefinedEnvironments.expC(1)"
                | SOME RC => 
		  (case (lookupRC (RC, con), sortfcn_opt)
		     of (NONE, _) => []
		      | (SOME NONE, NONE) => expC (con, t, NONE)
                      | (SOME (SOME sortfcn_list), SOME sf) =>
 	                List.foldl 
			  (fn (sf', r) => 
			     (expC (con, t, SOME (RO.conjSortFcn conjSN (sf, sf'))))
                             @ r)
			  []
			  sortfcn_list
		      | _ => Crash.impossible "RefinedEnvironment.expC(2)"
		  )
         (* Expand conjunction by looking up con, multiplying out alternatives *)
         (* Try to simplify this code!  *)
	 fun expandConj (_, []) = Crash.impossible "RefinedEnvironment.expandConj(1)"
	   | expandConj (con, sortname::sortnames) = 
	   case lookupR (R, sortname)
	     of NONE => Crash.impossible "RefinedEnvironments.expandConj(2)"
              | SOME RC => 
  	        (case lookupRC (RC, con)
		   of NONE => []
	            | SOME NONE => expC (con, sortnames, NONE)
                    | SOME (SOME sortfcn_list) =>
 	              List.foldl (fn (sf', r) => (expC (con, sortnames, SOME sf')) @ r)
			         []
			         sortfcn_list
                )
         fun subSortopt (NONE, []) = false
           | subSortopt (NONE, _) = true
	   | subSortopt (SOME sfcn, sfcn_opts) =
	     case (RO.applySortFcnVars sfcn) of (svs, sort) => 
	     case (map RO.mkSortSortVar svs) of sortsvs =>
	     RO.subSortList
	       (subSortName0(T, memo_pos, (sortnames1', sortnames_list2')::assumptions), conjSN)
	       (sort, (map (fn NONE => Crash.impossible "RefinedEnvironments.subSortopt"
			     | SOME sfcn' => RO.applySortFcn (sfcn', sortsvs))
		           sfcn_opts))
<<<<<<< RefinedEnvironments.sml
	 val _ = out_debug(fn ()=>"sorted sns: " ^ (pr_SortNames sortnames1') ^ " < " ^
                                      (ListHacks.stringSep "" "" " | " pr_SortNames sortnames_list2'))
         val res =   (* First case is an optimization *)
=======
	 val _ = out_debug(fn ()=>"sorted sns: " ^ (pr_SortNames sortnames1') ^ " < " ^
			   (ListHacks.stringSep "" "" " | " pr_SortNames sortnames_list2'))
	 val res =   (* First case is an optimization *)
>>>>>>> 1.41
           List.exists (fn sns => subset (sns, sortnames1')) sortnames_list2' orelse
	   List.exists matches assumptions orelse
	   List.all    (* Expand for each constructor to get sub-goals.  *)
	     (fn con =>
	       let val sortoptRHS =
	         List.foldl (fn (x, r) => (expandConj (con, x)) @ r) [] 
		            sortnames_list2' 
	       in
		 List.all (fn sortoptLHS => subSortopt (sortoptLHS, sortoptRHS))
		             (expandConj (con, sortnames1'))
	       end)
	     (domCE CE)
         val _ = add_indent ~4
	 val _ = out_debug(fn ()=>"subSortName0: " ^ (pr_SortNames sortnames1) ^ " < " ^
			   (ListHacks.stringSep "" "" " | " pr_SortNames sortnames_list2)^ 
			   " RESULT: " ^ (Bool.toString res) ^ "\n")
         val _ = if (memo_pos orelse (not res)) then
	           HashTable.insert memoSSN (sns1_snsl2', res)
		 else ()  (* Don't memoize true unless memo_pos is set *)
       in
	 res
       end
       end)  (* of subSortName0 *)

    fun subSortName T sns12 =
      if subSortName0 (T, false, []) sns12 then
	subSortName0 (T, true, []) sns12 (* repeat to force memoization of subgoals *)
      else
        false    (* false sub-goals are always memoized *)

    (* MAIN LATTICE CREATION CODE *)
    (* Create a modified T for TRs, creating conjunctions to complete the lattice.  *)
    (* TRs contains the raw RCs directly from a datasort declaration. *)
    (* TRs may contain "syntactic conjunctions" which will be simplified in the result. *)
    (* The SortNames in TRs will be renamed when they are equivalent to previous SortNames.  *)
    (* The same renaming and simplification is applied to RE, and the result returned. *)
    (* Also returns an enriched VE. *)
    (*    [Perhaps this function does too much, and it could be broken up?]  *)

    val DEBUG_completeT = ref true
    val _ = Flags.add_flag ("DEBUG_completeT",
                             "Debug completeT?", DEBUG_completeT)
    

    fun completeT (C : Context, TRs : (TyName * SortNameEnv) list, RE : SortEnv) 
        : TyNameEnv * SortEnv * VarEnv =
      let
        val out_debug = out_debug_maybe DEBUG_completeT
        val _ =  out_debug (fn () => "completeT: START")
        val TsubSortName = (* extend T from C by adding R's from TRs *)
	  List.foldl
	    (fn ((tyname, R'), T) => 
		(case lookupT (T, tyname)
		   of NONE => Crash.impossible "RefinedEnvironments.completeT(1)"
		    | SOME (TYSTR {CE, R, mlSN, covariant, RL}) => 
		      T_plus_T (T, singleT(tyname, TYSTR {CE=CE, R=R_plus_R (R, R'), mlSN=mlSN,
					                  covariant=covariant, RL=RL}))))
	    (T_of_C C)
            TRs
        val subSortName' = subSortName TsubSortName        (* Compares RC's when necessary. *)
	fun find_eq_sortname (conjuncts, sns) =  (* Scan sns for an equiv. to a conjunction *) 
          List.find (fn sn => (out_debug(fn ()=>"find_eq_sortname: -------------------------");
                               out_debug(fn ()=>" SortName: " ^ (SortName.pr_SortName ("", sn)) );
                               subSortName' (conjuncts, [[sn]]) andalso
			       subSortName' ([sn], [conjuncts]))
                               before (out_debug(fn ()=>"find_eq_sortname: END ----")))
		 	       
		    sns

        (* Add newSN = sortcons to tystr, including conjunctions, but with no CE (yet).
           If newSN has an equivalent in tystr, then instead add it to a "renaming" list.
           Returns the new tystr, the renaming list, and the sortnames actually added. *)
        fun addSortname (tyname, tystr as (TYSTR {CE, R, mlSN, covariant, RL}), 
			 newSN, sortcons) 
	              : TyStr * (SortName * SortName) list * SortName list = 
          case find_eq_sortname ([newSN], Rdom R)
	    of SOME (eq_sortname) =>
                   (tystr, [(newSN, eq_sortname)], [])
             | NONE =>       (* Need to add newSN to tystr *)
            let
              val sortnames = Rdom R  
	      val (newR, newRL, _, newConjs) =  (* Fold over R to fill in the conj's for newSN *)
		RFold                                      (* cnjs = info for the created sn's *)
		  (fn ((sn, sc), (R', RL', sns, cnjs)) =>        (* sns = all sns in R' so far *)
	            case find_eq_sortname ([newSN, sn], sns) (* inv. princ. strengthened later *)
		      of SOME eq_sn => (R', addRL (RL', (newSN, sn), eq_sn), sns, cnjs) 
                       | NONE =>   (* make sortname for the conjunction "newSN & sn" *)
                         let val empty = subSortName' ([newSN, sn], [])      (* check emptiness *)
                             val variance = SortName.variance sn          (* actually not used? *)
                             val newcnjSN =               (* create a SortName for "newSN & sn" *) 
                                 SortName.newConjSortName (newSN, sn, empty, variance, tyname)
                             val newRC = intersectRC (conjoinRL' RL) (sortcons, sc)
                             val newR = R_plus_R (R', singleR (newcnjSN, newRC))
			     val newRL = (* newSN & sn = newcnjSN ;  newSN & newcnjSN = newcnjSN *)
                              addRL (addRL (RL', (newSN,sn), newcnjSN), (newSN,newcnjSN), newcnjSN)
                         in 
			     (newR, newRL, newcnjSN::sns, (sn, newcnjSN)::cnjs)
                         end
	          )  (* end of fn for RFold *)
	          (R_plus_R (R, singleR (newSN, sortcons)), 
                   RL, newSN::sortnames, [])  (* start RFold with sortname added to R *)
	          R   (* Fold over R *)

	      (* Use associativity to fill in RL with the intersections of conj = (newSN & sn)
                 with each sortname in snsRL.  RL must have all intersections filled in for 
                 "newSN", and all intersections filled in for "sn" with each sortname in snsRL.  *)
	      fun addConjs (sn, conj, RL, snsRL) =
		List.foldl (fn (old_sn, RL') =>
			     addRL (RL', (conj, old_sn),
				    conjoinRL RL (conjoinRL RL (sn, old_sn), newSN)))
		           RL
			   snsRL
	      val (completeRL, _) =   (* Fill in lattice for conjunctions of newSN. *)
  	        List.foldl (fn ((sn, conj), (RL, snsRL)) =>
			      (addConjs (sn, conj, RL, snsRL), conj::snsRL))
		           (newRL, sortnames)
			   newConjs        (* list of (sn, conj), conj = sn & sortname *)
	    in
	      (TYSTR {CE=CE, R=newR, mlSN=mlSN, covariant=covariant, RL=completeRL}, [], 
	       newSN::(map #2 newConjs))
	    end   (* of addSortName  ("end" matches the "let" in the NONE case) *)	  

        (* Add each of the new definitions in R to oldTystr.  Uses addSortname (above). *)
        fun addTyStr (tyname, oldTystr, R) =
          RFold (fn ((sn, sortcons), (tystr, renaming, newSns)) =>
                    let val (tystr', renaming', newSns') = 
                            addSortname (tyname, tystr, sn, sortcons) 
		    in  (tystr', renaming' @ renaming, newSns @ newSns')  end
                )
                (oldTystr, [], [])
                R

        (* Now we apply addTyStr to each tystr in TRs, accumulating the additions and a renaming.*)
	(* changedT = changed part of T with TRs, except for CE's and renaming of RC's *)
        (* newT = whole of T + changedT *)
        val (newT, changedT, renaming, tyname_tystr_newSns_list) = 
          List.foldl     (* fold over TRs, add each R to the corresponding TyStr in T of C *)
            (fn ((tyname, R), (T1, T2, renaming, tnTsSn_s)) =>
	       (case lookupT (T1, tyname)
                  of NONE => Crash.impossible "RefinedEnvironments.completeT(2)"
                   | SOME tystr =>
                     let val (newTystr, renaming', newSns) =  addTyStr (tyname, tystr, R)
                         val thisT =  singleT (tyname, newTystr)
                     in   (T_plus_T (T1, thisT), 
                           T_plus_T (T2, thisT), 
                           renaming' @ renaming,
  			   (tyname, newTystr, newSns)::tnTsSn_s  )
                     end
	       )
            )
            (T_of_C C, emptyT, [], [])
            TRs

        val allNewSns = ListHacks.flatten (map #3 tyname_tystr_newSns_list)
        val newC = C_plus_T(C, changedT)  (* WAS: contains enough for allRefinements used below *)
        (* fullT was used by unionSF.  It might be needed for intersectRCmax? *)
        val fullT = T_plus_T(TsubSortName, changedT)  (* include both old and new SNs *)
        val conjSN = conjSortNameT newT                            
        (* Find the maximal sortfcns contained in a union.  We need to apply this to the "of"   *)
        fun unionSF [] = []  (*  sorts if we want completeness with respect to regular-tree     *)
          | unionSF [sfcn] = [sfcn]  (* grammars.  But, if the programmer always chooses maximal*)
          | unionSF (hsfcn::tsfcns) =  (* elements in datasort declarations, this isn't needed. *)
          if not (!add_unions_to_constructor_sorts) then (hsfcn::tsfcns) else
	  let
	    val (svs, hsrt) = RO.applySortFcnVars hsfcn
	    val svsorts = map RO.mkSortSortVar svs
            val tsrts = map (fn sf => RO.applySortFcn (sf, svsorts)) tsfcns
            val srts = hsrt::tsrts
            val out_srts =      (* This may be wildly expensive, and could be optimized. *)
	        List.foldr 
	           (fn (srt, acc) => 
	              if (RO.subSortList (subSortName fullT, conjSortNameT' newT)
			                 (srt, srts)) then
                        (out_debug(fn ()=>"ADDING TO CONSTRUCTOR SORT\n");
		         RO.addSortToMaximals (conjSortNameT newT) (srt, acc))
		      else
		        acc)
                   []
	           (allRefinements newC (RO.tyOfSort hsrt))
          in
            map (fn srt => RO.mkSortFcn(svs, srt)) out_srts
	  end
        
        fun unionSFopt NONE = NONE (* Apply unionSFopt to (sortfunction list) option (as in RC) *)
          | unionSFopt (SOME sfcns) = SOME (unionSF sfcns)

	fun renameSN [] sortname = sortname
          | renameSN ((sn1, sn2)::t) sortname = 
	    if SortName.eq (sortname,sn1) then sn2 else (renameSN t sortname)
        val renSimp = RO.renameSimpl_SFcn   (* renSimp does renaming and simplification *)
	                (renameSN renaming, conjSN)

	fun renSimpR (R, sns) : SortNameEnv =   (* rename and simplify R at sns *)
	  List.foldl      	             (* previously made RC and CE principal via unions *)
	    (fn (sn, accR) =>   (* fold over sns, modifying R *)
	       case lookupR (R, sn) 
		 of NONE => Crash.impossible "RefinedEnvironments.completeT(3)"
		  | SOME RC => 
		    let val newRC = RCmap (fn NONE => NONE
		                            | SOME sfcns => SOME (map renSimp sfcns))
		                          RC
		    in
		      R_plus_R (accR, singleR (sn, newRC))
		    end
	    )
	    R
	    sns

        fun renameRdom R = RFold (fn ((sn, sortcons), accR) =>
                                     R_plus_R (accR, singleR(renameSN renaming sn, sortcons)))
                                 emptyR
                                 R

	val renamedT =
	  List.foldl  (* fold over tyname_tystr_newSns_list, renaming changedT *)
	    (fn ((tyname, TYSTR {CE, R, mlSN, covariant, RL}, sns), T) =>
	       let val renR =  renSimpR (R, sns)
               in     (T_plus_T (T, singleT(tyname, TYSTR {CE=CE, R=renR, mlSN=mlSN,
			  			           covariant=covariant, RL=RL})))
               end)   (* end of fn *)
	    changedT
	    tyname_tystr_newSns_list  (* Contains all new sortnames, incl. conjunctions. *)

        fun RCimprove (withR, sn, RC) =    (* improve RC by intersecting with each RC in withR *)
          (out_debug (fn ()=>"\nRCimprove");
           if not (!improve_inversion_principles) then RC else
           RFold (fn ((with_sn, withRC), accRC) => if subSortNameT renamedT (sn, with_sn)
                                                 then intersectRCmax conjSN (accRC, withRC)
                                                 else accRC)
          
                 RC
                 withR  (* Fold over withR, improving RC *)
           )

(*      fun Rimprove (R, []) = R  (* tail recursive *)
          | Rimprove (R, head_sn::tail_sns) = 
              Rimprove(R_plus_R(R, singleR(head_sn, RCimprove(R, head_sn, lookupR(R, head_sn)))),
                       tail_sns)

        (* put this into the T_withCE fold below? *)
        (* Improve the RCs for all new sortnames. *)
	val renamedT =
	  List.foldl  (* fold over tyname_tystr_newSns_list, improving renamedT1 at new sns *)
	    (fn ((tyname, TYSTR {CE, R, mlSN, covariant, RL}, sns), T) =>
	       let val improvedR =  Rimprove (R, sns)
               in     (T_plus_T (T, singleT(tyname, TYSTR {CE=CE, R=improvedR, mlSN=mlSN,
			  			           covariant=covariant, RL=RL})))
               end)   (* end of fn *)
	    renamedT1
	    tyname_tystr_newSns_list  (* Contains all new sortnames, incl. conjunctions. *)
*)
        val renamedRE =   (* Apply the renaming also to the RE (input to completeT) *)
	  REmap (fn sortfcn => 
		   case RO.SortFcn_to_SortName sortfcn
		     of NONE => sortfcn
		      | SOME sn => RO.SortName_in_SortFcn (renameSN renaming sn))
                RE

        val (T_withCE, changedVE) =
          List.foldl     (* Fold over TRs, adding to CEs in renamedT and building VE from CEs *)
            (fn ((tyname, inR), (T, VE)) =>  (* fold over TRs *)
	       (case lookupT (T, tyname)
                  of NONE => Crash.impossible "RefinedEnvironments.completeT(4)"
                   | SOME (TYSTR {CE, R, mlSN, covariant, RL}) => 
                     let val (outCE, outR) =   
                       RFold (fn ((sn, RC), (accCE, accR)) =>  (* fold over R *)
                                let 
(* Move out of the inner fold? *)   val renamed_inR = renameRdom(renSimpR (inR, Rdom inR))
                                    val improvedRC1 = RCimprove (renamed_inR, sn, RC)
                                    val improvedRC =   (* improve with all of R if sn is new *)
                                        if List.exists (fn new_sn => SortName.eq(sn, new_sn))
                                                       allNewSns
                                          then RCimprove(R, sn, improvedRC1)
                                          else improvedRC1
                                    val unioned_RC = RCmap unionSFopt RC
                                    val thisCE = RC_to_CE newT (sn, unioned_RC)
                                in  (CE_plus_CE_conjCons newT (accCE, thisCE),
                                     R_plus_R (accR, singleR(sn, improvedRC)))
                                end)
                           (emptyCE, emptyR)
                           R    (* Includes conjunctions and old SNs.  (Use inR to exclude) *)
                         val outVE = VE_plus_VE (VE, CE_to_VE outCE)
                         val tystr = TYSTR{CE=outCE, R=outR, mlSN=mlSN, covariant=covariant, RL=RL}
                         val outT = T_plus_T (T, singleT(tyname, tystr))
                     in (outT, outVE)
                     end)
            )
            (renamedT, emptyVE)
            TRs


    (*   Need to modify the inversion principle (RC) for a datasort to include
         the inversion principles for supersorts.  (via intersectRCmax) 

         We can do this by 
    *)



        val _ = out_debug (fn ()=> "completeT: END")
(*                         PP.layout_pair layoutT layoutVE (T_withCE, changedVE)) *)

      in
	(T_withCE, renamedRE, changedVE)
      end  (* of completeT *)


    structure Rea = struct  (* See Environments.Realisation *)
      open RO.Realisation
      type trealisation = RO.trealisation

      type conjSN = SortName * SortName -> SortName

      (* This probably should be in RefObject.sml *)
      fun on_SortName' phi sn = 
          case RO.SortFcn_to_SortName (on_SortName phi sn) of SOME sn2 => sn2

      (* 10jan03 NOT NEEDED, and we'd need a context here for TNtoSN to work.  
      fun on_TyName phi tn = RO.tyOfSort (on_SortName' phi (TNtoSN tn))   *)

      fun on_VarEnv conjSN (phi : realisation) (VE : VarEnv) =
	    VEmap  (fn LONGVAR sigma =>
		         LONGVAR (on_SortScheme conjSN phi sigma)
	             | LONGCON sigma =>
			 LONGCON (on_SortScheme conjSN phi sigma)
		     | LONGEXCON tau =>







			 LONGEXCON (on_Sort conjSN phi tau))  VE

      fun on_TyEnv conjSN (phi : realisation) (TE : TyEnv) =
	    TEmap (on_SortFcn conjSN phi) TE
      fun on_SortEnv conjSN (phi : realisation) (RE : SortEnv) =
	    REmap (on_SortFcn conjSN phi) RE
      (* FIX: on_ToRea should adjust sortnames if realiser is not a sortname *)
      fun on_ToRea phi torea = 
          ToRea.map (fn sn => case RO.SortFcn_to_SortName (on_SortName phi sn) 
                                of SOME sn2 => sn2 ) torea
      fun on_TyGoals conjSN phi TG = 
          TG.map (fn (torea, sortscheme) => 
                     (on_ToRea phi torea, on_SortScheme conjSN phi sortscheme) )  TG

      fun on_Env conjSN phi (E as ENV{SE, TE, RE, VE, EE, TG}) =
	    if is_Id phi then E else
  	      ENV{SE=on_StrEnv conjSN phi SE, TE=on_TyEnv conjSN phi TE, 
                  RE=on_SortEnv conjSN phi RE,
                  VE=on_VarEnv conjSN phi VE, EE=EE (* unused! *), TG=on_TyGoals conjSN phi TG}
      and on_StrEnv conjSN phi (SE : StrEnv) =
	    if is_Id phi then SE else SEmap (on_Env conjSN phi) SE

      fun on_RC conjSN phi RC = RCmap (Option.map (List.map (on_SortFcn conjSN phi))) RC

   (* 14jan02 Previously the following incorrectly assumed that dom R and dom phi were disjoint. *)
      fun on_SortNameEnv conjSN phi (R as SORTNAMEENV m) = (*WAS: Rmap (on_RC conjSN phi) R  *)
            FinMapEq.Fold (fn ((sn, RC), accR) => 
                              addR (accR, (on_SortName' phi sn), on_RC conjSN phi RC)  )
              emptyR
              m

      fun on_RL phi (REFLATTICE m) =
          let  val onSN = on_SortName' phi  in
            FinMapEq.Fold (fn (((sn1, sn2), sn3), accRL) => 
                              addRL (accRL, (onSN sn1, onSN sn2), onSN sn3)  )
              emptyRL
              m
          end

      fun on_TyStr conjSN (phi : realisation) (TYSTR {CE, R, mlSN, covariant, RL}) =
	    TYSTR {CE = CEmap (on_SortScheme conjSN phi) CE, 
                   R = on_SortNameEnv conjSN phi R, 
                   mlSN = (case RO.SortFcn_to_SortName (on_SortName phi mlSN) of SOME sn => sn), 
                   covariant=covariant, RL= on_RL phi RL} (* 13jan03 Added on_RL (again).  *)

          
      fun on_TyNameEnv conjSN phi T = if is_Id phi then T else  (* Don't rename dom(T)! *)
                                        Tmap1 (on_TyStr conjSN phi) T

      (*                                Tmap (fn (tn, tystr) => 
                                                 (on_TyName phi tn, on_TyStr conjSN phi tystr)  ) T
                 13jan03  THIS WAS A MISTAKEN ATTEMPT TO RENAME THE DOMAIN OF T ALSO.  *)

    end (*Rea*)

    (* Merge tystrs for ts into the tystr for t1 in T and modify E accordingly. *)
    (* Currently this raises "No_match" if the tystrs are different. *)
    fun mergeT (T : TyNameEnv, E : Env, ts : TyName list, t1 : TyName)
             : TyNameEnv * Env =
        let
          val tystr1 = case lookupT (T, t1) of SOME tystr => tystr
                                        (*   | NONE => raise No_match     (* Shouldn't occur.*) *)
	  val (CE1, R1, mlSN1, covariant1, RL1) = unTyStr tystr1
          val ENV {SE, TE, RE as (SORTENV mRE), VE, EE, TG} = E
          (* FIX: add check for datatype *)
          val Tmerge = map (fn t2 => case lookupT (T, t2) of SOME tystr => tystr) ts
          val mTrest = FinMapEq.filter (fn (t3, _) => 
                                           not (List.exists (fn t4 => TyName.eq (t3, t4)) ts) )
                                       (case T of TYNAMEENV m => m)
          val phi = foldl (fn (tystr2, phi2) => Rea.oo (conjSortNameT T) 
                                                       phi2 (matchTyStr (tystr2, tystr1)))
	                  Rea.Id Tmerge         (* Might raise No_match here *) 
          val tystr_new = tystr1  (* FIX: allow different tystrs to be merged *)
          val Tout = T_plus_T (TYNAMEENV mTrest, singleT (t1, tystr_new))
          val conjSN = conjSortNameT T
        in
          (Rea.on_TyNameEnv conjSN phi Tout, Rea.on_Env conjSN phi E)
        end
                                      

    (*******
     Initial environment: built-in sorts (`->', `int', `real', `string',
			    `exn', `ref'), the constructor `ref', and the
			    value `prim'. `prim' has type 'a -> 'b.
                            Moreover the overloaded values, and therefore
			    the type `bool'.
     *******)

    local
      fun ctx' (tycon, sortname, CE, RC, cov) =
        let
          val _ = if cov then SortName.set_Covariance sortname else ()
          val sortfcn = RO.SortName_in_SortFcn sortname
          val TE = singleTE(tycon, sortfcn)	  
          val T = singleT(SortName.tyname sortname,
			  mkTyStr(CE, singleR(sortname, RC), sortname, cov, singleRL sortname))
          val RE = singleRE(tycon, sortfcn)
        in	  
	  mkC(T, E_plus_E(TE_in_E TE, RE_in_E RE))
        end

      fun ctx(tycon, sortname) = 
	ctx'(tycon, sortname, emptyCE, emptyRC, true)

      fun joinC [] = emptyC
	| joinC (C :: rest) = C_plus_C(C, joinC rest)

      val intC = ctx(TyCon.tycon_INT, SortName.sortName_INT)
      val realC = ctx(TyCon.tycon_REAL, SortName.sortName_REAL)
      val stringC = ctx(TyCon.tycon_STRING, SortName.sortName_STRING)
      val charC = ctx(TyCon.tycon_CHAR, SortName.sortName_CHAR)
      val exnC = ctx(TyCon.tycon_EXN, SortName.sortName_EXN)

      val wordC = ctx (TyCon.tycon_WORD, SortName.sortName_WORD)
      val word8C = ctx (TyCon.tycon_WORD8, SortName.sortName_WORD8)
      val word_tableC = ctx (TyCon.tycon_WORD_TABLE, SortName.sortName_WORD_TABLE)


      val boolRC = RC_plus_RC(singleRC(Ident.id_TRUE, NONE), singleRC(Ident.id_FALSE, NONE))
      val boolCE = RC_to_CE emptyT (SortName.sortName_BOOL, boolRC)
      val boolC = ctx'(TyCon.tycon_BOOL, SortName.sortName_BOOL, boolCE, boolRC, true)

      local
        val sv = RO.fresh_sv()
        val refRC = singleRC(Ident.id_REF, SOME (RO.mkSortFcn([sv], RO.mkSortSortVar sv)))
      in
        val refCE = RC_to_CE emptyT (SortName.sortName_REF, refRC)
        val refC = ctx'(TyCon.tycon_REF, SortName.sortName_REF, refCE, refRC, false)
      end

      local
        val sv = RO.fresh_sv()
	val alphaSort = RO.mkSortSortVar sv
	val listSort = RO.mkSortConsSort(RO.mkConsSort([alphaSort], SortName.sortName_LIST))
        val consArg = RO.mkSortPair(alphaSort, listSort)
        val listRC = RC_plus_RC(singleRC(Ident.id_NIL, NONE), 
				singleRC(Ident.id_CONS, SOME (RO.mkSortFcn([sv], consArg))))
      in
	val listCE = RC_to_CE emptyT (SortName.sortName_LIST, listRC)
	val listC = ctx'(TyCon.tycon_LIST, SortName.sortName_LIST, listCE, listRC, true)
      end

             (* overloaded functions *)
      local
	val alphaSort = RO.mkSortSortVar(RO.fresh_sv())
	val betaSort = RO.mkSortSortVar(RO.fresh_sv())
        val primArg = RO.mkSortTriple(RO.SortString, RO.SortString, alphaSort)
	val primSort = RO.mkSortArrow(primArg, betaSort)
	val primSortScheme = RO.Close_Sort primSort

	val num = RO.fresh_sv()
	val numSort = RO.mkSortSortVar num
	val overloadedOpArityTwoSort = 
	  RO.mkSortArrow(RO.mkSortPair(numSort, numSort), numSort)
	val overloadedOpArityTwoSortScheme = 
	  RO.mkSortScheme([num], overloadedOpArityTwoSort)
	val overloadedOpArityOneSort = RO.mkSortArrow(numSort, numSort)
	val overloadedOpArityOneSortScheme = 
	  RO.mkSortScheme([num], overloadedOpArityOneSort)
        val overloadedRelOpSort = 
	  RO.mkSortArrow(RO.mkSortPair(numSort, numSort), RO.SortBool)
        val overloadedRelOpSortScheme =
	 RO.mkSortScheme([num], overloadedRelOpSort)
      in
	val primVE      = singleVE(Ident.id_PRIM, LONGVAR primSortScheme)
	val absVE       = singleVE(Ident.id_ABS, LONGVAR overloadedOpArityOneSortScheme)
	val negVE       = singleVE(Ident.id_NEG, LONGVAR overloadedOpArityOneSortScheme)
	val divVE       = singleVE (Ident.id_DIV, LONGVAR overloadedOpArityTwoSortScheme)
	val modVE       = singleVE (Ident.id_MOD, LONGVAR overloadedOpArityTwoSortScheme)
	val plusVE      = singleVE(Ident.id_PLUS, LONGVAR overloadedOpArityTwoSortScheme)
	val minusVE     = singleVE(Ident.id_MINUS, LONGVAR overloadedOpArityTwoSortScheme)
	val mulVE       = singleVE(Ident.id_MUL, LONGVAR overloadedOpArityTwoSortScheme)
	val lessVE      = singleVE(Ident.id_LESS, LONGVAR overloadedRelOpSortScheme)
	val greaterVE   = singleVE(Ident.id_GREATER, LONGVAR overloadedRelOpSortScheme)
	val lesseqVE    = singleVE(Ident.id_LESSEQ, LONGVAR overloadedRelOpSortScheme)
        val greatereqVE = singleVE(Ident.id_GREATEREQ, LONGVAR overloadedRelOpSortScheme)

	val DivVE       = singleVE (Ident.id_Div, LONGEXCON RO.SortExn)
	val BindVE      = singleVE (Ident.id_Bind, LONGEXCON RO.SortExn)
	val MatchVE     = singleVE (Ident.id_Match, LONGEXCON RO.SortExn)
	val OverflowVE  = singleVE (Ident.id_Overflow, LONGEXCON RO.SortExn)

        fun joinVE [] = emptyVE
          | joinVE (VE :: rest) = VE_plus_VE(VE, joinVE rest)
      end

      local
        val unitSortfcn = RO.mkSortFcn([], RO.SortUnit)
        val unitTE = singleTE(TyCon.tycon_UNIT, unitSortfcn)
        val unitRE = singleRE(TyCon.tycon_UNIT, unitSortfcn)
      in
        val unitC = E_in_C(E_plus_E(TE_in_E unitTE, RE_in_E unitRE))
      end

      val initialC1 = joinC [unitC, intC, realC, stringC, charC, exnC, refC, boolC, listC,
                             wordC, word8C, word_tableC]

      val initialVE = joinVE [CE_to_VE refCE, CE_to_VE boolCE, CE_to_VE listCE, 
			      primVE, absVE, negVE, divVE, modVE, plusVE, minusVE,
			      mulVE, lessVE, greaterVE, lesseqVE, greatereqVE,
			      DivVE, BindVE, MatchVE, OverflowVE]
    in
      val initialC = C_plus_C(initialC1, E_in_C(VE_in_E initialVE))
    end

    val emptyB = emptyC
    and initialB = initialC
    and B_plus_B = C_plus_C

    fun C_of_B C = C
    val B_plus_C = C_plus_C
    val T_of_B = T_of_C
    val E_of_B = E_of_C
    and E_in_B = E_in_C
    and C_in_B = C_of_B


    type Report = Report.Report
    infix //
    val op // = Report.//

    fun reportCE(CONENV map) =
      SortedFinMap.reportMapSORTED
        (fn (con, sortScheme) =>
	   let
	     val names = RO.newSVNames()
	   in
	     Report.line("con " ^ Ident.pr_id con ^ " :> "
			 ^ RO.pr_SortSchemePRETTY names sortScheme
			)
	   end
	) map

    (* Not used anymore *)
    fun reportT (TYNAMEENV map) =
      FinMapEq.reportMapSORTED
        (fn (tyname1, tyname2) => 
	   TyCon.< (TyName.tycon tyname1, TyName.tycon tyname2))
	(fn (tyname, TYSTR {CE,...}) => 
	   Report.indent(2, reportCE CE)
	   // Report.line "")
        map

    fun reportSortDef heading (bindings, sortcon, sortfcn) =
	  let
	    val tyfcn = (RO.TypeFcn_of_SortFcn sortfcn)
	    val {vars, body} =
	      RO.pr_SortFcnPRETTY (RO.newSVNames()) sortfcn
	    val vars' =
	      case vars of "" => "" | _ => vars ^ " "
	    val {body=tyfcnBody, ...} = 
	      StatObject.TypeFcn.pretty_string (StatObject.newTVNames()) tyfcn
	  in
	    Report.line(heading ^ " " ^ vars'
			^ TyCon.pr_TyCon sortcon
			^ (if bindings then " = " ^ body
			   else "")
			^ " <: " ^ tyfcnBody
			)
	  end

    val WIDTH = 100

    fun reportRL (RL, R) =
      let
	fun pr_sn sortname = SortName.pr_SortName ("", sortname)
	val (maxLen, sizeR) =    (* determine the maximum column width and number of sorts *)
	   RFold (fn ((sortname, _), (len, size)) =>
		       (Int.max (len, String.size (SortName.pr_SortName ("", sortname))),
		        size+1))
	         (0, 0) R
        val width = maxLen + 2
        val cols = Int.max ((WIDTH div width) - 1, 1)
        val final_cols = case (sizeR mod cols) of 0 => cols | n => n
        val (_, sn_groups) = RFold (fn ((sortname, _), (n, h::t)) => 
			              (case n of 0 => (cols-1, [sortname]::h::t)
			                       | _ => (n-1, (sortname::h)::t) )
                                     | _ => Crash.impossible "RefinedEnvs.sn_groups")
                                   (final_cols, [[]]) R
        fun pad s = (nspaces (width - (String.size s))) ^ s
        fun pr_line head f sns = 
	  Report.line ((pad head) ^ (List.foldl (fn (sn, r) => r ^ pad (f sn)) "" sns))
        fun pr_group sns = 
          pr_line "" pr_sn sns //
	  RFold (fn ((sn, _), r) => 
		   pr_line (pr_sn sn) (fn sn' => pr_sn (conjoinRL RL (sn, sn'))) sns 
		   // r)
		Report.null R
     in
       Report.line "Intersection Grid:" //
       ListHacks.foldr' (fn (this_rep, acc_rep) => this_rep // Report.line "" // acc_rep)
                        (map pr_group sn_groups)
    (* foldl (fn (sns, r) => r // pr_group sns // Report.line "")
             sn_groups *)
     end        
		    
    fun reportR R = PP.reportStringTree (layoutR R)

    fun reportTyStr tystr = 
      reportR (R_of_TyStr tystr) //   (* The RC's in R have strong inv. princples  *)
      reportCE (CE_of_TyStr tystr) //  (* while CE has max argument sorts *)
      (if (isSingleRL (RL_of_TyStr tystr)) then  Report.null
       else  (* Report.line "" // *)
             reportRL (RL_of_TyStr tystr, R_of_TyStr tystr) )

    fun reportRE{sortEnv=SORTENV m, T, bindings} =
      let
        fun eq_TyName_opt (NONE, NONE) = true
	  | eq_TyName_opt (SOME tn1, SOME tn2) = TyName.eq (tn1, tn2)
          | eq_TyName_opt _ = false
	val RE_by_TyName_map = 
	  FinMap.Fold 
	   (fn ((sortcon, sortfcn), tnm) => 
	      let
		val tynameOpt = case RO.SortFcn_to_SortName sortfcn	      
		                  of NONE => NONE
			           | SOME sortname => SOME (SortName.tyname sortname)
	      in
		FinMapEq.mergeMap eq_TyName_opt (op @) tnm 
				(FinMapEq.singleton (tynameOpt, [(sortcon, sortfcn)]))
	      end)
	   FinMapEq.empty
	   m

	fun report_RE_by_TyName (tyname_opt, sortconSortfcn_list) =
	  let
	    val tystr_opt =
	      case tyname_opt 
		of NONE => NONE
                 | SOME tyname => 
		   case lookupT (T, tyname)
	             of NONE => NONE
		      | SOME tystr => 
		          if isemptyCE (CE_of_TyStr tystr) then  NONE
			  else  SOME tystr

            val heading = case tystr_opt of NONE => "sort" | SOME _ => "datasort"
	  in
	    (List.foldl (fn ((sortcon, sortfcn), r) =>
		           reportSortDef heading (bindings, sortcon, sortfcn) // r)
	                Report.null
		        sortconSortfcn_list)
	     // (case tystr_opt
                   of NONE => Report.null
                    | SOME tystr => Report.indent(2, reportTyStr tystr))
(*             // (Report.line "") *)
	  end
      in
	FinMapEq.reportMap report_RE_by_TyName RE_by_TyName_map
      end


   (* Identifiers bound in the EE have sort exn if non-valued or instances
      of 'a -> exn if they are constructors over 'a. *)

    fun reportEE(EXCONENV map) =
      FinMap.reportMapSORTED
        (Ident.<) (fn (excon, sort) =>
		     Report.line(
		       "exception " ^ Ident.pr_id excon
		       ^ (if RO.isSortExn sort
			  then ""
			  else " of "
			       ^ (case RO.unSortArrow sort
				    of SOME(domSort, _) =>
				         RO.pr_Sort domSort

				     | NONE =>
					 Crash.impossible "reportEE"
				 )
			 )
		     )
		  ) map
  end;


