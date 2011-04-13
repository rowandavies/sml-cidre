(* Environments for refinement phase.
   See Environments, Module_Environments.    - Rowan
*)

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
    type Variance = SortName.Variance

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

    val add_unions_to_constructor_sorts = ref false (* true *)  (** true => lattice enumeration **)
    val _ = Flags.add_flag ("add_unions_to_constructor_sorts",
                             "Add unions to constructor sorts?", add_unions_to_constructor_sorts)
    val improve_inversion_principles = ref true
    val _ = Flags.add_flag ("improve_inversion_principles",
                             "Improve inversion principles?", improve_inversion_principles)

    val XDuce_extra_assumptions = ref true
    val _ = Flags.add_flag ("XDuce_extra_assumptions",
                             "Use XDuce memoization using extra assumptions?", XDuce_extra_assumptions)

    (* Print a conjunction for debugging *)
    fun pr_SortNames sns = 
       ListHacks.stringSep "(" ")" " ^ " (fn sn => SortName.pr_SortName ("", sn)) sns

    (* Printing of debugging messages *)
    val debug_indent = ref 0
    fun nspaces 0 = ""
      | nspaces n = " " ^ (nspaces (n-1))

    fun add_indent n =  (* if !Flags.DEBUG_REFOBJECTS then*) (debug_indent := !debug_indent + n)
                        (*else () *)

    fun out_debug_maybe_indent indent flag str = 
       if !flag 
         then TextIO.output(TextIO.stdErr,(nspaces indent) ^str() ^"\n")
         else ()

    fun out_debug_maybe flag str = 
       out_debug_maybe_indent (!debug_indent) flag str

    fun out_debug str = () (* out_debug_maybe Flags.DEBUG_REFOBJECTS str *)

    val timer = Timer.totalRealTimer ()
    fun time_ms () = Int.fromLarge (Time.toMilliseconds (Timer.checkRealTimer timer))
    val last_report_time = ref (time_ms ())

    val debug_subsortname = ref true (*ref false *)
    val debug_count = ref 0
    fun inc_debug_count () = debug_count := !debug_count + 1
    fun debug_print_is_on () = false (* true *)
	(* !debug_indent < 10 andalso *) (* !debug_subsortname andalso*)
                               (* !debug_count mod 5000 < 2 *)
                               (*let val now = time_ms () in
				    if now - (!last_report_time) > 10000 then
				        (last_report_time := now; 
					 true)
 				   else
	 			       false
			       end*)

    val debug_stack : (int * int * bool * (unit -> string list)) list ref = ref []

    fun pr_now indent str = TextIO.output (TextIO.stdErr,(nspaces indent) ^ str ^"\n")

    fun print_unprinted_debug_stack ((indent, time, false, strs)::stack_tail) = 
          (let val new_tail = print_unprinted_debug_stack stack_tail
	       val _ = map (pr_now indent) (strs())
	   in  (indent, time, true, strs) :: new_tail
	   end)
      | print_unprinted_debug_stack stack = stack (* End of stack, or already printed. *)


    (* When set to true, the optimizer should remove essentially all the cost of the debugging code. *)
    val debug_fns_do_nothing = true (* false *)
    
    fun debug_push_maybe print_now strs = if debug_fns_do_nothing then ()
					  else
	let 
            val indent = !debug_indent
            val _ = debug_stack := (indent, time_ms(), false, strs) :: !debug_stack
            val _ = if print_now then debug_stack := print_unprinted_debug_stack (!debug_stack) 
		    else ()
            val _ = debug_indent := !debug_indent + 4
	in
            ()
	end

    fun debug_push strs = if debug_fns_do_nothing then ()
			  else
	let val print_now1 = debug_print_is_on()
            val _ = debug_count := !debug_count +1
            val indent = !debug_indent
            val print_now = debug_print_is_on()
            val _ = if print_now andalso not print_now1 then 
			pr_now indent "----------------------------------------------------------"
		    else ()
            val _ = debug_push_maybe (print_now orelse print_now1) strs
	in
            ()
	end

    val debug_push_must = debug_push_maybe true
    
    fun debug_pop strs = if debug_fns_do_nothing then ()
			  else 
	let val print_now = debug_print_is_on()
            val (indent, time, push_printed, _)::stack = 
		case !debug_stack of [] => [(0, 0, false, fn () => ["POPPED OFF TOP"])]
				   | curstack => curstack
            val now = time_ms()
	    val _ = if print_now orelse push_printed then 
			(debug_stack := print_unprinted_debug_stack (!debug_stack);
                         map (pr_now indent) (strs()) ;
                         pr_now indent ("[Time = " ^ Int.toString (now - time) ^ "ms  now = "
					^ Int.toString now ^"ms]"); 
			 [] )
		    else []
	    val _ = debug_stack := tl (!debug_stack)
            val _ = debug_indent := indent
	in
	    ()
	end

    fun debug_layout string_tree : string list = 
	PP.lines_minipage (PP.format (120, (string_tree)))

    fun debug_print_must str = if debug_fns_do_nothing then ()
			       else (debug_push_must str; debug_pop (fn () => []))
    fun debug_print str = if debug_fns_do_nothing then ()
			  else if debug_print_is_on() then debug_print_must str
			  else ()

    fun assert f =     (* Simple assertions: SOME means there's a bug *)
	if debug_fns_do_nothing then () else
	case f () of NONE => ()
		   | SOME strs => debug_print_must (fn () => "BUG: ASSERTION FAILED:" :: strs)
				   


    fun no_debug _ = ()
    val _ = RO.debug_push_ref :=  (*no_debug *) debug_push
    val _ = RO.debug_push_must_ref := (*no_debug*) debug_push_must
    val _ = RO.debug_pop_ref := (*no_debug*) debug_pop
    val _ = RO.assert_ref := (*no_debug*) assert


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


(* Maps from sortnames, using a variant of the twelf red-black tree implementation. *)
(*    structure SortNameMap = 
      RedBlackTree(type key'=SortName
                   val compare = SortName.compare) *)

(* Using the following instead is slightly faster, unless we have hundreds of
   refinements (even the tarjan example is faster).  Perhaps a data structure that
   uses a list for small sizes would give the best of both worlds? *) 

structure SortNameMap = 
         ListTable(type key'=SortName
                   val eq = SortName.eq)

(* Hash tables don't work so well.  SortNameMaps are used persistantly.  *)
(*    structure SortNameMap =
       HashTableTwelf(type key'=SortName
                      val hash = SortName.idnum (* ! *)
                      val eq = SortName.eq) *)


    (********
    Refinement Lattices
     represented by an intersection map.
    ********)

    structure RLmap = RedBlackTree(type key'=SortName * SortName
                                   fun compare ((sn11, sn12), (sn21, sn22)) =
				       if SortName.eq (sn11, sn21) then
					   if SortName.eq (sn12, sn22) then
					         EQUAL
					   else if SortName.leq (sn12, sn22) then
					         LESS
					   else  GREATER
				       else if SortName.leq (sn11, sn21) then
					     LESS
				       else  GREATER)

     infix  5 << >> ~>>
     (* Probably not the best hash function, but at least the high order bits affect
        the low order ones.  *)
     local open Word in
        val xval = ref 0w1
        fun pause 0w1 = 0w1  (* Just to check the effect of a slow hash function *)
          | pause n =  (!xval) + n * pause (n-0w1)

	 val anEasyPrime = 0w76543   (* a 16 bit prime *)
	 val shift = 0w15 (*(fromInt wordSize) div 2*) (* 15, 16 or 31, 32 should be fine. *)
	 fun hashWords (x, y) =  (* (pause 0w10) + *)
	     (x + (y * anEasyPrime) + 0w4443337 + (x >> shift) + (y >> shift))
     end

     (* A very poor hash function, but it is associative.
       (It's also commutative.  And very fast.) *)
     (* fun hashWordsAssoc (x, y) = Word.+ (x, y) *)

         
(* Mod not necessary: word operations do not raise exceptions *)
(*      fun hashWords (x, y) = (x mod 0w777337) + ((y mod 0w444337) * 0w1111) + 0w4443337 *)

(* The following uses the twelf implementation of a hash table instead.  It is
   slightly faster, but doesn't propertly implement insertPersistant.  I think
   refinement lattices are only used in a single threaded way, but this needs to be
   properly checked.  - Rowan *)

(*    fun eq_sn2 ((sn11, sn12), (sn21, sn22)) = SortName.eq (sn11, sn21) andalso
                                              SortName.eq (sn12, sn22)
    fun hash_sn2 (sn1, sn2) = hashWords (SortName.idnum sn1, SortName.idnum sn2)

    structure RLmap = HashTableTwelf(type key'=SortName * SortName
                                     val hash = hash_sn2
                                     val eq = eq_sn2)
*)
                                    

    datatype RefLattice = REFLATTICE of SortName RLmap.Table

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

    (* This should really be "sort goals".  Oh well.  I'll fix it later.  - Rowan *)
    datatype TyGoals = TYGOALS of (id, ToRealise * SortScheme list) FinMap.map

    structure TG =
      struct
          val empty : TyGoals = TYGOALS FinMap.empty
          val singleton : id * (ToRealise * SortScheme list) -> TyGoals = 
              TYGOALS o FinMap.singleton
          fun singleton0 (id, sortschs) = singleton (id, (ToRea.empty, sortschs))
          fun add (id, (torea, sortschs), TYGOALS s) = TYGOALS (FinMap.add (id, (torea,sortschs), s))
          fun add0 (id, sortschs, TG) = add (id, (ToRea.empty, sortschs), TG)
          fun plus (TYGOALS s, TYGOALS s') : TyGoals = TYGOALS (FinMap.plus(s, s'))
          fun map f (TYGOALS m) = TYGOALS (FinMap.composemap f m)
          fun lookup (TYGOALS map) (id : id) : (ToRealise * SortScheme list) option = 
            FinMap.lookup map id
          fun layout (TYGOALS m) = 
            FinMap.layoutMap {start="", finish="",sep=", ", eq=" : "} 
                     (PP.LEAF o Ident.pr_id) (PP.layout_pair ToRea.layout 
                     (PP.layout_list RO.layoutSortScheme)) m
      end

(*  Old version before adding alternative goals.
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
*)

    type hash_code = word  (* For hash-consing in variable environments. *)

    datatype Context = CONTEXT of {T: TyNameEnv, E: Env, U: ExplicitTyVarEnv}
         and Env = ENV of {SE: StrEnv, TE: TyEnv, RE: SortEnv, 
                           VE: VarEnv, EE: ExConEnv, TG: TyGoals}
         and StrEnv = STRENV of (strid, Env) FinMap.map
         and TyEnv = TYENV of (tycon, SortFcn) FinMap.map
         and TyNameEnv = TYNAMEENV of (TyName, TyStr) FinMapEq.map
         and TyStr = TYSTR of {CE: ConEnv, R:SortNameEnv, mlSN : SortName, 
                               covariant:bool, RL: RefLattice}
(*         and SortNameEnvOld = SORTNAMEENVOLD of (SortName, SortCons) FinMapEq.map *)
         and SortNameEnv = SORTNAMEENV of SortCons SortNameMap.Table
         and SortCons = SORTCONS of (con, SortFcn list option) SortedFinMap.map (* "of" sorts *)
         and SortEnv = SORTENV of (tycon, SortFcn) FinMap.map
         and VarEnv = VARENV of (id, VarEnvRange) FinMap.map
         and ConEnv = CONENV of (con, SortScheme) SortedFinMap.map
         and ExConEnv = EXCONENV of (excon, Sort) FinMap.map
(*       and Str = STR of {m: StrName, E: Env} *)


    (* Unused now?  (was: Fix this for modules!) *)
    type Basis = Context

    (*** Layout functions **)
       (* local (pr_srtfcn for layout fns) *)
    fun pr_srtfcn svnames sfcn = #body (RO.pr_SortFcnPRETTY svnames sfcn)

     fun prRL (REFLATTICE m) = (* "<REFINEMENT LATTICE NOT PRINTED>" *)
        let val outstr = ref "" 
            val _ = 
              RLmap.app (fn ((sn1, sn2), sn3) => 
                          outstr := !outstr ^ 
                          "(" ^ (SortName.pr_SortName ("", sn1)) ^ "\t^\t" ^
                                (SortName.pr_SortName ("", sn2)) ^ "\t=\t" ^
                                (SortName.pr_SortName ("", sn3)) ^ ")\n")
			m
	in
	    !outstr
	end

    fun layoutRL RL = PP.LEAF (prRL RL)
(*       FinMapEq.layoutMap {start="intersect ", finish="",sep="", eq=" = "}  *)
(*         (fn (sn1, sn2) => PP.NODE *)
(*                {start = "(", finish = ")", childsep = PP.RIGHT ") with (", indent=1, *)
(*                 children = [SortName.layout sn1, SortName.layout sn2]}) *)
(*         SortName.layout m *)

    fun layoutCE (CONENV m) =
      SortedFinMap.layoutMap {start="", finish="",sep=", ", eq=" : "} 
      (fn id => PP.LEAF ("con " ^ Ident.pr_id id)) RO.layoutSortScheme m

    fun layoutRCsvn svnames (SORTCONS m) =
      let

(*  WAS:  (until 13nov03)
        fun layout_sfcns sfcns = 
            PP.NODE{start="", finish="", indent=0, childsep=PP.LEFT " | ", 
                    children=map (PP.LEAF o (pr_srtfcn svnames)) sfcns}

        fun doit (id, SOME sfcns) = 
            PP.NODE{start="", finish="", indent=0, childsep=PP.RIGHT " of ",
                    children=[PP.LEAF (Ident.pr_id id), layout_sfcns sfcns] }
          | doit (id, NONE) = PP.LEAF (Ident.pr_id id)
*)
        fun layout_id_sfcn id sfcn = PP.LEAF (Ident.pr_id id ^ " of " ^ pr_srtfcn svnames sfcn)
          (* PP.NODE{start="", finish="", indent=0, childsep=PP.RIGHT " of ",
                    children=[PP.LEAF (Ident.pr_id id), PP.LEAF (pr_srtfcn svnames sfcn)] } *)

        fun doit (id, SOME sfcns) = map (layout_id_sfcn id) sfcns
(*            PP.NODE{start="", finish="", indent=2, childsep=PP.LEFT " | ",
                    children=map (layout_id_sfcn id) sfcns }  *)
          | doit (id, NONE) = [PP.LEAF (Ident.pr_id id)]

        val RClist = SortedFinMap.Fold (op ::) [] m
        val childnodes = case RClist of [] => [PP.LEAF "<EMPTY>"]
                                      |  _ => ListHacks.flatten (map doit RClist)
      in
        PP.NODE{start="  ", finish="", indent=2,
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

    fun RtoOld m =  (* Convert to the old representation for reporting. *)
        let val current = ref FinMapEq.empty
            val () = SortNameMap.app 
			 (fn (sn, sc) => current := FinMapEq.add SortName.eq (sn, sc, !current) )
			 m
	in
	    (!current)
	end

    fun layoutR (SORTNAMEENV m) =  (* USED BY TopLevelReport! *)
      let val svnames = ref (RO.newSVNames ())
          fun pr_sn sn = (svnames :=  RO.newSVNames ();    (* Need new svnames for each.  *)
                          pr_srtfcn (!svnames) (RO.SortName_in_SortFcn sn))
	  val m_old = RtoOld m
      in
        FinMapEq.layoutMapLeftSep {start="", finish="",sep=nspaces 999, eq=" = "}
                                  (fn sn => PP.LEAF ("datasort " ^ pr_sn sn))
				  (layoutRCsvnIndented (!svnames))
				  m_old
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

    fun reportLayoutC C = PP.reportStringTree (layoutC C)

    (********
    Refinement Lattices
    ********)

     fun sn2_eq ((sn11, sn12), (sn21, sn22)) = 
         (SortName.eq (sn11, sn21)) andalso (SortName.eq (sn12, sn22))

     val emptyRL = REFLATTICE (RLmap.new 10) (*FinMapEq.empty*)
			                  (* Not in signature.  Only used in "on_RL". *)
     fun isSingleRL (REFLATTICE m) = 
	 let val flag = ref true  (* Only one element if the lattice map is empty. *)
	     fun setFlag _ = (flag := false)
	     val _ = RLmap.app setFlag m
	 in
	     !flag
	 end
           (*FinMapEq.isEmpty m*)  (* Not in signature, as yet. *)

     fun singleRL sortname = emptyRL

     fun conjoinRL' (REFLATTICE m) (sn1, sn2) =
       case SortName.eq (sn1, sn2)
         of true => SOME sn1 
          | false => case SortName.leq (sn1, sn2)
                       of true => RLmap.lookup m (sn1, sn2)
                        | false =>  RLmap.lookup m (sn2, sn1)

     fun conjoinRL RL (sn1, sn2) =
       case conjoinRL' RL (sn1, sn2)
         of SOME sortname => sortname
          | NONE => if !(Flags.lookup_flag_entry "elaboration_error_found") then  sn1 (* Elab err *)
                    else  sn1 before     (* Should crash, but this is more useful for debugging. *)
                          debug_print_must (fn () =>
                                               ["BUG: RefinedEnvironments.conjoinRL " ^ 
                                      (SortName.pr_SortName ("", sn1)) ^ ", " ^
                                      (SortName.pr_SortName ("", sn2)) ^ 
                                      "\nRL = \n " ^ (prRL RL) ^ "\n"] )

     fun conjoinListRL' _  [] = Crash.impossible "RefinedEnvironments.conjoinListRL'"
       | conjoinListRL' RL (sortname::sortnames) =
         List.foldl (fn (x, SOME r) => conjoinRL' RL (x, r)
                      | (_, NONE) => NONE)
                     (SOME sortname)
                     sortnames

     fun conjoinListRL RL (sortname::sortnames) =
         List.foldl (conjoinRL RL) sortname sortnames

     fun subSortNameRL RL (sortname, sortname') =
              SortName.eq (sortname, sortname')
       orelse SortName.eq (conjoinRL RL (sortname, sortname'), sortname)

     (* warning - this doesn't complete the lattice *)
(*     fun RL_plus_RL (REFLATTICE m, REFLATTICE m') = 
         REFLATTICE (FinMapEq.plus sn2_eq (m, m')) *)

     fun addRL (REFLATTICE m, (sn1, sn2), sn3 : SortName) =
       case SortName.eq (sn1, sn2)
         of true => REFLATTICE m
          | false => case SortName.leq (sn1, sn2)
                       of true => REFLATTICE (RLmap.insertPersistant m ((sn1, sn2), sn3))
                        | false => REFLATTICE (RLmap.insertPersistant m ((sn2, sn1), sn3))
       
     val bogusRL = emptyRL

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
      Set.list(FinMapEq.dom TyName.eq map)

    fun restrictT (TYNAMEENV m) isRemoved = 
	TYNAMEENV (FinMapEq.filter (fn (t, _) => not (isRemoved t)) m)


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

    fun conjSortNameT T (sn1, sn2) =
       if SortName.eq (sn1, sn2) then  sn1
       else  (case lookupT (T, SortName.tyname sn1)
                of SOME (TYSTR {RL, ...}) => conjoinRL RL (sn1, sn2)
                 | NONE => 
                   if !(Flags.lookup_flag_entry "elaboration_error_found") then  sn1 (* Elab err *)
                    else  sn1 before     (* Should crash, but this is more useful for debugging. *)
                          debug_print_must (fn () => 
                                             ["BUG: REnv.conjoinSortNameT.  Type name missing: (" ^ 
                                               (SortName.pr_SortName ("", sn1)) ^ ", " ^
                                               (SortName.pr_SortName ("", sn2)) ^ ")\n"] ) )

    fun subSortNameT T (sortname, sortname') =
              SortName.eq (sortname, sortname')
       orelse SortName.eq (conjSortNameT T (sortname, sortname'), sortname)

(*     fun Tremove T *)

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

    (* Intersect all in sorts1 with all in sorts2, removing non-maximal and empty elements. *)
    fun productConj_sorts _ ([], _) = []
      | productConj_sorts conjSN (head_sort1::tail_sorts1, sorts2) =
          let val res_tail = productConj_sorts conjSN (tail_sorts1, sorts2)
              val conjSN2 = (SOME o conjSN)
              fun mkconj_sort_simpl (s1, s2) = 
                  if RO.subSort conjSN (s1, s2) then s1
                  else if RO.subSort conjSN (s2, s1) then s2
                  else RO.mkSortConj conjSN2 (s1, s2)
              val res_head = map (fn s2 => mkconj_sort_simpl (head_sort1, s2)) sorts2
              fun conjSNprint args = (out_debug (fn () => "HERE"); conjSN args)
              val res_head_non_empty = List.filter (fn s => not (RO.emptySort conjSNprint s)) res_head
          in
              foldl (RO.addSortToMaximals conjSN) res_tail res_head_non_empty
          end

    (* Intersect all in sortfcns1 with all in sortfcns2, removing non-maximal and empty elements. *)
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
                    of NONE => Crash.impossible "RefinedEnvironments.intersectRCmax"
                     | SOME sortfcns =>
                       case productConj_sortfcns conjSN (sortfcns, sortfcns')
                         of [] => mb
                          | new_sortfcns =>
                            SortedFinMap.add Ident.< (con, SOME new_sortfcns, mb)
                 )
          )
          SortedFinMap.empty
          m
        )

    fun RC_to_CE T (sortname, SORTCONS sortcons) =
      let
        val _ = debug_push (fn () => ["RC_to_CE"])
        val (sortvars, sort) = 
          RO.unSortFcn (RO.SortName_in_SortFcn sortname)
        val sortscheme = RO.mkSortScheme(sortvars, sort)
        val sortsvs = map RO.mkSortSortVar sortvars
        fun mk_arrow sfcn = RO.mkSortArrow (RO.applySortFcn (sfcn, sortsvs), sort)
        fun mkSortConj (sort1, sort2) = RO.mkSortConj (conjSortNameT' T) (sort1, sort2)
        val res = 
        CONENV (SortedFinMap.composemap
                  (fn NONE => sortscheme  (* No "of" argument for this constructor *)
                    | SOME sfcns =>       (* c :> All sortvars. sort1 -> sortname & ...  *)
                      RO.mkSortScheme (sortvars,  
                                       ListHacks.foldl' mkSortConj (map mk_arrow sfcns)))
                  sortcons
               )
        val _ = debug_pop (fn() => ["RC_to_CE:END"])
      in
        res
      end

    (********
    SortName Environments
    ********)
    local open SortNameMap in

      val emptyR: SortNameEnv = SORTNAMEENV (new 10)

      fun singleR ((sn, sc): SortName * SortCons): SortNameEnv = 
	  SORTNAMEENV (insertPersistant (new 10) (sn,sc))

      fun R_plus_R(SORTNAMEENV t, SORTNAMEENV t'): SortNameEnv =
          (* SORTNAMEENV (plus (t, t')) *)
	  let val out_t = copy t
              val () = app (fn entry => insert out_t entry) t'
          in
              SORTNAMEENV out_t
	  end


      fun addR(SORTNAMEENV t, sortname, sortcons): SortNameEnv =  (* Not in REFINED_ENV yet. *)
	SORTNAMEENV (insertPersistant t (sortname, sortcons))

(*      fun Rmap (f: SortCons -> SortCons) (SORTNAMEENV m): SortNameEnv =
	  SORTNAMEENV (FinMapEq.composemap f m)
*)
      fun Rfold (f: (SortCons * 'a) -> 'a) (start: 'a) (SORTNAMEENV m): 'a = 
        let val current = ref start
            val () = app (fn (sn, sc) => current := f (sc, !current) ) m
	in
	    !current
	end

      fun RFold (f: ((SortName * SortCons) * 'a) -> 'a) (start: 'a) (SORTNAMEENV m): 'a = 
        let val current = ref start
            val () = app (fn (sn, sc) => current := f ((sn, sc), !current) ) m
	in
	    !current
	end

      fun Rdom R =
        RFold (fn ((sn, sc), acc) => sn :: acc) [] R

      fun lookupR(SORTNAMEENV m, SortName: SortName.SortName): SortCons option =
        lookup m SortName

    end
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
                  val cnjs2 = SortName.conjuncts sn2  (* Should almost always work. *)
                  fun tycon_of sortname = TyName.tycon (SortName.tyname sortname)
              in
                (case (cnjs1, cnjs2) of ([sortcon1], [sortcon2]) =>  (* match default sortnames *)
                                          (sortcon1 = tycon_of sn1 andalso sortcon2 = tycon_of sn2)
                                      | _ => false)
                orelse ListHacks.eqSet (cnjs1, cnjs2)
              end
          fun dummyConjSN _ = Crash.impossible "RefinedEnvironments.matchR:dummyConjSN"
      in
	  case (Rdom1, Rdom2) 
	    of ([sn1], [sn2]) => Rea.singleSN (sn1, sn2)
	     | _ =>
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

    val emptyVE: VarEnv = (VARENV FinMap.empty)

    fun VEdom (VARENV m) = FinMap.dom m

    val bogus_VE = emptyVE

    (* These are used to keep track of which variables might have many different sorts assigned. *)
    (* Variables must originally be added to an environment via singleVE or singleVarVE. *)
    val manySortVars : id list ref = ref []
    fun clearManySortVars () = manySortVars := []
    fun addManySortVar id = if not (List.exists (fn id2 => id=id2) (!manySortVars)) then
				((*print (Int.toString (length (!manySortVars) )^ "\t" ); *)
				 manySortVars := id::(!manySortVars) )
			    else ()
  
    fun singleVE (v as (id, LONGVAR _): id * VarEnvRange): VarEnv =
       (addManySortVar id;
	VARENV (FinMap.singleton v) )
      | singleVE v = VARENV (FinMap.singleton v)

    fun singleVarVE(id: id, sortscheme: SortScheme): VarEnv =
       (addManySortVar id;
        VARENV(FinMap.singleton(id, LONGVAR sortscheme)) )

    fun removeVE (VARENV v,id) = 
      VARENV(case FinMap.remove(id,v) of NONE => v
                                       | SOME v' => v')

    fun VE_plus_VE (VARENV v, VARENV v'): VarEnv =
      VARENV(FinMap.plus (v, v'))

    fun addVE (id, venvrng, VARENV m) = VARENV (FinMap.add (id, venvrng, m))

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
               ((not (List.exists (fn id2 => id=id2) (!manySortVars))) orelse (* Always same sort *)
                (case lookupVE(VE2, id) 
                   of SOME(LONGVAR sscheme2) =>
                      let
                        val (svs, srt1) = RO.instance_vars sscheme1
                        val srt2 = RO.instance(sscheme2, map RO.mkSortSortVar svs)
                      in
                        RO.subSort (conjSortNameT T) (srt1, srt2)
                      end
                    | _ => Crash.impossible "RefinedEnvironments.subVE" ) )
               | _ => true)
             true
             VE1

    (* Inefficient! *)
    fun eqVE T (VE1, VE2) = subVE T (VE1, VE2) andalso subVE T (VE2, VE1)

    fun inManySortVars id = List.exists (fn id2 => id=id2) (!manySortVars) 

(*
    fun sntx_eqVE (VE1 as VARENV m, VE2) =       (* DOESN'T assume vars in same order *)
      foldl (fn ((id, LONGVAR sscheme1), r) =>
               r andalso                       
               ( (* (not (inManySortVars id)) orelse *) (* Always same sort *)
                 (case lookupVE(VE2, id)
                   of SOME(LONGVAR sscheme2) =>
                        RO.sntx_eqsrtsch (sscheme1, sscheme2)
                    | _ => Crash.impossible "RefinedEnvironments.subVE" ) )
               | (_, r) => r)
             true
             (FinMap.list m)
*)

    (* Folding this way is faster: FinMap.foldl is O(n^2), with n including shadowed
       bindings, even from previous files for top-level bindings! *)
    fun sntx_eqVE (VE1 as VARENV m, VE2) =       (* DOESN'T assume vars in same order *)
      foldl (fn (id, acc) =>
               acc andalso                       
                 (case lookupVE(VE1, id)
                   of SOME(LONGVAR sscheme1) =>
                      (case lookupVE(VE2, id)
			of SOME(LONGVAR sscheme2) =>
                           RO.sntx_eqsrtsch (sscheme1, sscheme2)
			 | _ => Crash.impossible "RefinedEnvironments.subVE" )
		    | _ => acc) )
             true
             (!manySortVars)


    (* This isn't currently used because it's slower... *)
    fun trimVE VE = foldl (fn (id, acc) => case lookupVE (VE, id)
			                     of SOME (LONGVAR ssch) => addVE (id, LONGVAR ssch, acc)
					      | _ => acc)
			  emptyVE 
			  (!manySortVars)

(*
    fun trimVE VE =
       VEFold (fn ((id, LONGVAR ssch), acc) => if inManySortVars id then (* (id, LONGVAR ssch) :: acc *)
			                           addVE (id, LONGVAR ssch, acc)
					       else acc
		| (_, acc) => acc)
              emptyVE
              VE
*)

    (********
    Constructor environments
    ********)

    val emptyCE: ConEnv = CONENV SortedFinMap.empty
    val singleCE: con * SortScheme -> ConEnv = CONENV o SortedFinMap.singleton

    fun CE_plus_CE (CONENV m, CONENV m'): ConEnv =
        CONENV(SortedFinMap.plus Ident.< (m, m'))

    fun CE_plus_CE_conjCons (T : TyNameEnv) (CONENV m, CONENV m'): ConEnv =
      let
        val _ = debug_push (fn() =>["CE_plus_CE_conjCons"])
        val conjSortName = conjSortNameT' T
        val plusSortScheme = RO.plus_SortScheme conjSortName
        val res = CONENV(SortedFinMap.mergeMap plusSortScheme m m')
        val _ = debug_pop (fn () => ["CE_plus_CE_conjCons"])
      in
        res
      end

    (* Enrich the first CE by adding the second, and forming all intersections
       required by distributivity.  Redundant conjuncts are removed.  *)
    fun CE_plus_CE_and_intersections (T : TyNameEnv) (CONENV m, CONENV m'): ConEnv =
      let
        val conjSortName = conjSortNameT' T
      in
        CONENV(SortedFinMap.mergeMap (RO.combineSortSchemes conjSortName) m m')
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

    fun lookupCE(CONENV m, con): SortScheme option = 
      SortedFinMap.lookup m con 

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
          val phi = matchR (R1, R2)  (* might raise No_match *)
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
      EqSet.list(FinMap.dom map)        (* MEMO: TIDY UP! *)

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
      EqSet.list(FinMap.dom map)        (* MEMO: TIDY UP! *)

    fun lookupRE(SORTENV m, sortcon: sortcon): SortFcn option =
      FinMap.lookup m sortcon

    fun convertREtoTE (SORTENV m) = TYENV m

    fun overrideDefaults T1 RE =
	REFold (fn ((srtcon, sortfcn),T) => 
                  case RO.SortFcn_to_SortName sortfcn 
		    of NONE => T
                     | SOME sn => 
			let val tn = SortName.tyname sn  in
			    if TyName.tycon tn = srtcon then
				case lookupT (T1, tn) 
				  of SOME (TYSTR {CE, R, mlSN, covariant, RL}) => 
                                        T_plus_T (T, singleT (tn, TYSTR {CE=CE, R=R, mlSN=sn, 
									 covariant=covariant, RL=RL}))
			    else T
			end)
               emptyT RE

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
      EqSet.list(FinMap.dom map)        (* MEMO: TIDY UP! *)

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
    fun SE_of_E (ENV{SE,...}) = SE

    fun lookupE_strid (ENV {SE, ...}, strid: strid): Env option =
      lookupSE(SE, strid)

    fun lookupE_tycon (ENV {TE, ...}, tycon: tycon): SortFcn option =
      lookupTE(TE, tycon)

    fun E_erase_TG (ENV {SE, TE, RE, VE, EE, TG}) =
	ENV {SE = SE, TE = TE, RE = RE, VE = VE, EE = EE, TG = TG.empty}



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

    fun C_erase_TG (CONTEXT {T, E, U}): Context =
        CONTEXT {T = T, E = E_erase_TG E, U=U }

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

    fun Lookup_longid (CONTEXT{E, T, ...}, longid) =
        let
          val (strid_list, id) = Ident.decompose longid
        in
          case Traverse E strid_list
            of NONE => NONE
             | SOME (ENV {VE, ...}) => 
	       (case lookupVE(VE, id) of SOME (LONGCON ssch) =>
                 (let  (* Lookup the actual sort scheme in T.  *)
		    val (_, srt) = RO.instance_vars ssch
                    val srtcons = case RO.unSortArrow (RO.firstConjunct srt)
				    of NONE => srt
                                     | SOME (srt1, srt2) => srt2
(*                    val _ = print (RO.pr_Sort srtcons) *)
		    val SOME (_, sn) = RO.unSortCons srtcons
                    val tn = SortName.tyname sn
                    val SOME tystr = lookupT (T, tn)
                    val CE = CE_of_TyStr tystr
                    val SOME out_ssch = lookupCE (CE, id)
		  in
                    SOME (LONGCON out_ssch)
		  end handle _ => (debug_print_must (fn () => ["POSSIBLE BUG: Lookup_longid: " ^ Ident.pr_id id]);
				   SOME (LONGCON ssch)))  (* Shouldn't happen, but should be fine anyway... - Rowan *)
                | result => result)

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
                                           | NONE => (map print
							  ((*fn() =>*)"TNtoSN CRASHED.  T = " ::
								   debug_layout (layoutT (T_of_C C))
								  @ ["tyname = ", 
								     TyName.pr_TyName tyname]);
                                                      debug_pop (fn()=>[]);
						      Crash.impossible "rEnv.TNtoSN"))

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
      fun conjSorts srts = 
        ListHacks.foldl' (fn (srt, acc) => RO.mkSortConj (conjSortNameT' (T_of_C C)) (srt, acc))
                         srts
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

     (* Some types for subSortName0, below. *)     
     (* In a goal, the LHS is a conjunction, and the RHS is a disjunction of conjunctions. *)
     type subSortNameGoal = SortName list * SortName list list
     type subSortNameAssumptions = subSortNameGoal list   (* Could do better than a list here *)

     (* Some code for memoizing in a hash table for subSortName below *)
     fun eqsns (sns1, sns2) = 
         (List.length sns1 = List.length sns2) andalso
         List.all SortName.eq (ListPair.zip (sns1, sns2))

     fun eqsnsl (snsl1, snsl2) = 
         (List.length snsl1 = List.length snsl2) andalso
         List.all eqsns (ListPair.zip (snsl1, snsl2))

     fun eqSSN ((sns11, snsl21), (sns12, snsl22)) = 
         eqsns(sns11, sns12) andalso eqsnsl(snsl21, snsl22)

     fun hashsns sns = List.foldl hashWords anEasyPrime
				  (map (Word.fromInt o SortName.idnum) sns)
     fun hashSSN (sns1, sns_list2) = 
       hashWords (hashsns sns1, List.foldl hashWords anEasyPrime (map hashsns sns_list2))

     exception memoHashExn;

     structure GoalTable =
     struct
        type key = subSortNameGoal
        type value = bool * subSortNameAssumptions
        type table = (key, value) HashTable.hash_table
        (*[ val mkTable :> (key -> word) * (key * key -> bool) -> (int * exn) -> table ]*)
        val mkTable = (HashTable.mkTable (*[ :>  (key -> word) * (key * key -> bool) -> (int * exn) -> table ]*))
		        : (key -> word) * (key * key -> bool) -> (int * exn) -> table
        (*[ val insert :> table -> key * value -> unit ]*)
        val insert = HashTable.insert 
        (*[ val find : table -> key -> value option ]*)
        val find = HashTable.find
     end

     (* val memoSSN :> (subSortNameGoal, bool * subSortNameAssumptions) HashTable.hash_table *)
     (*val memoSSN : (subSortNameGoal, bool * subSortNameAssumptions) HashTable.hash_table
       = HashTable.mkTable (hashSSN, eqSSN) (65533, memoHashExn)  *)

     val memoSSN = GoalTable.mkTable (hashSSN, eqSSN) (65533, memoHashExn)

     val unionSN = ListHacks.unionEq eqSSN

     fun pr_ssns (sns1, sns_list2) = (pr_SortNames sns1) ^ " <| " ^
		      (ListHacks.stringSep "" "" " | " pr_SortNames sns_list2)

     fun pr_assumptions header assms = 
	 (fn () => header ^ " length = " ^ Int.toString (length assms) ::
		   (map pr_ssns assms) )

     val reported_size_assumptions = ref 0

     (* subSortName0 is the core function for checking inclusion of datasorts. *)
     (*[ val subSortName0 :> TyNameEnv * bool * subSortNameAssumptions -> 
                             subSortNameGoal -> bool * subSortNameAssumptions           ]*)
     fun subSortName0 _ ([], _) = Crash.impossible "RefinedEnvironments.subSortName0(1)"
       | subSortName0 (T, memo_success, assumptions) (sortnames1 as (sn1::_), sortnames_list2) =
       let val _ = debug_push (fn ()=> ["subSName0TOP: " ^ (pr_SortNames sortnames1) ^ " <| " ^
                                      (ListHacks.stringSep "" "" " | " pr_SortNames sortnames_list2)])
           val result = 
       (if TyName.pr_TyName (SortName.tyname sn1) = "paragraph" then 
	    debug_subsortname := true
	else ();
        (* add_indent 4; *)
        (* Optimize the case of a RHS with a single disjunct (with many conjuncts). *)
        case sortnames_list2 of [sortnames2 as _::_::_] => 
               foldl (fn (sn2, (false, _)) => (false, assumptions)
                       | (sn2, (true, r_assumptions)) =>
                                      let val (res, r_assumptions2) = 
					      subSortName0 (T, memo_success, r_assumptions) 
							   (sortnames1, [[sn2]])
				      in (res, r_assumptions2)
				      end )
		     (true, assumptions)
                     sortnames2
        | _ =>  (* otherwise *)
       case lookupT (T, SortName.tyname sn1)
         of NONE => Crash.impossible "RefinedEnvironments.subSortName0(2)"
          | SOME (TYSTR {CE, R, RL, covariant, ...}) =>
       if isEmptyCE CE then    (* sn1 is not a datatype *)
          let val conj1 = conjoinListRL RL sortnames1 in   (* Use the lattice to solve the goal.  *)
            out_debug (fn ()=>"NOT DATATYPE");
            (List.exists (fn x => subSortNameRL RL (conj1, x))
                        (map (conjoinListRL RL) sortnames_list2), 
	     assumptions)
          end
       else  (* Try to determine the goal anyway using RL.  *)
       case (case conjoinListRL' RL sortnames1 of
               SOME conj1 =>(let val conjs_opt2 = map (fn sns2 => conjoinListRL' RL (conj1::sns2))
						      sortnames_list2
			     in  if List.exists (fn NONE => false 
						  | SOME sn =>  SortName.eq (conj1, sn))
				                conjs_opt2
				 then SOME true
				 else (case conjs_opt2 of [SOME _] => SOME false  (* Single conjunct *)
					  	        | _ => NONE) (* Union or not in lattice. *)
			     end, [conj1])
             | NONE => (NONE, sortnames1)) (* Can't conjoin LHS. *)
         of (SOME res, _) => (res, assumptions)
          | (NONE, sortnames1) =>  (* sortnames1 is simplified to a single sortname, if possible *)

       let
	 fun tryConjList sns = case conjoinListRL' RL sns of SOME sn => [sn] 
                                                             | NONE => sns
         val sortnames_list2 = map tryConjList sortnames_list2

         val expandSNs =   (* Expand sortnames to their conjuncts *)
           List.foldl (fn (x, r) => (SortName.sortNameConjuncts x) @ r) []

         fun leq a b = SortName.leq (a, b)
         (* Use the following to test the effect of reversing the order. *)
(*         fun leq a b = SortName.eq (a, b) orelse not (SortName.leq (a, b)) *)

         fun leq_snlist [] _ = true
           | leq_snlist _ [] = false
           | leq_snlist (h1::t1) (h2::t2) = 
             leq h1 h2 andalso (not (SortName.eq (h1, h2)) orelse leq_snlist t1 t2)

         fun uniqSorted eq [] = []
           | uniqSorted eq [h] = [h]
           | uniqSorted eq (h1::(t as h2::_)) = if eq(h1, h2) then (uniqSorted eq t)
                                                else h1::(uniqSorted eq t)
         fun uniqAndSort sortnames = 
           uniqSorted SortName.eq (ListSort.sort leq (expandSNs sortnames))

         (* subset checks inclusion of sorted sortname lists.  It's used by matches, and later.  *)
         (*[ val subset :> SortName list * SortName list -> bool ]*)
         fun subset ([], _) = true
           | subset (_, []) = false
           | subset (h1::t1, h2::t2) = 
             leq h2 h1 andalso
             (if SortName.eq (h1, h2) then subset(t1, t2) else subset(h1::t1, t2))

         (* Check that all on one side are included in one on the other side. *)
         (* This actually slows things down compared to the version above that omits subsorting. *)
         fun subSN' (sn1, sn2) = case conjSortNameT' T (sn1, sn2) 
				  of SOME sn => SortName.eq (sn1, sn) | NONE => false
         fun subset' (sns1, sns2) = List.all (fn x => List.exists (fn y => subSN' (y, x)) sns2) sns1

         fun subset'' (sns1, sns2) = let val res = subset (sns1, sns2)
					 val res2 = subset' (sns1, sns2)
					 val _ = if res2 andalso not res then 
						     print ("EXTRA INCLUSION: " ^ pr_SortNames sns2
							    ^ "\t<=\t" ^ pr_SortNames sns1 ^ "\n")
						 else ()
				     in
					 res
				     end

	 (*val subset = subset'*) (* Change this to use a different version of subset *)

(* Use the following to test the effect of matching assumptions by equality. *)
(*         fun subset ([], []) = true
	   | subset (h1::t1, h2::t2) = SortName.eq (h1, h2) andalso subset (t1, t2)
           | subset _ = false
*)

         (* Match a goal against the assumptions.  *)
         (*[ val matches :> SortName list * SortName list list 
			    -> SortName list * SortName list list 
			    -> bool ]*)
         fun matches (sortnames11, sortnames_list22) 
		     (sortnames : SortName list, sortnames_list : SortName list list) =
             subset (sortnames, sortnames11) andalso 
             List.all (fn x => List.exists (fn y => subset (y, x)) sortnames_list22)
                      sortnames_list

(* Use the following to test the effect of matching assumptions by equality. *)
(*         fun matches (sns1, []) (sns2, []) = subset (sns1, sns2)
	   | matches (sns1, h1::t1) (sns2, h2::t2) = subset (h1, h2) andalso matches (sns1,t1) (sns2, t2)
	   | matches _ _ = false
*)
         val sortnames1' = uniqAndSort sortnames1
         val sortnames_list2' =
           uniqSorted eqsns (ListSort.sort leq_snlist (map uniqAndSort sortnames_list2))
         val matches_goal = matches (sortnames1', sortnames_list2')
         val sns1_snsl2' = (sortnames1', sortnames_list2')
	 val result_opt = 
	     case GoalTable.find memoSSN sns1_snsl2'
              of SOME (b,[]) => (debug_print (fn ()=>["subsortname: HIT MEMOIZED RESULT"]); 
				 SOME (b, assumptions) )(* memoized *)
               | NONE => NONE
       in
          case result_opt of SOME result => result
            | NONE => 
       let  
         val conjSN = conjSortNameT' T  (* Conjoin sortnames using T. *)

         fun subSortopt assumptions2 (NONE, []) = (false, assumptions2) (* No arg, RHS is empty. *)
           | subSortopt assumptions2 (NONE, _) = (true, assumptions2)  (* No arg, RHS non-empty. *)
           | subSortopt assumptions2 (SOME sfcn, sfcn_opts) =
            let val (svs, sort) = RO.applySortFcnVars sfcn
                val sortsvs = map RO.mkSortSortVar svs
                val new_assumption = (sortnames1', sortnames_list2')
                val r_assumptions = ref (unionSN (assumptions2, [new_assumption]))
                fun subSN sns_arg = (* Accumulate assumptions using a reference.  *)
		    let val (res, r_assumptions_new) = 
			    subSortName0 (T, memo_success, !r_assumptions)
					 sns_arg
			val _ = if res andalso !XDuce_extra_assumptions then 
				    r_assumptions := r_assumptions_new 
				else ()
(*
			val len_assume = length (r_assumptions_new)
			val _ = if  len_assume > !reported_size_assumptions then
				    print ("subSortName0: length r_assumptions = " ^ Int.toString len_assume ^ "\n")
				    before (reported_size_assumptions := len_assume)
				else  ()
*)
		    in
			res
              	    end
                fun pr_sort srt = RO.pr_SortPRETTY (RO.newSVNames()) srt
                fun pr_sorts [] = [" < "]
		  | pr_sorts (h::t) = ("<  " ^ pr_sort h) :: (map (fn s => " | " ^ pr_sort s) t)
		val srts2 = map (fn NONE => Crash.impossible "RefinedEnvironments.subSortopt"
			 	  | SOME sfcn' => RO.applySortFcn (sfcn', sortsvs))
			        sfcn_opts
		val _ = debug_push (fn () => "subSortOpt:START" 
					     :: "   " ^ pr_sort sort :: pr_sorts srts2)

		val result = 
		    RO.subSortList (subSN, conjSN) (* Check subsorting, using subSN.  *)
				   (sort, srts2)
		val _ = debug_pop (fn () => "subSortOpt:END " ^ Bool.toString result ::
					    "   " ^ pr_sort sort :: pr_sorts srts2)

		val return_assumptions = if result andalso !XDuce_extra_assumptions then 
					      !r_assumptions 
					 else assumptions2
            in
              (result, return_assumptions)
            end

         (* Helper function with accumulator for expandConj *)
         (* For some reason it goes much faster with this than directly recursive (on tarjan.sml).*)
         (* Probably it has to do with the order that sorts are intersected. *)
         (* Reversing the sortname inputs does avoid some of this effect, but not all. *)
         (*[ val expC :> con * SortName list * SortFcn option -> SortFcn option list ]*)
         fun expC (con, [], sortfcn_opt) = [sortfcn_opt]
           | expC (con, sortname::t, sortfcn_opt) =
             case lookupR (R, sortname)
               of NONE => Crash.impossible "RefinedEnvironments.expC(1)"
                | SOME RC => 
                  (case (lookupRC (RC, con), sortfcn_opt)
                     of (NONE, _) => []
                      | (SOME NONE, NONE) => expC (con, t, NONE)
                      | (SOME (SOME sortfcn_list), SOME sf) =>
                         ListHacks.flatten 
			     (map (fn sf' => let val conj = RO.conjSortFcn conjSN (sf, sf')
					     in (*if #1 (subSortopt assumptions (SOME conj, []))
						then [] (* Checking for an empty intersection *)
						else *)        (* actually slows things down. *)
                                                expC (con, t, SOME conj)
                                             end)
				  sortfcn_list)

                      | _ => Crash.impossible "RefinedEnvironment.expC(2)"
                  )
         (* Expand a conjunction by looking up a constructor, then multiplying out alternatives *)
         (* e.g. muliplying out: (c of R_1 | c of R_2) & (c of S_1 | c of S_2) 
                 yields:         (R_1 & S_1) | (R_1 & S_2) | (R_2 & S_1) | (R_2 & S_2)  *)
         (* One simplification might be to use intersectRC, which does essentially 
            the same thing. A version of intersectRC that memoized on sortnames might even
            speed things up - careful though because RC's are improved.  - Rowan 6oct04. *)
         (*[ val expandConj :> con * SortName list -> SortFcn option list ]*)
         fun expandConj (_, []) = Crash.impossible "RefinedEnvironment.expandConj(1)"
           | expandConj (con, sortname::sortnames) = 
           case lookupR (R, sortname)
             of NONE => Crash.impossible "RefinedEnvironments.expandConj(2)"
              | SOME RC => 
                 case lookupRC (RC, con)
                   of NONE => []                            (* con absent from datasort body. *)
                    | SOME NONE =>
(* 		        (case sortnames of [] => [NONE]
 					 | _ => expandConj (con, sortnames))
*)                        expC (con, sortnames, NONE)
                    | SOME (SOME sortfcn_list) =>
                       ListHacks.flatten (map (fn sf' => expC (con, sortnames, SOME sf')) 
			 		      sortfcn_list)
(*
		      case sortnames of [] => map SOME sortfcn_list
                                      | _ =>
		      let val expanded_sortnames = expandConj (con, sortnames)
                          val conj_sfcn = RO.conjSortFcn conjSN
			  fun conj_exp_sns sf' = map (fn SOME sf => SOME (conj_sfcn (sf, sf')))
						     expanded_sortnames
		      in
                        ListHacks.flatten (map conj_exp_sns sortfcn_list)
		      end
*)

         (* Reversing the order of the output for expandConj' made the sort checker take 40 times
            as long for one example! (tarjan.sml)  We depend on the fact that sortnames are sorted
            by idnum, and earlier ones are more likely to be in the lattice. *)
(*	 fun expandConj' (arg, sns) = let val res = (expandConj (arg, sns))
				    val _ = if length res > 1 then
					       (print "\nexpandConj result = \n"; 
						map (fn SOME sf => print ("\t" ^ pr_srtfcn (RO.newSVNames ()) sf ^ "\n")
						      | _ => ()) res)
					    else []
			       in
				   res
			       end

	 val expandConj = expandConj'
*)
(*
                     let val res = 
                       ListHacks.flatten (map (fn sf' => expC (con, sortnames, SOME sf')) 
			 		      sortfcn_list)
                         val _ = print "\nexpandConj result = \n"
                         val _ = map (fn SOME sf => print ("\t" ^ pr_srtfcn (RO.newSVNames ()) sf ^ "\n")) res
		     in
			  res
		     end
*)
(*                    List.foldl (fn (sf', r) => (expC (con, sortnames, SOME sf')) @ r)
                                 []
                                 sortfcn_list
                )
*)


         val _ = debug_push (fn ()=> ["subSortName0: " ^ (pr_SortNames sortnames1') ^ " <| " ^
                                      (ListHacks.stringSep "" "" " | " pr_SortNames sortnames_list2')])
         val (res, r_assumptions) =   (* First case is an optimization *)
           if List.exists (fn sns => subset (sns, sortnames1')) sortnames_list2' then
              (true, assumptions)
           else 
	   case List.find matches_goal assumptions of
	       (SOME assumption) => (true, assumptions)
	     | NONE => 

           (* See the BITS subdirectory for a modification of the following to use intersectRC. *)
           foldl    (* Expand for each constructor to get sub-goals.  *)
             (fn (con, (false, _)) => (false, assumptions)
               | (con, (true, r_assumptions)) =>

               let val sortoptRHS =
                 foldl (fn (sn2',r) => (expandConj (con, sn2')) @ r) [] 
                       sortnames_list2' 
               in
                 foldl (fn (sortoptLHS, (false, _)) => (false, assumptions)
                         | (sortoptLHS, (true, r_assumptions2)) => 
					    let val (res3, r_assumptions3) = 
						    subSortopt r_assumptions2 (sortoptLHS, sortoptRHS)
					    in  (res3, r_assumptions3)
		                            end 
                       )
                       (true, r_assumptions)
                       (expandConj (con, sortnames1'))
               end)
             (true, assumptions)  (* Result and assumptions *)
             (domCE CE)

         val _ = if (memo_success orelse (not res)) then
                   GoalTable.insert memoSSN (sns1_snsl2', (res, []))
                 else   (* Memoize successes with assumptions when memo_success is false. *)
                   ()


          val len_assume = length (r_assumptions)
          (* val _ = debug_indent := orig_indent *)
          val _ = if false (* len_assume > !reported_size_assumptions *) then
		      (debug_print_must (fn () => ["subSortName0 END: length r_assumptions = "
						   ^ Int.toString len_assume ^ ""]);
                       debug_print_must (pr_assumptions "IN ASSUMPTIONS:" assumptions);
                       debug_print_must (pr_assumptions "OUT_ASSUMPTIONS:" r_assumptions);
                       reported_size_assumptions := len_assume)
                  else  ()
          val r_assumptions = if res andalso !XDuce_extra_assumptions then 
                                  (* The following union shouldn't be needed... *)
                                  r_assumptions
				  (* unionSN (r_assumptions, [(sortnames1', sortnames_list2')]) *)
			      else assumptions
          val () = debug_pop (fn () => [("subSortName0: " ^ (pr_ssns (sortnames1', sortnames_list2')) ^
   				           " RESULT: " ^ (Bool.toString res))])
       in
         (res, r_assumptions)
       end
       end)
           val _ = debug_pop (fn ()=> ["subSName0TOP: " ^ (pr_SortNames sortnames1) ^ " <| " ^
                                      (ListHacks.stringSep "" "" " | " pr_SortNames sortnames_list2)
				       ^ " RESULT: " ^ (Bool.toString (#1 result))])
       in
	   result

       end  (* of subSortName0 *)

    fun subSortName T sns12 =
        let val (res, assms) = (subSortName0 (T, false, []) sns12)
            fun memoize sns1_snsl2 = GoalTable.insert memoSSN (sns1_snsl2, (true, []))
            val () = if res then app memoize assms else ()
	in
	    res
	end

(*      if #1 (subSortName0 (T, false, []) sns12) then
        #1 (subSortName0 (T, true, []) sns12)        (* Repeat to force memoization of true subgoals. *)
      else    (* Repeating works because no false assumptions are made, since they are memoized. *)
        false (* On failure, we've already memoized as much as we can.  *)
*)


    (* MAIN LATTICE CREATION CODE FOLLOWS. *)

    val DEBUG_completeT = ref false

    (* Use associativity to fill in RL with the intersections of conj = (newSN & sn)
       with each sn in snsRL.  RL must have all intersections filled in for 
       "newSN", and all intersections filled in for "sn" with each sortname in snsRL.  *)
    fun assoc_completeRL (RLpartial, newSN, newConjs, sortnames) = 
        let
          fun addConjs (sn, conj, RL, snsRL) =
            List.foldl (fn (old_sn, RLacc) =>
                         addRL (RLacc, (conj, old_sn),
                                conjoinRL RL (conjoinRL RL (sn, old_sn), newSN)))
                       RL
                       snsRL

          val (completeRL, _) =   (* Fill in lattice for conjunctions of newSN. *)
            List.foldl (fn ((sn, conj), (RL, snsRL)) =>
                          (addConjs (sn, conj, RL, snsRL), conj::snsRL))
                       (RLpartial, sortnames)
                       newConjs        (* list of (sn, conj), conj = sn & sortname *)
        in
          completeRL
        end

    (* Create a modified T for TRs, creating conjunctions to complete the lattice.  *)
    (* TRs contains the raw RCs directly from a datasort declaration. *)
    (* TRs may contain "syntactic conjunctions" which will be simplified in the result. *)
    (* The SortNames in TRs will be renamed when they are equivalent to previous SortNames.  *)
    (* The same renaming and simplification is applied to RE, and the result returned. *)
    (* Also returns an enriched VE. *)
    (*    [Perhaps this function does too much, and it could be broken up?]  *)
    fun completeT (C : Context, TRs : (TyName * SortNameEnv) list, RE : SortEnv) 
        : TyNameEnv * SortEnv * VarEnv =
      let
        val out_debug = out_debug_maybe DEBUG_completeT
        val _ =  debug_push (fn () => "completeT: START" :: 
			      ListHacks.flatten (map (fn (T, R) => debug_layout (layoutR R)) TRs))

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
        (* It would be slightly better to add the "richer" RL to T in find_eq_sortname.  *)
        val subSortName' = subSortName TsubSortName        (* Compares RC's when necessary. *)
        (* find_eq_sortname is called with conjuncts containing one or two sortnames. *)
        fun find_eq_sortname RL (conjuncts, sns) =  (* Scan sns for an equiv. to a conjunction *) 
          List.find (fn sn => (debug_push(fn ()=>["find_eq_sortname loop: -------------------------"
                                               ^ " SortName: " ^ (SortName.pr_SortName ("", sn))] );
                       case
                         (case map (fn conjunct => conjoinRL' RL (sn, conjunct)) conjuncts of
			      [SOME conj_sn1, SOME conj_sn2] => 
			          if not (SortName.eq (conj_sn1, sn)) orelse 
				     not (SortName.eq (conj_sn2, sn)) then
				      (debug_push (fn() => ["find_eq_sortname: FOUND BOTH"]);
                                       debug_pop (fn () => []); 
				        SOME false)
				  else SOME (subSortName' (conjuncts, [[sn]]))
                                     
			    | [_, SOME conj_sn] => 
			        if not (SortName.eq (conj_sn, sn)) then 
				    SOME false (* not (sn < conjunct) *)
				else (SOME (subSortName' (conjuncts, [[sn]]) andalso
							 subSortName' ([sn], [[hd conjuncts]])) )
			    | [SOME conj_sn, _] => 
			        if not (SortName.eq (conj_sn, sn)) then 
				    SOME false (* not (sn < conjunct) *)
				else (SOME (subSortName' (conjuncts, [[sn]]) andalso
							 subSortName' ([sn], [tl conjuncts])) )
			    | _ => NONE)
			of SOME res => (debug_pop (fn() => ["find_eq_sortname: FOUND"]); 
					res)
			 | _ => 
                               (subSortName' (conjuncts, [[sn]]) andalso
                                subSortName' ([sn], [conjuncts]))
                                before (debug_pop (fn ()=>["find_eq_sortname loop: END------------",
                                                        " SortName: " ^ 
							(SortName.pr_SortName ("", sn))] )) )
		    )
                    sns

         fun tryComponents _ _ [] _ = NONE
	   | tryComponents RL' newSN (conj :: conjuncts) acc_conjuncts = 
             case conjoinListRL' RL' (newSN :: conj :: conjuncts @ acc_conjuncts) of
		 SOME conj_sn => SOME conj_sn
	       | NONE =>   (* Try intersecting newSN first with all except conj. *)
		   case conjoinListRL' RL' (conjuncts @ acc_conjuncts @ [newSN, conj]) of
		       SOME conj_sn => SOME conj_sn
		     | NONE => tryComponents RL' newSN conjuncts (conj::acc_conjuncts)

        (* Add newSN = sortcons to tystr, including conjunctions, but with no CE (yet).
           If newSN has an equivalent in tystr, then instead add it to a "renaming" list.
           Returns the new tystr, the renaming list, and the sortnames actually added. *)
        fun addSortname (tyname, tystr as (TYSTR {CE, R, mlSN, covariant, RL}), 
                         newSN, sortcons) 
                      : TyStr * (SortName * SortName) list * SortName list = 
          case find_eq_sortname RL ([newSN], Rdom R)
            of SOME (eq_sortname) =>
                   (SortName.improve_variance eq_sortname (SortName.variance newSN) ;
                    (tystr, [(newSN, eq_sortname)], []))  (* BUG: we do need to modify the CE. *)
                                                          (* For this sortname and intersections.*)
             | NONE =>       (* Need to add newSN to tystr *)
            let
              val sortnames = Rdom R  
              
              val (newR, newRL, _, newConjs) =  (* Fold over R to fill in the conj's for newSN *)
                RFold                                      (* cnjs = info for the created sn's *)
                  (fn ((sn, sc), (R', RL', sns, cnjs)) =>        (* sns = all sns in R' so far *)

                    (* This cuts the total sort checking time for some examples by 1/3 *)
                    case tryComponents RL' newSN (SortName.sortNameConjuncts sn) [] of 
	 	       SOME conj_sn =>
		       ( (*debug_push (fn () => ["addSortName: found conjunction:  \t" ^ 
						SortName.pr_SortName ("", newSN) ^ "  \t" ^
                                                SortName.pr_SortName ("", sn) ^ "  \t" ^
						SortName.pr_SortName ("", conj_sn)]); 
                          debug_pop (fn() => []); *)
                          SortName.improve_variance conj_sn (SortName.variance newSN) ;
                          (R', addRL (RL', (newSN, sn), conj_sn), sns, cnjs)    )

                     | NONE =>

                    case find_eq_sortname RL' ([newSN, sn], sns) (* inv princ. strengthened later *)
                      of SOME eq_sn => (SortName.improve_variance eq_sn (SortName.variance newSN) ;
			                (R', addRL (RL', (newSN, sn), eq_sn), sns, cnjs) )
                       | NONE =>   (* make sortname for the conjunction "newSN & sn" *)
                         let val empty = subSortName' ([newSN, sn], [])      (* check emptiness *)
                             val variance = SortName.join_variance (SortName.variance newSN,
					                            SortName.variance sn)
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
                  (* start RFold with sortname added to R *)
                  ( R_plus_R (R, singleR (newSN, sortcons)), RL, newSN::sortnames, [] )
                  R   (* Fold over R *)

(*              fun conjVia sn1 conjsn = SortName.sortNameConjuncts

              val newRL2 = addRL (
*)
              val completeRL = assoc_completeRL (newRL, newSN, newConjs, sortnames)
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
        val newC = C_plus_T(C, changedT)  (* Sufficient for allRefinements in unionSF below. *)
        (* fullT is used by unionSF. *)
        val fullT = T_plus_T(TsubSortName, changedT)  (* include both old and new SNs *)
        val conjSN = conjSortNameT newT                            
        (* Find the maximal sortfcns contained in a union.  We need to apply this to the "of"   *)
        fun unionSF [] = []  (*  sorts if we want completeness with respect to regular-tree     *)
          | unionSF [sfcn] = [sfcn]  (* grammars.  But, if the programmer always includes maximal*)
          | unionSF (hsfcn::tsfcns) =  (* elements in datasort declarations, this isn't needed. *)
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
                        (out_debug(fn ()=>"ADDING TO CONSTRUCTOR SORT");
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
          List.foldl                         (* previously made RC and CE principal via unions *)
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
          let val _ = debug_push (fn () => ["RCimprove"])
	   val RCres = 
           if not (!improve_inversion_principles) then RC else
           RFold (fn ((with_sn, withRC), accRC) => if subSortNameT renamedT (sn, with_sn)
                                                 then intersectRCmax conjSN (accRC, withRC)
                                                 else accRC)
          
                 RC
                 withR  (* Fold over withR, improving RC *)
              val _ = if !DEBUG_completeT then pr ("RCimprove RESULT: ", (layoutRC RCres)) else ()
              val _ = debug_pop (fn () => ["RCimprove:END"])
          in
            RCres
          end

        val renamedRE =   (* Apply the renaming also to the RE (input to completeT) *)
          REmap (fn sortfcn => 
                   case RO.SortFcn_to_SortName sortfcn
                     of NONE => sortfcn
                      | SOME sn => RO.SortName_in_SortFcn (renameSN renaming sn))
                RE

        (* CE_plus_newR takes a constructor environment and adds new
           conjuncts to it for a set of new datasort definitions.  *)

        fun CE_plus_newR (CE : ConEnv, R : SortNameEnv) =
            RFold (fn ((sn, RC), accCE) =>  
                      CE_plus_CE_and_intersections newT (accCE, RC_to_CE newT (sn, RC))
                      (* We want to add thisCE to CE, but not redundant conjuncts.
                         We also need to intersect each conjunct with each conjunct of CE.  *) 
                      )
                  CE
                  R

        val _ = debug_push (fn () => ["Calculating constructor sorts and inversion principles."])
        val (T_withCE, changedVE) =
          List.foldl     (* Fold over TRs, adding to CEs in renamedT and building VE from CEs *)
            (fn ((tyname, inR), (T, VE)) =>  (* fold over TRs *)
               (case lookupT (T, tyname)
                  of NONE => Crash.impossible "RefinedEnvironments.completeT(4)"
                   | SOME (TYSTR {CE, R, mlSN, covariant, RL}) => 
                     let 
                         val renamed_inR = renameRdom(renSimpR (inR, Rdom inR))
                         val _ = debug_push (fn() =>["Whole RFold in completeT"])
                         val (outCE1, outR) =   
                            RFold (fn ((sn, RC), (accCE, accR)) =>  (* fold over R *)
                                let 
                                    val _ = debug_push (fn() =>["Body of RFold in completeT"])
                                    val improvedRC1 = RCimprove (renamed_inR, sn, RC)
                                    val improvedRC =   (* improve with all of R if sn is new *)
                                        if List.exists (fn new_sn => SortName.eq(sn, new_sn))
                                                       allNewSns
                                          then RCimprove(R, sn, improvedRC1)
                                          else improvedRC1
                                    val unioned_RC = if !add_unions_to_constructor_sorts then 
                                                       RCmap unionSFopt RC
                                                     else RC
                                    val _ = debug_push (fn() =>["RC_to_CE in completeT"])
                                    val thisCE = RC_to_CE newT (sn, unioned_RC)
                                    val _ = debug_pop (fn() =>["RC_to_CE in completeT"])
                                    val _ = debug_push (fn() =>["res in completeT"])
                                    val res = (CE_plus_CE_conjCons newT (accCE, thisCE),
					       R_plus_R (accR, singleR(sn, improvedRC)))
                                    val _ = debug_pop (fn() =>["res in completeT:END"])
				    val _ = debug_pop (fn() =>["Body of RFold in completeT:END"])
				in
				    res
                                end)   (* End of fn for RFold. *)
                           (emptyCE, emptyR)
                           R    (* R includes conjunctions and old SNs.  (inR excludes them) *)
                         val _ = debug_pop (fn() =>["Whole RFold in completeT:END"])
                         val outCE = if !add_unions_to_constructor_sorts then outCE1
                                        else CE_plus_newR (CE, renamed_inR)
                         val outVE = VE_plus_VE (VE, CE_to_VE outCE)
                         val tystr = TYSTR{CE=outCE, R=outR, mlSN=mlSN, covariant=covariant, RL=RL}
                         val outT = T_plus_T (T, singleT(tyname, tystr))
                     in (outT, outVE)
                     end)
            )
            (renamedT, emptyVE)
            TRs
        val _ = debug_pop (fn () => ["Calculating constructor sorts and inversion principles: FINISHED."])

        val _ = debug_pop (fn () => ["completeT(end): "])
      in
        (T_withCE, renamedRE, changedVE)
      end  (* of completeT *)


    structure Rea = struct  (* See Environments.Realisation *)
      open RO.Realisation
      type trealisation = RO.trealisation

      type conjSN = SortName * SortName -> SortName

      (* This is slightly different to on_SortName' in RefObject.sml *)
      fun on_SortName' conjSN phi sn = 
          case RO.SortFcn_to_SortName (on_SortName conjSN phi sn) of SOME sn2 => sn2

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
      fun on_ToRea conjSN phi torea = 
          ToRea.map (fn sn => case RO.SortFcn_to_SortName (on_SortName conjSN phi sn) 
                                of SOME sn2 => sn2 ) torea
      fun on_TyGoals conjSN phi TG = 
          TG.map (fn (torea, sortschemes) => 
                     (on_ToRea conjSN phi torea, map (on_SortScheme conjSN phi) sortschemes) )  TG

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
            RFold (fn ((sn, RC), accR) => 
                              addR (accR, (on_SortName' conjSN phi sn), on_RC conjSN phi RC)  )
	          emptyR
                  R
(*
            FinMapEq.Fold (fn ((sn, RC), accR) => 
                              addR (accR, (on_SortName' conjSN phi sn), on_RC conjSN phi RC)  )
              emptyR
              m
*)
  
    fun on_RL conjSN phi (REFLATTICE m) =
          let  val () = debug_push_must
                            (fn () => "on_RL:top: phi = " :: debug_layout (layout phi)
                                              @ debug_layout (layoutRL (REFLATTICE m)))
               val onSN = on_SortName' conjSN phi
               val newRL = RLmap.new 10
               fun ins ((sn1, sn2), sn3) = 
		   let val (newsn1, newsn2) = (onSN sn1, onSN sn2)
                       val pairsn = if SortName.leq (newsn1, newsn2) then (newsn1, newsn2) 
				    else (newsn2, newsn1)
		   in
                       RLmap.insert newRL (pairsn, onSN sn3)
		   end
	       val _ = RLmap.app ins m
               val () = debug_pop (fn () => "on_RL:end:"::debug_layout (layoutRL (REFLATTICE newRL)))
	  in
            (REFLATTICE newRL)
(*            FinMapEq.Fold (fn (((sn1, sn2), sn3), accRL) => 
                                addRL (accRL, (onSN sn1, onSN sn2), onSN sn3)  )
                emptyRL
                m *)
          end

      fun on_TyStr conjSN (phi : realisation) (TYSTR {CE, R, mlSN, covariant, RL}) =
            TYSTR {CE = CEmap (on_SortScheme conjSN phi) CE, 
                   R = on_SortNameEnv conjSN phi R, 
                   mlSN = (case RO.SortFcn_to_SortName (on_SortName conjSN phi mlSN) of
			       SOME sn => sn), 
                   covariant=covariant, RL= on_RL conjSN phi RL} (* 13jan03 Added on_RL (again).  *)

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
                                        (*   | NONE => raise No_match   (*Shouldn't occur.*) *)
          val (CE1, R1, mlSN1, covariant1, RL1) = unTyStr tystr1
          val ENV {SE, TE, RE as (SORTENV mRE), VE, EE, TG} = E
          (* LATER: add check for datatype *)
          val Tmerge = map (fn t2 => case lookupT (T, t2) of SOME tystr => tystr) ts
          val mTrest = FinMapEq.filter (fn (t3, _) => 
                                           not (List.exists (fn t4 => TyName.eq (t3, t4)) ts) )
                                       (case T of TYNAMEENV m => m)
          val phi = foldl (fn (tystr2, phi2) => Rea.oo (conjSortNameT T) 
                                                       phi2 (matchTyStr (tystr2, tystr1)))
                          Rea.Id Tmerge         (* Might raise No_match here *) 
          val tystr_new = tystr1  (* LATER: allow different tystrs to be merged *)
          val Tout = T_plus_T (TYNAMEENV mTrest, singleT (t1, tystr_new))
          val conjSN = conjSortNameT T
        in
          (Rea.on_TyNameEnv conjSN phi Tout, Rea.on_Env conjSN phi E)
        end

    fun RL_add_abstract ( C: Context, tyname: TyName, sortcon: sortcon, variance: Variance list) 
           : TyNameEnv * SortName =
        let 
            val sn_new =
                 SortName.freshSortName {conjuncts=[sortcon], variance=variance, tyname=tyname}
            val SOME tystr = lookupT(T_of_C C, tyname)
            val TYSTR {CE, R=Rold, mlSN, covariant, RL=RLold} = tystr
            val sns_old = Rdom Rold

            fun mk_new_conj sn =
                 (sn, SortName.newConjSortName (sn_new, sn, 
                                                SortName.isEmpty sn orelse SortName.isEmpty sn_new, 
                                                variance, tyname))
            val sns_new_conjs = map mk_new_conj sns_old

            val RL2 = List.foldl (fn ((sn, sn_conj), RLacc) => 
				    addRL (addRL (addRL (RLacc, (sn, sn_new), sn_conj), (sn_new, sn_conj), sn_conj),
                                           (sn, sn_conj), sn_conj)
                                 )
                                 RLold sns_new_conjs  (* Conjs are subsorts.  *)

            (* Add conjunctions of sns_new_conjs *)
            val RL3 = assoc_completeRL (RL2, sn_new, sns_new_conjs, sns_old)

            (*val _ = pr ("RL2 =\n", layoutRL RL2)
              val _ = pr ("RL3 =\n", layoutRL RL3) *)

            val new_conjs = map #2 sns_new_conjs
            fun add_sn_to_R (sn, R) = addR (R, sn, emptyRC) (* emptyRC for non-datatype *)

            val Rnew = List.foldl add_sn_to_R Rold (sn_new :: new_conjs)

            val tystr_new = TYSTR {CE=emptyCE, R=Rnew, mlSN=mlSN, covariant=covariant, RL=RL3}
            val Tnew = singleT (tyname, tystr_new)
        in
            (Tnew, sn_new)
        end

    (* Collapse two sortnames into one.  Collapses subsorts also.  *)    
    fun RL_collapse (T : TyNameEnv, sn1 : SortName, sn2: SortName) : realisation = 
	let
            val tyname = SortName.tyname sn1
            val SOME tystr = lookupT(T, tyname)
            val TYSTR {CE, R=Rold, mlSN, covariant, RL=RLold} = tystr
            val sns_old = Rdom Rold

            val conjSN = conjoinRL RLold
            val subSN = subSortNameRL RLold

            val sn1_sn2 = conjSN (sn1, sn2)
            val sns_not_below_sn1_sn2 = List.filter (fn sn => not (subSN (sn, sn1_sn2))) sns_old 
            val sns_below_sn1 = List.filter (fn sn => (subSN (sn, sn1))) sns_not_below_sn1_sn2
            val sns_below_sn2 = List.filter (fn sn => (subSN (sn, sn2))) sns_not_below_sn1_sn2
            val sns_renaming = sns_below_sn1 @ sns_below_sn2
            (* Basically every sn below sn1 or sn2 is mapped to sn & sn1_sn2 *)
(*
            val _ = print ("sn1 = " ^ SortName.pr_SortName ("", sn1)^ "\n")
            val _ = print ("sn2 = " ^ SortName.pr_SortName ("", sn2)^ "\n")
            val _ = print ("sn2_sn2 = " ^ SortName.pr_SortName ("", sn1_sn2)^ "\n")
            val _ = print "sns_not_below_sn1_sn2 = "
            val _ = map (fn sn => print (SortName.pr_SortName ("", sn) ^ "  ")) 
			sns_not_below_sn1_sn2
            val _ = print "\n"
            val _ = print "sns_renaming = "
            val _ = map (fn sn => print (SortName.pr_SortName ("", sn) ^ "  ")) sns_renaming
            val _ = print "\n"
*)
            (* This could cause trouble if a non-conjunct sortname is removed from the lattice.
               If this happens, we should find all conjunct sortnames that mention that sortname,
               and "refresh" them so that their conjuncts are all in the lattice. *)
            val phi1 = foldl (fn (sn, acc) => 
				 Rea.plus acc (Rea.singleSN (sn, conjSN (sn, sn1_sn2))))
                             Rea.Id sns_renaming
(*            val _ = pr ("phi1 = ", Rea.layout phi1) *)
            (* Use this to map back to a sensible representitive. This could be improved. *)
            val phi2 = foldl (fn (sn, acc) => 
				 Rea.plus acc (Rea.singleSN (conjSN (sn, sn1_sn2), sn)))
                             Rea.Id sns_renaming
(*            val _ = pr ("phi2 = ", Rea.layout phi2) *)
            val phi = Rea.oo conjSN phi2 phi1

            (* Look for non-conjunct sort names that are not mapped to themselves by phi *)
	in
	    phi
        end


(*
    (* Add a sortname to a non-datasort lattice (or find an equivalent in the lattice).  *)
    (* sub_sns and super_sns are lower and upper bounds. *)
    (* RETURNS: the new part of T : TyNameEnv, and the new (or old) sortname.  *)    
    fun extend_lattice ( C: Context, tyname: TyName, sortcon: sortcon, variance: Variance list,
                         sub_sns: SortName list, super_sns: SortName list ): TyNameEnv * SortName =
        let val SOME tystr = lookupT(T_of_C C, tyname)
            val TYSTR {CE, R=Rold, mlSN, covariant, RL=RLold} = tystr
            val (Tnew, sn_new) = RL_add_abstract (C, tyname, sortcon, variance)
            val conjSNnew = conjSortNameT Tnew
            val sn_new_renamed = foldl (fn (sn, acc_sn) => conjSNnew (sn, acc_sn)) sn_new super_sns
            val phi = RL_collapse (Tnew, sn_new, sn_new_renamed)    
            (*val _ = pr ("RLold = ", layoutRL RLold)
            val _ = pr ("phi = ", Rea.layout phi) *)
            val Trenamed = Rea.on_TyNameEnv conjSNnew phi Tnew
            val SOME (TYSTR {CE, R=Rold, mlSN, covariant, RL}) = lookupT(Trenamed, tyname)
            (* val _ = pr ("RL = ", layoutRL RL) *)
	in
	    (Trenamed, Rea.on_SortName' conjSNnew phi sn_new)
	end
*)



    (* Add a sortname to a non-datasort lattice (or find an equivalent in the lattice).  *)
    (* sub_sns and super_sns are lower and upper bounds. *)
    (* RETURNS: the new part of T : TyNameEnv, and the new (or old) sortname.  *)    
    fun extend_lattice ( C: Context, tyname: TyName, sortcon: sortcon, variance: Variance list,
                         sub_sns: SortName list, super_sns: SortName list ): TyNameEnv * SortName =
        let val sn_new =  (* FIX: need to add variance to signature.  Also, a single U.B? *)
                 SortName.freshSortName {conjuncts=[sortcon], variance=variance, tyname=tyname}
            val SOME tystr = lookupT(T_of_C C, tyname)
            val TYSTR {CE, R=Rold, mlSN, covariant, RL=RLold} = tystr
            val sns_old = Rdom Rold
            val conjSN = conjoinRL RLold
            val subSN = subSortNameRL RLold
	    val super_sn =
                case super_sns
                 of super_sn::sns => foldl conjSN super_sn sns
                  |  _ => Crash.unimplemented 
                           "RefinedEnvironments.extend_lattice: must have upper bound."
                          (* In general, we need to check that the conjunction of the upper bounds
                             is above each lower bound.  Or, we need to collapse the lattice. *)

            val RL1 = addRL (RLold, (sn_new, super_sn), sn_new)  (* Since sn_new <= super_sn. *)
            fun below_super sn = subSN (sn, super_sn) andalso not (SortName.eq (sn, super_sn))
            val sns_below_super = List.filter below_super sns_old 
            val sns_not_below_super = List.filter (fn sn => not (subSN (sn, super_sn))) sns_old 
            val sns_above_super = List.filter (fn sn => (subSN (super_sn, sn)) andalso 
					                not (SortName.eq (sn, super_sn)) )
					      sns_old 
            val sns_not_above_super = List.filter (fn sn => not (subSN (super_sn, sn))) sns_old 

            (* FIX: Consider whether sn_new is an empty sort? *)
            fun mk_new_conj sn =
                 (sn, SortName.newConjSortName (sn_new, sn, SortName.isEmpty sn, 
                                                variance, tyname))
            val sns_new_conjs = map mk_new_conj sns_not_above_super

            val RL2 = List.foldl (fn ((sn, sn_conj), RLacc) => 
				    addRL (addRL (RLacc, (sn,sn_new), sn_conj),
				           (sn_new, sn_conj), sn_conj) )
                                 RL1 sns_new_conjs

            val conjSN2 = conjoinRL RL2
            fun conj_via_super sn = conjSN2 (conjSN (sn, super_sn), sn_new) (* Uses assoc. of & *)
        
            val RL3 = List.foldl (fn (sn, RLacc) => addRL (RLacc, (sn, sn_new), conj_via_super sn))
                                 RL2 sns_above_super
            (* Add conjunctions of sns_new_conjs *)
            val RL4 = assoc_completeRL (RL3, sn_new, sns_new_conjs, sns_old)
            (* val _ = pr ("RL3 = ", layoutRL RL3)
               val _ = pr ("RL4 = ", layoutRL RL4) *)
            val new_conjs = map #2 sns_new_conjs
            fun add_sn_to_R (sn, R) = addR (R, sn, emptyRC) (* emptyRC for non-datatype *)

            val Rnew = List.foldl add_sn_to_R Rold (sn_new :: new_conjs)

            val tystr_new = TYSTR {CE=emptyCE, R=Rnew, mlSN=mlSN, covariant=covariant, RL=RL4}
            val Tnew = singleT (tyname, tystr_new)
        in
            (Tnew, sn_new)
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

    fun reportCE(CONENV m) =
      SortedFinMap.reportMapSORTED
        (fn (con, sortScheme) =>
           let
             val names = RO.newSVNames()
	     val hcnj::tcnj = map (RO.pr_SortPRETTY names) 
		                  (RO.list_Conjuncts (#2 (RO.instance_vars sortScheme)))
           in
             Report.decorate ("con " ^ Ident.pr_id con ^ " <:",
                              Report.line ("  " ^ hcnj)
                              // Report.flatten (map (fn str => Report.line ("& " ^ str))
						     tcnj)
			     )
           end
        ) m

    (* Not used anymore *)
    fun reportT (TYNAMEENV m) =
      FinMapEq.reportMapSORTED
        (fn (tyname1, tyname2) => 
           TyCon.< (TyName.tycon tyname1, TyName.tycon tyname2))
        (fn (tyname, TYSTR {CE,...}) => 
           Report.indent(2, reportCE CE)
           // Report.line "")
        m

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
                        ^ (if true (* bindings *) then " = " ^ body
                           else "")
                        ^ "  |: " ^ tyfcnBody
                        )
          end

    val allowanceForIndent = 12
    val WIDTH = !Flags.colwidth - allowanceForIndent

    val maxLatticeSizePrinted = ref 20
    val _ = Flags.add_int_to_menu (["Extensions"], "maxLatticeSizePrinted", 
				   "Maximum refinement lattice to print ", maxLatticeSizePrinted)
    fun reportRL (RL, R) =
      let
        fun pr_sn sortname = SortName.pr_SortName ("", sortname)
        val (maxLen, sizeR) =    (* determine the maximum column width and number of sorts *)
           RFold (fn ((sortname, _), (len, size)) =>
                       (Int.max (len, String.size (SortName.pr_SortName ("", sortname))),
                        size+1))
                 (0, 0) R
        val width = maxLen + 2
        val cols = Int.max (((WIDTH-2) div width) - 1, 2)
        val final_cols = case (sizeR mod cols) of 0 => cols | n => n
        val (_, sn_groups) = RFold (fn ((sortname, _), (n, h::t)) => 
                                      (case n of 0 => (cols-1, [sortname]::h::t)
                                               | _ => (n-1, (sortname::h)::t) )
                                     | _ => Crash.impossible "RefinedEnvs.sn_groups")
                                   (final_cols, [[]]) R
        fun pad s = s ^ (nspaces (width - (String.size s)))
        fun leftPad s = (nspaces (width - (String.size s))) ^ s

        fun dashes 0 = ""
          | dashes n = "-" ^ (dashes (n-1))
        fun pr_line head f sns = 
          Report.line ((pad head) ^ "| " ^ (List.foldl (fn (sn, r) => r ^ pad (f sn)) "" sns))
        fun pr_group sns = 
          pr_line "" pr_sn sns //   (* Headings for columns *)
          Report.line ((dashes width) ^ "+-" ^ (dashes (width * (length sns)))) //
          RFold (fn ((sn, _), r) => 
                   pr_line (pr_sn sn) (fn sn' => pr_sn (conjoinRL RL (sn, sn'))) sns 
                   // r)
                Report.null R

	fun table_body () = 
	       ListHacks.foldl' (fn (this_rep, acc_rep) => this_rep // Report.line "" // acc_rep)
                        (map pr_group sn_groups)

     in
       Report.line "" // Report.line "Table of Intersections:"
       // (if sizeR <= !maxLatticeSizePrinted then
	       table_body()
	   else
	       Report.line ("[Skipped large table with " ^ Int.toString sizeR ^ " X "
			    ^ Int.toString sizeR ^ " elements.]") )
      // Report.line ""


     end        
                    
    fun reportR R = PP.reportStringTree (layoutR R)

    fun reportTyStr tystr = 
      (if isemptyCE (CE_of_TyStr tystr) then  
             Report.null
       else  reportR (R_of_TyStr tystr) //   (* The RC's in R have strong inv. princples  *)
             reportCE (CE_of_TyStr tystr)     (* while CE has max argument sorts *)
      ) //
      (if (isSingleRL (RL_of_TyStr tystr)) then  Report.line "unique refinement"
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
                 | SOME tyname => lookupT (T, tyname) 
(*
                   case lookupT (T, tyname)
                     of NONE => NONE
                      | SOME tystr =>
                          if isemptyCE (CE_of_TyStr tystr) then  NONE
                          else  SOME tystr
*)
            val heading = case tystr_opt of NONE => "sortdef" 
	                                  | SOME tystr => 
		                              if isemptyCE (CE_of_TyStr tystr) then "sortdef"
					      else "sortdef" (* "datasort" *)
(*	    val report_tystr = case sortconSortfcn_list of (_, sortfcn) *)
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


