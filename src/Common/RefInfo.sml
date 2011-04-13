
(* RefInfo is a hook attached to the AST during Elaboration and modified during
   Refinement.   It is used for memoization.  *)

functor RefInfo (structure ElabInfo : ELAB_INFO
		 structure REnv : REFINED_ENVIRONMENTS
                 structure RefObject : REFOBJECT
                      sharing type RefObject.SortName = REnv.SortName
                      sharing type RefObject.SortFcn = REnv.SortFcn
                      sharing type RefObject.SortScheme = REnv.SortScheme
                      sharing type RefObject.TypeFcn = REnv.TypeFcn
                      sharing type RefObject.TyName = REnv.TyName
                      sharing type RefObject.Type = REnv.Type
                      sharing type RefObject.SortVar = REnv.SortVar
                      sharing type RefObject.Sort = REnv.Sort
                 structure FinMapEq : FINMAPEQ
                 structure Comp : COMP
                 structure PP : PRETTYPRINT
		    sharing type PP.StringTree = ElabInfo.StringTree
 )  : REF_INFO =
  struct
    structure ElabInfo       = ElabInfo
    structure RefObject = RefObject
    structure REnv = REnv
    structure Comp = Comp

    (*types imported from other modules:*)
    type TyNameEnv = REnv.TyNameEnv
    type Env = REnv.Env
    type VarEnv = REnv.VarEnv
    type Sort = REnv.Sort
    type StringTree           = PP.StringTree
    type 'a Memo = 'a Comp.Memo

    type ('a, 'b) MemoTable = ('a, 'b Memo ref) FinMapEq.map ref  (* Fix later *)

    (* Lookup a VE in a memoTable, adding a new entry if not found. *)
    (* f may used to "optimize" x, in this case trim the VE (although not currently) *)
    fun lookupMemo eq f (x, memoTable) = 
      case FinMapEq.lookup eq (!memoTable) x
        of SOME y => y
         | NONE =>
	    let val cell = Comp.newMemoCell ()  (* Not triming is faster, for now. *)
		val _ = (memoTable := FinMapEq.add eq ( (*f*) x, cell, !memoTable))
	    in 
		cell
	    end
    

    (* val lookupMemoVE :> VarEnv * (VarEnv, 'a Memo ref) MemoTable -> 'a Memo ref *)
    fun lookupMemoVE (VE, memoTable) = lookupMemo REnv.sntx_eqVE REnv.trimVE (VE, memoTable)

    fun eqVEsrt ((VE1, srt1), (VE2, srt2)) = RefObject.sntx_eqsrt (srt1, srt2) andalso
					     REnv.sntx_eqVE (VE1, VE2)
    fun trimVEsrt (VE, srt) = (REnv.trimVE VE, srt)
    fun lookupMemoVEsort ((VE, srt), memoTable) = 
	  lookupMemo eqVEsrt trimVEsrt ((VE, srt), memoTable) 
 
    datatype RefDecMemo =
       EMPTY
     | INFERABLE_EXP of (VarEnv, Sort Comp.Redo) MemoTable
     | CHECKABLE_EXP of (VarEnv * Sort, unit) MemoTable
     | DEC_NODEPEND of (TyNameEnv * REnv.Env) Comp.Redo Comp.Result
     | DEC_DEPEND of (VarEnv, (TyNameEnv * REnv.Env) Comp.Redo) MemoTable

    type RefInfo = ElabInfo.ElabInfo * (RefDecMemo ref)

    fun from_ElabInfo ElabInfo =
          (ElabInfo, ref EMPTY)
    fun to_ElabInfo (ElabInfo, _) = ElabInfo
    fun to_RefDecMemo (_, RefDecMemo) = RefDecMemo

    fun new () = ref EMPTY

    fun get_DEPEND (rdmemo : RefDecMemo ref) = 
	case !rdmemo of DEC_DEPEND x => x
		      | EMPTY => let val newTable = ref FinMapEq.empty
				     val () = rdmemo := DEC_DEPEND newTable
				 in  newTable
				 end
    fun get_INFERABLE (rdmemo : RefDecMemo ref) = 
	case !rdmemo of INFERABLE_EXP x => x
		      | _ => let val newTable = ref FinMapEq.empty (* Replace checkable by inferable *)
				     val () = rdmemo := INFERABLE_EXP newTable
				 in  newTable
				 end

    fun get_CHECKABLE (rdmemo : RefDecMemo ref) = 
	case !rdmemo of CHECKABLE_EXP x => x
		      | EMPTY => let val newTable = ref FinMapEq.empty
				     val () = rdmemo := CHECKABLE_EXP newTable
				 in  newTable
				 end
                      | INFERABLE_EXP x => ref FinMapEq.empty (* dummy table, memoize inferred *)


    fun layoutWrapper ElabInfo childNodes = 
      PP.NODE
	    {start="RefInfo{",
	     finish="}",
	     indent=3,
             children = (ElabInfo.layout ElabInfo)::childNodes,
             childsep=PP.RIGHT "; "}

    fun layout (ei, ref EMPTY) = layoutWrapper ei [PP.LEAF "EMPTY"]
      | layout (ei, ref (INFERABLE_EXP memoTable)) = 
          layoutWrapper ei
            [PP.LEAF "INFERABLE_EXP", PP.LEAF "<TyNameEnv>" , PP.LEAF "<MemoTable>"]
      | layout (ei, ref (CHECKABLE_EXP memoTable)) = 
          layoutWrapper ei
            [PP.LEAF "CHECKABLE_EXP", PP.LEAF "<TyNameEnv>" , PP.LEAF "<MemoTable>"]
      | layout (ei, ref (DEC_NODEPEND _ )) = 
          layoutWrapper ei
            [PP.LEAF "DEC_NODEPEND", PP.LEAF "<TyNameEnv>" , PP.LEAF "<REnv>", PP.LEAF "<errs>"]
  end;
