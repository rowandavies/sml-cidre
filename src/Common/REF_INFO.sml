
signature REF_INFO =
  sig
    structure ElabInfo : ELAB_INFO
    structure REnv : REFINED_ENVIRONMENTS
    structure Comp : COMP
    structure RefObject : REFOBJECT

    type TyNameEnv = REnv.TyNameEnv
    type Env = REnv.Env
    type VarEnv = REnv.VarEnv
    type Sort = REnv.Sort
    type 'a Memo = 'a Comp.Memo

    (* Make a new structure for MemoTable? *)
    type ('a, 'b) MemoTable
    val lookupMemoVE : VarEnv * (VarEnv, 'b) MemoTable -> 'b Memo ref
    val lookupMemoVEsort : (VarEnv * Sort) * (VarEnv * Sort, 'b) MemoTable -> 'b Memo ref

    datatype RefDecMemo = 
       EMPTY
     | INFERABLE_EXP of (* TyNameEnv * *) (VarEnv, Sort Comp.Redo) MemoTable
     | CHECKABLE_EXP of (* TyNameEnv * *) (VarEnv * Sort, unit) MemoTable
     | DEC_NODEPEND of (TyNameEnv * Env) Comp.Redo Comp.Result
     | DEC_DEPEND of (* TyNameEnv * Env * *) (VarEnv, (TyNameEnv * Env) Comp.Redo) MemoTable

    type RefInfo = ElabInfo.ElabInfo * RefDecMemo ref

    val get_DEPEND : RefDecMemo ref -> (VarEnv, (TyNameEnv * Env) Comp.Redo) MemoTable
    val get_INFERABLE : RefDecMemo ref -> (VarEnv, Sort Comp.Redo) MemoTable
    val get_CHECKABLE : RefDecMemo ref -> (VarEnv * Sort, unit) MemoTable

    val from_ElabInfo  : ElabInfo.ElabInfo -> RefInfo
    val to_ElabInfo : RefInfo -> ElabInfo.ElabInfo
    val to_RefDecMemo : RefInfo -> RefDecMemo ref

    val layout : RefInfo -> StringTree.t

  end

