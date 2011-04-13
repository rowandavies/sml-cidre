(* NOT USED CURRENTLY - rowan 25jul01 *)

functor TyGoals(structure DecGrammar: DEC_GRAMMAR

		structure Ident: IDENT
		       sharing type Ident.longid = DecGrammar.longid
		       sharing type Ident.id = DecGrammar.id

                structure PP: PRETTYPRINT
                       sharing type PP.StringTree = DecGrammar.StringTree

		structure FinMap: FINMAP
		  sharing type FinMap.StringTree = PP.StringTree

               ) : TYGOALS = 
struct

      type id = Ident.id
      type ty = DecGrammar.ty

      datatype TyGoals = TYGOALS of (id, ty) FinMap.map

      val empty : TyGoals = TYGOALS FinMap.empty
      val singleton : id * ty -> TyGoals = TYGOALS o FinMap.singleton
      fun add (id, ty, TYGOALS s) = TYGOALS(FinMap.add(id, ty, s))
      fun plus (TYGOALS s, TYGOALS s') : TyGoals =
	    TYGOALS(FinMap.plus(s, s'))
      fun lookup (TYGOALS map) (id : id) : ty option =
	    FinMap.lookup map id

      fun layout (TYGOALS m) = 
      FinMap.layoutMap {start="", finish="",sep=", ", eq=" : "} 
                       (PP.LEAF o Ident.pr_id) DecGrammar.layoutTy m

end
