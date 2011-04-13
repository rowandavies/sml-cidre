
(* Well the actual code isn't here, since the code is the file RefinedEnvironments.sml,
   part of the sort checker itself.
   It sort checks fine unless you've previously checked "tarjan2.sml" or "tarjan.sml".  In which
   case it gets stuck at the following: *)

     type subSortNameGoal = SortName list * SortName list list
     type subSortNameAssumptions = subSortNameGoal list   (* Could do better than a list here *)

     val memoSSN : (subSortNameGoal, bool * subSortNameAssumptions) HashTable.hash_table
       = HashTable.mkTable (hashSSN, eqSSN) (65533, memoHashExn)


(* Adding a sort annotation fixed this problem, but it still suggests that a better approach
   polymorphic instantiation is desirable. *)