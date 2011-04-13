(* RefineError information *)

(*
$File: Common/RefineErrorInfo.sml $
$Date: 2004/10/10 14:56:04 $
$Revision: 1.10 $
$Locker:  $
*)

functor RefineErrorInfo(structure SourceInfo : SOURCE_INFO
                        structure RefObject: REFOBJECT
                        structure Ident : IDENT
                        structure StatObject: STATOBJECT
                          sharing type StatObject.TVNames = RefObject.TVNames
			structure TyCon: TYCON
			structure Report: REPORT
                          sharing type Report.Report = SourceInfo.Report
		       ): REFINE_ERROR_INFO =
  struct
    type Sort = RefObject.Sort
    type SortScheme = RefObject.SortScheme
    type Type = StatObject.Type
    type longsortcon = TyCon.longtycon
    type longid = Ident.longid
    type SourceInfo = SourceInfo.SourceInfo

(*    type SyntaxSortVar = SortVar.SyntaxSortVar *)

    datatype ErrorInfo =
        LOOKUP_LONGCON of longid
      | LOOKUP_LONGSORTCON of longsortcon
      | SORTING of Sort
      | NOT_SUBSORT of Sort * Sort
      | CANT_APPLY of Sort
      | REDEX
      | INCOMPATIBLE of SortScheme * SortScheme
      | WRONG_TYPE of Sort * Type
      | SHADOWED of longsortcon
      | VARIANCE
      | WRONG_ARITY of {expected: int, actual: int}
      | NOT_COVARIANT
      | MULTIPLE_BINDINGS
      | UNMATCHED of Sort
      | NOT_UNIQUE
      | SHARING_REFINEMENTS_DIFFER

    type Error = SourceInfo * ErrorInfo

    type Report = Report.Report
    val line = Report.line
    infix //
    val op // = Report.//

    fun reportInfo (LOOKUP_LONGCON longid) = 
          line ("Unbound value constructor: " ^ (Ident.pr_longid longid))
      | reportInfo (LOOKUP_LONGSORTCON longsortcon) = 
          line ("Unbound sort constructor: " ^ (TyCon.pr_LongTyCon longsortcon))
      | reportInfo (SORTING sort) =
	     let
	       val names = RefObject.newSVNames()
	       val pr = RefObject.pr_SortPRETTY names
	     in
	          line "The expression does not have the required sort:"
	       // line("   " ^ pr sort)
	     end    
      | reportInfo (NOT_SUBSORT (sort1, sort2)) =
	     let
	       val names = RefObject.newSVNames()
	       val pr = RefObject.pr_SortPRETTY names
	     in
	          line("Sort mismatch: " ^ pr sort2)
	       // line("    expecting: " ^ pr sort1)
	     end
      | reportInfo (CANT_APPLY sort) =
	     let
	       val names = RefObject.newSVNames()
	       val pr = RefObject.pr_SortPRETTY names
	     in
	          line "The argument does not match the sort of the function, which is:"
	       // line("   " ^ pr sort)
	     end
      | reportInfo REDEX = 
          line("Can't determine goal sort in redex.")
      | reportInfo (INCOMPATIBLE (sortScheme, sortScheme')) = 
	  let
	    val pr = RefObject.pr_SortSchemePRETTY (RefObject.newSVNames())
	  in
	    line "Sorts refine different types:"
	       // line("   " ^ pr sortScheme)
	       // line("   " ^ pr sortScheme')
	  end
      | reportInfo (WRONG_TYPE (sort, ty)) =
	     let
	       val names = RefObject.newSVNames()
	       val prs = RefObject.pr_SortPRETTY names
               val prt = StatObject.Type.pretty_string (RefObject.TVNamesOfSVNames names)
	     in
	          line "Sort refines the wrong type."               
	       // line("            Sort:" ^ prs sort)
	       // line("   Expected Type:" ^ prt ty)
	     end
      | reportInfo (SHADOWED longsortcon) = 
	     line "The sort with the same name as the type is shadowed"
	   //line ("by a sort refining a different type: " 
		   ^ (TyCon.pr_LongTyCon longsortcon))
      | reportInfo VARIANCE = 
	     line "Sort variable has the wrong variance."
      | reportInfo (WRONG_ARITY{expected, actual}) =
	     line("Wrong arity (expected " ^ Int.toString expected
		  ^ ", actual " ^ Int.toString actual ^ ")"
		 )
      | reportInfo NOT_COVARIANT =
	     line("Refinements can only be defined for types which")
	  // line("do not contain function types or reference types.")

      | reportInfo MULTIPLE_BINDINGS =
	     line("Pattern contains constructors which lead to multiple")
          // line("possible sorts for variables.  Rewritting using 'case' may help.")

      | reportInfo (UNMATCHED srt) =
	     let
	       val names = RefObject.newSVNames()
	       val prs = RefObject.pr_SortPRETTY names
	     in
	          line("WARNING: Patterns do not cover sort:")
	       // line("  " ^ (prs srt))
	     end

      | reportInfo NOT_UNIQUE =
	     line("No unique best sort.  Perhaps add a sort constraint.  ")

      | reportInfo SHARING_REFINEMENTS_DIFFER =
	     line("You cannot share these types because they have different refinements.")

    fun report (sourceInfo, errorInfo) = 
         (SourceInfo.report sourceInfo)
      // Report.indent (2, reportInfo errorInfo)
      // line "------------------------------------------------------------------"

  end;


