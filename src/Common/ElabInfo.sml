
functor ElabInfo (structure ParseInfo : PARSE_INFO
                  structure TypeInfo : TYPE_INFO
		  structure OverloadingInfo : OVERLOADING_INFO
                        where type TyVar = TypeInfo.TyVar
		  structure ErrorInfo : ERROR_INFO
                        where type RefineErrorInfo.Report = ParseInfo.SourceInfo.Report
                        where type RefineErrorInfo.SourceInfo = ParseInfo.SourceInfo
                        where type TyVar = TypeInfo.TyVar
			where type tycon = TypeInfo.tycon
		  structure PrettyPrint : PRETTYPRINT
		  structure Crash : CRASH
		    ) : ELAB_INFO =
  struct
    fun impossible s = Crash.impossible ("ElabInfo." ^ s)

    (*types imported from other modules:*)
    structure ParseInfo       = ParseInfo
    structure ErrorInfo       = ErrorInfo
    structure TypeInfo        = TypeInfo
    structure OverloadingInfo = OverloadingInfo
    type ParseInfo            = ParseInfo.ParseInfo
    type ErrorInfo            = ErrorInfo.ErrorInfo
    type TypeInfo             = TypeInfo.TypeInfo
    type OverloadingInfo      = OverloadingInfo.OverloadingInfo
    structure SourceInfo      = ParseInfo.SourceInfo

    (*the type provided by this module:*)
    datatype ElabInfo =
      ELAB_INFO of {ParseInfo       : ParseInfo,
		    ErrorInfo       : ErrorInfo option,
		    TypeInfo        : TypeInfo option,
		    OverloadingInfo : OverloadingInfo option}

    fun plus_ErrorInfo
         (ELAB_INFO {ParseInfo, ErrorInfo=NONE, TypeInfo, OverloadingInfo}) i =
	    ELAB_INFO {ParseInfo=ParseInfo, TypeInfo=TypeInfo, 
		       OverloadingInfo=OverloadingInfo, ErrorInfo=SOME i}
      | plus_ErrorInfo _ _ = impossible "plus_ErrorInfo"

    fun plus_TypeInfo 
          (ELAB_INFO {ParseInfo, ErrorInfo, TypeInfo, OverloadingInfo}) i =
	     ELAB_INFO {ParseInfo=ParseInfo, ErrorInfo=ErrorInfo,
			TypeInfo=SOME i, OverloadingInfo=OverloadingInfo}

    fun plus_OverloadingInfo 
         (ELAB_INFO {ParseInfo,ErrorInfo, TypeInfo, OverloadingInfo}) i =
          ELAB_INFO {ParseInfo=ParseInfo, ErrorInfo=ErrorInfo,
		     TypeInfo=TypeInfo, OverloadingInfo=SOME i}

    fun to field (ELAB_INFO innards) = field innards
    val to_ParseInfo = to #ParseInfo
    val to_ErrorInfo = to #ErrorInfo
    val to_TypeInfo = to #TypeInfo
    val to_OverloadingInfo = to #OverloadingInfo
    fun remove_OverloadingInfo
          (ELAB_INFO {ParseInfo, ErrorInfo, TypeInfo, OverloadingInfo}) =
	     ELAB_INFO {ParseInfo = ParseInfo, ErrorInfo = ErrorInfo,
			TypeInfo = TypeInfo, OverloadingInfo = NONE}

    fun from_ParseInfo ParseInfo =
          ELAB_INFO {ParseInfo=ParseInfo, ErrorInfo=NONE,
		     TypeInfo=NONE, OverloadingInfo=NONE}

    val report_SourceInfo =
            ParseInfo.SourceInfo.report
	  o ParseInfo.to_SourceInfo
	  o to_ParseInfo

    (*retractRight (i1, i2) = an info identical to i1, except
     that the the right-hand position of the source info of i1
     becomes the right-hand position of the source info of i2:*)

    local
      fun PP pos_l pos_r = ParseInfo.from_SourceInfo
                             (SourceInfo.from_positions pos_l pos_r)
      val un_PP = SourceInfo.to_positions o ParseInfo.to_SourceInfo
      val right = #2 o un_PP
      val left = #1 o un_PP
    in
      fun retractRight
          (ELAB_INFO {ParseInfo, ErrorInfo, TypeInfo, OverloadingInfo},
	   ELAB_INFO {ParseInfo = ParseInfo', ...}) =
	     let
	       val ParseInfo'' = PP (left ParseInfo) (right ParseInfo')
	       val ParseInfo''' =
		     (case ParseInfo.to_DFInfo ParseInfo of
			NONE => ParseInfo''
		      | SOME DFInfo1 =>
			  ParseInfo.plus_DFInfo ParseInfo'' DFInfo1)
	     in
	       ELAB_INFO {ParseInfo=ParseInfo''', ErrorInfo=ErrorInfo,
			  TypeInfo=TypeInfo, OverloadingInfo=OverloadingInfo}
	     end
    end (*local*)

    fun layout (ELAB_INFO {ParseInfo, ErrorInfo, TypeInfo, OverloadingInfo}) =
          StringTree.NODE
	    {start="ElabInfo{",
	     finish="}",
	     indent=3,
	     children=
	       [ParseInfo.layout ParseInfo,
		PrettyPrint.layout_opt
		  (fn _ => StringTree.LEAF "<ErrorInfo>") ErrorInfo,
		  (*TODO 28/02/1997 00:57. tho.  there oughtabe a
		   layout function in module ErrorInfo*)
		PrettyPrint.layout_opt TypeInfo.layout TypeInfo,
		PrettyPrint.layout_opt OverloadingInfo.layout
		  OverloadingInfo],
	     childsep=StringTree.RIGHT "; "}
  end;
