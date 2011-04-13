functor RefineInfo (structure ParseInfo : PARSE_INFO
		  structure ErrorInfo : ERROR_INFO
		    sharing type ErrorInfo.Report = ParseInfo.SourceInfo.Report
                  structure RefineErrorInfo : REFINE_ERROR_INFO
		  structure TypeInfo : TYPE_INFO
		  structure SortInfo: SORT_INFO
		  structure OverloadingInfo : OVERLOADING_INFO
		  structure PrettyPrint : PRETTYPRINT
		    sharing type ParseInfo.StringTree = TypeInfo.StringTree
			            = OverloadingInfo.StringTree = PrettyPrint.StringTree 
                                    = SortInfo.StringTree
		  structure Crash : CRASH
		    ) : ELAB_INFO =
