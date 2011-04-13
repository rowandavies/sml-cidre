(* A minimal signature matched by both ElabInfo and (coerced) RefInfo for use 
   by ErrorTraverse. *)

signature E_OR_R_INFO = 
  sig
    (*type supplied by this module:*) 
    type ElabInfo

    structure ErrorInfo : sig
                            type ErrorInfo
                            type Report
                            val report : ErrorInfo -> Report
                            structure ErrorCode : ERROR_CODE
                               sharing type ErrorCode.ErrorInfo = ErrorInfo
                          end

    type ErrorInfo = ErrorInfo.ErrorInfo
    type StringTree
    val report_SourceInfo :      ElabInfo -> ErrorInfo.Report
    val to_ErrorInfo :           ElabInfo -> ErrorInfo option
  end;
