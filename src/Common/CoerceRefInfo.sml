(* Coerce RefInfo so that functor ErrorTraverse can be applied to it.
   
   - Rowan 6jul01  *)

functor CoerceRefInfo(RefInfo : REF_INFO) 
   : E_OR_R_INFO where type ElabInfo = RefInfo.RefInfo
                   and type ErrorInfo.ErrorInfo = RefInfo.RefineErrorInfo.ErrorInfo
                   and type ErrorInfo.Report = RefInfo.RefineErrorInfo.Report
                   and type ErrorInfo.ErrorCode.ErrorCode = 
                            RefInfo.RefineErrorInfo.ErrorCode.ErrorCode
                   and type StringTree = RefInfo.StringTree  
= struct     
    type ElabInfo = RefInfo.RefInfo
    structure ErrorInfo = RefInfo.RefineErrorInfo
    type ErrorInfo = RefInfo.RefineErrorInfo.ErrorInfo
    type StringTree = RefInfo.StringTree
    val report_SourceInfo = RefInfo.report_SourceInfo
    val to_ErrorInfo = RefInfo.to_RefineErrorInfo
  end
 