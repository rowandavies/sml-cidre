(*$PARSE_ELAB: ERROR_CODE*)

signature PARSE_ELAB =
  sig
    structure ErrorCode : ERROR_CODE

    type Report and InfixBasis and ElabBasis and topdec

    type prjid = string

    datatype Result = SUCCESS of {report: Report, infB: InfixBasis, 
				  elabB: ElabBasis, topdec: topdec}
		    | FAILURE of Report * ErrorCode.ErrorCode list

    val parse_elab : {infB: InfixBasis, elabB: ElabBasis, 
		      prjid: prjid, file: string} -> Result

    val refine_string : string -> unit
    val refine_stdin : unit -> unit
    val refine_file : string -> unit

    val currentInfB : InfixBasis ref
    val currentElabB : ElabBasis ref

    val resetBasis : unit -> unit

  end
