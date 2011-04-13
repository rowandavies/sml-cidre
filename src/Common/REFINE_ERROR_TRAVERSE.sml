(*$REFINE_ERROR_TRAVERSE*)
signature REFINE_ERROR_TRAVERSE =
  sig
    type topdec
    type Report

    datatype result = SUCCESS
		    | FAILURE of Report

    val traverse: topdec -> result
  end;
