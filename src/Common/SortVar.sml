(* Sort variables *)
(* NOT USED: in RML/Kit3, TYVAR is used instead *)

functor SortVar(structure Crash: CRASH): SORTVAR =
  struct
    datatype SyntaxSortVar = SORTVAR of string
    datatype Variance = COVARIANT | CONTRAVARIANT | IGNORED | MIXED

    val mk_SortVar = SORTVAR
    fun pr_sortvar(SORTVAR str) = str

    local
      fun snd(SORTVAR str) = String.sub(str,1)
        handle Subscript => Crash.impossible "SortVar.snd"
    in
      fun variance sv = 
	case snd sv
	  of #"+" => COVARIANT
	   | #"-" => CONTRAVARIANT
	   | #"?" => IGNORED
	   | _ => MIXED
    end

    fun variancePrefix COVARIANT = "+"
      | variancePrefix CONTRAVARIANT = "-"
      | variancePrefix IGNORED = "?"
      | variancePrefix MIXED = ""

    fun contravariant CONTRAVARIANT = true
      | contravariant IGNORED = true
      | contravariant _ = false

    fun covariant COVARIANT = true
      | covariant IGNORED = true
      | covariant _ = false

    fun join_variance (v, v') =
      case ((covariant v) orelse (covariant v'), 
	    (contravariant v) orelse (contravariant v'))
	of (true, true) => IGNORED
	 | (true, false) => COVARIANT
         | (false, true) => CONTRAVARIANT
         | (false, false) => MIXED


  end;

