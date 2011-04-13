(* Type variables - Definition v3 page ?? *)

(*$TyVar: CRASH TYVAR*)
functor TyVar(structure Crash: CRASH): TYVAR =
  struct
    datatype SyntaxTyVar = TYVAR of string

    val mk_TyVar = TYVAR
    fun pr_tyvar(TYVAR str) = str

    fun isEquality (TYVAR s) = (case explode s of
				  #"'" :: #"'" :: ss => true
				| _ => false)

    (* For refinements *)
    datatype Variance = COVARIANT | CONTRAVARIANT | IGNORED | MIXED

    local
      fun snd(TYVAR str) = String.sub(str,1)
        handle Subscript => Crash.impossible "TypeVar.snd"
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
