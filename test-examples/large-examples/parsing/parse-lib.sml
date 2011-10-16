
(* First, some stub code to allow sort checking, even though 
   functors aren't supported *)
fun intToString (n:int) = "";

datatype 'a option = NONE | SOME of 'a;

datatype order = LESS | EQUAL | GREATER;

structure Int = 
struct
  fun compare (s1:int,s2:int) =
      if s1 = s2 then EQUAL
      else if s1 < s2 then LESS
	   else GREATER;
end;

signature FIXITY =
sig

  datatype associativity = Left | Right | None

  (* type precedence = int *)
  val maxPrec : int
  val minPrec : int

  datatype fixity =
      Nonfix
    | Infix of int * associativity
    | Prefix of int
    | Postfix of int

  val prec : fixity -> int
  val toString : fixity -> string

end;  (* signature FIXITY *)

structure Fixity : FIXITY =
struct

  datatype associativity = Left | Right | None

  type precedence = int

  val maxPrec = 9999
  val minPrec = 0

  datatype fixity =
      Nonfix
    | Infix of precedence * associativity
    | Prefix of precedence
    | Postfix of precedence

  fun prec (Infix(p,_)) = p
    | prec (Prefix(p)) = p
    | prec (Postfix(p)) = p
    | prec (Nonfix) = maxPrec+1

  fun toString (Infix(p,Left)) = "%infix left " ^ intToString p
    | toString (Infix(p,Right)) = "%infix right " ^ intToString p
    | toString (Infix(p,None)) = "%infix none " ^ intToString p
    | toString (Prefix(p)) = "%prefix " ^ intToString p
    | toString (Postfix(p)) = "%postfix " ^ intToString p
    | toString (Nonfix) = "%nonfix"

end;  (* structure Fixity *)

structure FX = Fixity;
(* from Names *)
fun fixityLookup (s:string) = FX.Nonfix;

(* Stub code for external syntax, since functors are not yet sort-checked. *)
signature EXTSYN =
sig
  type term
  type decl
  val lcid : string * Paths.region -> term
  val ucid : string * Paths.region -> term
  val quid : string * Paths.region -> term

  val app : term * term -> term
  val arrow : term * term -> term
  val backarrow : term * term -> term
  val hastype : term * term -> term
  val omitobj : Paths.region -> term
  val pi : decl * term * Paths.region -> term
  val lam : decl * term * Paths.region -> term
  val typ : Paths.region -> term
  val decl : string option * term -> decl
  val decl0 : string option -> decl
end;  (* signature EXTSYN *)

structure ExtSyn : EXTSYN =
struct
  type term = unit;
  type decl = unit;
  fun lcid (s:string, r:Paths.region) = ();
  fun ucid (s:string, r:Paths.region) = ();
  fun quid (s:string, r:Paths.region) = ();
  fun app (t1:term, t2:term) = ();
  fun arrow (t1:term, t2:term) = ();
  fun backarrow (t1:term, t2:term) = ();
  fun hastype (t1:term,t2:term) = ();
  fun omitobj (r:Paths.region) = ();
  fun pi (d:decl, t:term, r:Paths.region) = ();
  fun lam (d:decl, t:term, r:Paths.region) = ();
  fun typ (r:Paths.region) = ();
  fun decl (x:string option, t:term) = ();
  fun decl0 (x:string option) = ()
end;  (* structure ExtSyn *)

exception Error of string;
fun error (r:Paths.region, msg:string) = raise Error(msg);

    (* Operators and atoms for fixity parsing *)
    datatype 'a operator =
        Atom of 'a
      | Infix of (int * FX.associativity) * ('a * 'a -> 'a)
      | Prefix of int * ('a -> 'a)
      | Postfix of int * ('a -> 'a);
(*[
    datasort 'a Atom = Atom of 'a
         and 'a Infix = Infix of (int * FX.associativity) * ('a * 'a -> 'a)
         and 'a Prefix = Prefix of int * ('a -> 'a)
         and 'a Postfix = Postfix of int * ('a -> 'a);
    datasort 'a shiftable = Infix of (int * FX.associativity) * ('a * 'a -> 'a)
                          | Prefix of int * ('a -> 'a)
	                  | Atom of 'a
]*)
    (* Stacks of unresolved operators *)
    datatype 'a stack = nil | :: of 'a operator * 'a stack;

    (* Various refinements of stacks to enforce invariants *)
(*[
    datasort 'a pNil = nil
         and 'a pOp = :: of 'a Infix * 'a pAtom
	            | :: of 'a Prefix * 'a pNil
	            | :: of 'a Prefix * 'a pOp
         and 'a pAtom = :: of 'a Atom * 'a pNil
	              | :: of 'a Atom * 'a pOp;

    datasort 'a pStable = (* 'a pOp | 'a pAtom | 'a pNil *)
                    nil
		  | :: of 'a Atom * 'a pNil
                  | :: of 'a Atom * 'a pOp
	          | :: of 'a Infix * 'a pAtom
	          | :: of 'a Prefix * 'a pNil
	          | :: of 'a Prefix * 'a pOp;

    datasort 'a pComplete = :: of 'a Atom * 'a pNil 
                          | :: of 'a Atom * 'a pOp
                          | :: of 'a Postfix * 'a pAtom

    datasort 'a pRedex = :: of 'a Postfix * 'a pAtom
                       | :: of 'a Atom * 'a pOp;

    datasort 'a p = nil (* 'a pStable | 'a pRedex *)
		  | :: of 'a Atom * 'a pNil
                  | :: of 'a Atom * 'a pOp
	          | :: of 'a Infix * 'a pAtom
                  | :: of 'a Postfix * 'a pAtom
	          | :: of 'a Prefix * 'a pNil
	          | :: of 'a Prefix * 'a pOp;
]*)
    (* Predeclared infix operators *)
    val juxOp = Infix ((FX.maxPrec+1, FX.Left), ExtSyn.app); (* juxtaposition *)
    val arrowOp = Infix ((FX.minPrec-1, FX.Right), ExtSyn.arrow);
    val backArrowOp = Infix ((FX.minPrec-1, FX.Left), ExtSyn.backarrow);
    val colonOp = Infix ((FX.minPrec-2, FX.Left), ExtSyn.hastype);

    fun infixOp (infixity, tm) =
          Infix (infixity, (fn (tm1, tm2) => ExtSyn.app (ExtSyn.app (tm, tm1), tm2)));
    fun prefixOp (prec, tm) =
          Prefix (prec, (fn tm1 => ExtSyn.app (tm, tm1)));
    fun postfixOp (prec, tm) =
          Postfix (prec, (fn tm1 => ExtSyn.app (tm, tm1)));

    fun idToTerm (Lexer.Lower, name, r) = ExtSyn.lcid (name, r)
      | idToTerm (Lexer.Upper, name, r) = ExtSyn.ucid (name, r)
      | idToTerm (Lexer.Quoted, name, r) = ExtSyn.quid (name, r)

    fun isQuoted (Lexer.Quoted) = true
      | isQuoted _ = false

    (*
    type stack = (ExtSyn.term operator) list;
    *)
    type opr = ExtSyn.term operator;

    (* The next section deals generically with fixity parsing          *)
    (* Because of juxtaposition, it is not clear how to turn this      *)
    (* into a separate module without passing a juxtaposition operator *)
    (* into the shift and resolve functions                            *)

    (*
    structure P :
      sig
	val reduce : stack -> stack
        val reduceAll : Paths.region * stack -> ExtSyn.term
        val shiftAtom : ExtSyn.term * stack -> stack
        val shift : Paths.region * opr * stack -> stack
        val resolve : Paths.region * opr * stack -> stack
      end =
    struct
    *)
      (* Stack invariants, refinements of operator list *)
      (*
	 <p>       ::= <pStable> | <pRed>
	 <pStable> ::= <pAtom> | <pOp?>
	 <pAtom>   ::= Atom _ :: <pOp?>
	 <pOp?>    ::= nil | <pOp>
	 <pOp>     ::= Infix _ :: <pAtom> :: <pOp?>
		     | Prefix _ :: <pOp?>
	 <pRed>    ::= Postfix _ :: Atom _ :: <pOp?>
		     | Atom _ :: <pOp>
      *)
      (* val reduce : <pRed> -> <p> *)
      (*[ val reduce : 'a pRedex -> 'a pAtom ]*);
      fun reduce (Atom(tm2)::Infix(_,con)::Atom(tm1)::p') =
	     Atom(con(tm1,tm2))::(p':'a stack)
	| reduce (Atom(tm)::Prefix(_,con)::p') = Atom(con(tm))::p'
	| reduce (Postfix(_,con)::Atom(tm)::p') = Atom(con(tm))::p'
	(* no other cases should be possible by stack invariant *)

      (*[ val reduceRec : 'a pComplete -> 'a ]*);
      (* val reduceRec : <pStable> -> ExtSyn.term *)
      fun reduceRec (Atom(e)::nil) = e
	| reduceRec (p:'a stack) = reduceRec (reduce p)

      (* val reduceAll : <p> -> ExtSyn.term *)
      (*[ val reduceAll : Paths.region * 'a p -> 'a ]*);;
      fun reduceAll (r, Atom(e)::nil) = e
        | reduceAll (r, Infix _::p') = error (r, "Incomplete infix expression")
	| reduceAll (r, Prefix _::p') = error (r, "Incomplete prefix expression")
	| reduceAll (r, nil) = error (r, "Empty expression")
	| reduceAll (r, p:'a stack) = reduceRec (reduce p)
      (* raises error for all incomplete expressions *)

      (* val shiftAtom : term * <pStable> -> <p> *)
      (* does not raise Error exception *)
      (*[ val shiftAtom : ExtSyn.term * ExtSyn.term pStable -> ExtSyn.term pStable ]*);;
      fun shiftAtom (tm, p as (Atom _::p')) =
	  (* insert juxOp operator and reduce *)
	  (* juxtaposition binds most strongly *)
	    reduce (Atom(tm)::juxOp::p)
	| shiftAtom (tm, p) = Atom(tm)::p

      (* val shift : Paths.region * opr * <pStable> -> <p> *)
      (*[ val shift : Paths.region * ExtSyn.term shiftable * ExtSyn.term pStable -> ExtSyn.term pStable
                      & Paths.region * ExtSyn.term Postfix * ExtSyn.term pAtom -> ExtSyn.term pRedex
                      & Paths.region * ExtSyn.term Postfix * ExtSyn.term pNil -> ExtSyn.term pStable (* >: empty[stack] *)
                      & Paths.region * ExtSyn.term Postfix * ExtSyn.term pOp -> ExtSyn.term pStable;  ]*);
           (* >: empty[stack] *)
      fun shift (r, opr as Atom _, p as (Atom _::p')) =
	    (* insert juxOp operator and reduce *)
	    (* juxtaposition binds most strongly *)
	    reduce (opr::juxOp::p)
	(* Atom/Infix: shift *)
	(* Atom/Prefix: shift *)
	(* Atom/Postfix cannot arise *)
	(* Atom/Empty: shift *)
	(* Infix/Atom: shift *)
	| shift (r, Infix _, Infix _::p') =
	    error (r, "Consective infix operators")
	| shift (r, Infix _, Prefix _::p') =
	    error (r, "Infix operator following prefix operator")
	(* Infix/Postfix cannot arise *)
	| shift (r, Infix _, nil) =
	    error (r, "Leading infix operator")
	| shift (r, opr as Prefix _, p as (Atom _::p')) =
	   (* insert juxtaposition operator *)
	   (* will be reduced later *)
	   opr::juxOp::p
	(* Prefix/{Infix,Prefix,Empty}: shift *)
	(* Prefix/Postfix cannot arise *)
	(* Postfix/Atom: shift, reduced immediately *)
	| shift (r, Postfix _, Infix _::p') =
	    error (r, "Postfix operator following infix operator")
	| shift (r, Postfix _, Prefix _::p') =
	    error (r, "Postfix operator following prefix operator")
	(* Postfix/Postfix cannot arise *)
	| shift (r, Postfix _, nil) =
	    error (r, "Leading postfix operator")
	| shift (r, opr, p) = opr::p


      (* val resolve : Paths.region * opr * <pStable> -> <p> *)
      (* Decides, based on precedence of opr compared to the top of the
         stack whether to shift the new operator or reduce the stack
      *)
      (*[ val resolve : Paths.region * ExtSyn.term operator * ExtSyn.term pStable -> ExtSyn.term pStable  ]*);;
      fun resolve (r, opr as Infix((prec, assoc), _),
		     p as (Atom(_)::Infix((prec', assoc'), _)::p')) =
	  (case (Int.compare(prec,prec'), assoc, assoc')
	     of (GREATER,_,_) => shift(r, opr, p)
	      | (LESS,_,_) => resolve (r, opr, reduce(p))
	      | (EQUAL, FX.Left, FX.Left) => resolve (r, opr, reduce(p))
	      | (EQUAL, FX.Right, FX.Right) => shift(r, opr, p)
	      | _ => error (r, "Ambiguous: infix following infix of identical precedence"))
	| resolve (r, opr as Infix ((prec, assoc), _),
		     p as (Atom(_)::Prefix(prec', _)::p')) =
	  (case Int.compare(prec,prec')
	     of GREATER => shift(r, opr, p)
	      | LESS => resolve (r, opr, reduce(p))
	      | EQUAL => error (r, "Ambiguous: infix following prefix of identical precedence"))
	(* infix/atom/atom cannot arise *)
	(* infix/atom/postfix cannot arise *)
	(* infix/atom/<empty>: shift *)

	(* always shift prefix *)
	| resolve (r, opr as Prefix _, p) =
	    shift(r, opr, p)

	(* always reduce postfix, possibly after prior reduction *)
	| resolve (r, opr as Postfix(prec, _),
		     p as (Atom _::Prefix(prec', _)::p')) =
	    (case Int.compare(prec,prec')
	       of GREATER => reduce (shift (r, opr, p))
		| LESS => resolve (r, opr, reduce (p))
		| EQUAL => error (r, "Ambiguous: postfix following prefix of identical precedence"))
	(* always reduce postfix *)
	| resolve (r, opr as Postfix(prec, _),
		     p as (Atom _::Infix((prec', _), _)::p')) =
	    (case Int.compare(prec,prec')
	       of GREATER => reduce (shift (r, opr, p))
		| LESS => resolve (r, opr, reduce (p))
		| EQUAL => error (r, "Ambiguous: postfix following infix of identical precedence"))
	| resolve (r, opr as Postfix _, p as (Atom _::nil)) =
	    reduce (shift (r, opr, p))

	(* default is shift *)
	| resolve (r, opr, p) = (* nil *)
	    shift(r, opr, p)
