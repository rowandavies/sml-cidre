
(* Stream Library *)
(* Author: Frank Pfenning. *)
(* Additonal sorts added by Rowan Davies *)

exception Subscript;

(* datasort truebool = true
    datasort falsebool = false  *)


signature STREAM =
sig
  type 'a stream
  datatype 'a front = Empty | Cons of 'a * 'a stream

  (*  datasort 'a infStream
           and 'a infFront = Cons of 'a * 'a infStream
  *)

  (* Lazy stream construction and exposure *)
  val delay : (unit -> 'a front) -> 'a stream
  val expose : 'a stream -> 'a front

  (* Eager stream construction *)
  val empty : 'a stream
  val cons : 'a * 'a stream -> 'a stream

  exception EmptyStream
  val null : 'a stream -> bool
  val hd : 'a stream -> 'a
  val tl : 'a stream -> 'a stream

  val map : ('a -> 'b) -> 'a stream -> 'b stream
  val filter : ('a -> bool) -> 'a stream -> 'a stream
  val exists : ('a -> bool) -> 'a stream -> bool

  val take : 'a stream * int -> 'a list
  val drop : 'a stream * int -> 'a stream

  val fromList : 'a list -> 'a stream
  val toList : 'a stream -> 'a list

  val tabulate : (int -> 'a) -> 'a stream
end;

structure Stream (* : STREAM *) =
struct
  datatype 'a stream = Stream of unit -> 'a front
       and 'a front = Empty | Cons of 'a * 'a stream

  (*[ datasort 'a infStream = Stream of unit -> 'a infFront
           and 'a infFront = Cons of 'a * 'a infStream       ]*)

  (*[ val delay :> (unit -> 'a front) -> 'a stream
                 & (unit -> 'a infFront) -> 'a infStream     ]*)
  fun delay d = Stream(d)

  (*[ val expose :> 'a stream -> 'a front
                  & 'a infStream -> 'a infFront             ]*)
  fun expose (Stream(d:unit -> 'a front)) = d ()

  (* val empty : 'a stream *)
  val empty = Stream (fn () => Empty:'a front)

  (*[ val cons :> 'a * 'a stream -> 'a stream
                & 'a * 'a infStream -> 'a infStream ]*)
  fun cons (x, s:'a stream) = Stream (fn () => Cons (x, s))

  exception EmptyStream

  (* functions null, hd, tl, map, filter, exists, take, drop *)
  (* parallel the functions in the List structure *)
  (* val null :> 'a stream -> bool & 'a infStream -> falsebool
      val null' :> 'a front -> bool & 'a infFront -> falsebool  *)
  fun null (s:'a stream) = null' (expose s)
  and null' (Empty:'a front) = true
    | null' (Cons _) = false

  fun hd (s) = hd' (expose s)
  and hd' (Empty) = raise EmptyStream
    | hd' (Cons (x,s)) = x

  fun tl (s) = tl' (expose s)
  and tl' (Empty) = raise EmptyStream
    | tl' (Cons (x,s)) = s

  (*[ val map :> ('a -> 'b) -> 'a stream -> 'b stream
               & ('a -> 'b) -> 'a infStream -> 'b infStream 
      val map' :> ('a -> 'b) -> 'a front -> 'b front
                & ('a -> 'b) -> 'a infFront -> 'b infFront    ]*)
  fun map (f:'a -> 'b)  s = delay (fn () => map' f (expose s))
  and map' (f:'a -> 'b) (Empty) = Empty
    | map' f (Cons(x,s)) = Cons (f(x), map f s)

  fun filter p s = delay (fn () => filter' p (expose s))
  and filter' p (Empty) = Empty
    | filter' p (Cons(x,s)) =
        if p(x) then Cons (x, filter p s)
	else filter' p (expose s)

  (* val exists :> ('a -> bool) -> ('a stream -> bool & 'a infStream -> truebool)              
      val exists' :> ('a -> bool) -> ('a front -> bool & 'a infFront -> truebool)  *)
  fun exists p (s:'a stream) = exists' p (expose s)  (* could add to the sort *)
  and exists' p (Empty:'a front) = false
    | exists' p (Cons(x,s)) =
        p(x) orelse exists p s

  fun takePos (s, 0) = nil
    | takePos (s, n) = take' (expose s, n)
  and take' (Empty, _) = nil
    | take' (Cons(x,s), n) = x::takePos(s, n-1)

  fun take (s,n) = if n < 0 then raise Subscript else takePos (s,n)

  fun fromList (nil) = empty
    | fromList (x::l) = cons(x,fromList(l))

  fun toList (s) = toList' (expose s)
  and toList' (Empty) = nil
    | toList' (Cons(x,s)) = x::toList(s)
  (* withsort toList' : 'a infFront -> empty[list] *)

  (*[ val dropPos :> 'a stream * int -> 'a stream  &  'a infStream * int -> 'a infStream
      val drop' :> 'a front * int -> 'a stream  &  'a infFront * int -> 'a infStream   ]*)
  fun dropPos (s, 0) = s
    | dropPos (s, n) = drop' (expose s, n)
  and drop' (Empty, _) = empty
    | drop' (Cons(x,s), n) = dropPos (s, n-1)

  (*[ val drop :> 'a stream * int -> 'a stream  &  'a infStream * int -> 'a infStream  ]*)
  fun drop (s,n) = if n < 0 then raise Subscript else dropPos (s,n)

  (*[ val tabulate' :> (int -> 'a) -> 'a infFront
      val tabulate :> (int -> 'a) -> 'a infStream   ]*)
  fun tabulate (f) = delay (fn () => tabulate' f)
  and tabulate' (f) = Cons (f(0), tabulate (fn i => f(i+1)))

end;


(* Copyright (c) 1997 by Carnegie Mellon University *)
(* Author: Frank Pfenning <fp@cs.cmu.edu>           *)

(* Stub code for paths, since functors are not yet sort-checked. *)
signature PATHS =
sig
  type region
  val join : region * region -> region
end;  (* signature PATHS *)

structure Paths (* : PATHS *) =
struct
  datatype region = REGION of int * int
  fun join (REGION (l1,r1), REGION (l2,r2)) = REGION (l1,r2)
end;  (* structure Paths *)




signature LEXER =
sig

  (* Stream is not memoizing for efficiency *)
  structure Stream : STREAM
  structure Paths : PATHS

  datatype IdCase =
      Upper				(* [A-Z]<id> or _<id> *)
    | Lower				(* any other <id> *)
    | Quoted				(* '<id>' *)

  datatype Token =
      EOF				(* end of file or stream, also `%.' *)
    | DOT				(* `.' *)
    | COLON				(* `:' *)
    | LPAREN | RPAREN			(* `(' `)' *)
    | LBRACKET | RBRACKET		(* `[' `]' *)
    | LBRACE | RBRACE			(* `{' `}' *)
    | BACKARROW | ARROW			(* `<-' `->' *)
    | TYPE				(* `type' *)
    | EQUAL				(* `=' *)
    | ID of IdCase * string		(* identifer *)
    | UNDERSCORE			(* `_' *)
    | INFIX | PREFIX | POSTFIX		(* `%infix' `%prefix' `%postfix' *)
    | NAME				(* `%name' *)
    | MODE				(* `%mode' *)

  exception Error of string

  (* lexer returns an infinite stream, terminated by EOF token *)
  (*
  val lexStream : TextIO.instream -> (Token * Paths.region) Stream.stream
  val lexTerminal : string * string -> (Token * Paths.region) Stream.stream
  *)

  val toString : Token -> string
end;  (* signature LEXER *)

structure Lexer =
struct

  datatype IdCase =
      Upper				(* [A-Z]<id> or _<id> *)
    | Lower				(* any other <id> *)
    | Quoted				(* '<id>' *)

  datatype Token =
      EOF				(* end of file or stream, also `%.' *)
    | DOT				(* `.' *)
    | COLON				(* `:' *)
    | LPAREN | RPAREN			(* `(' `)' *)
    | LBRACKET | RBRACKET		(* `[' `]' *)
    | LBRACE | RBRACE			(* `{' `}' *)
    | BACKARROW | ARROW			(* `<-' `->' *)
    | TYPE				(* `type' *)
    | EQUAL				(* `=' *)
    | ID of IdCase * string		(* identifer *)
    | UNDERSCORE			(* `_' *)
    | INFIX | PREFIX | POSTFIX		(* `%infix' `%prefix' `%postfix' *)
    | NAME				(* `%name' *)
    | MODE				(* `%mode' *)

  exception Error of string

  (*
  fun error (r, msg) =
      raise Error (Paths.toString r ^ " " ^ "Error: " ^ msg)

  (* isSym (c) = B iff c is a legal symbolic identifier constituent *)
  (* excludes quote character and digits, which are treated specially *)
  (* Char.contains stages its computation *)
  val isSym : char -> bool = Char.contains "_!&$^+/<=>?@~|#*`;,-\\"

  (* isQuote (c) = B iff c is the quote character *)
  fun isQuote (c) = (c = #"'")

  (* isIdChar (c) = B iff c is legal identifier constituent *)
  fun isIdChar (c) = Char.isLower(c) orelse Char.isUpper (c)
                     orelse Char.isDigit (c) orelse isSym(c)
		     orelse isQuote (c)

  (* stringToToken (idCase, string, region) = (token, region)
     converts special identifiers into tokens, returns ID token otherwise
  *)
  fun stringToToken (Lower, "<-", r) = (BACKARROW, r)
    | stringToToken (Lower, "->", r) = (ARROW, r)
    | stringToToken (Upper, "_", r) = (UNDERSCORE, r)
    | stringToToken (Lower, "=", r) = (EQUAL, r)
    | stringToToken (Lower, "type", r) = (TYPE, r)
    | stringToToken (idCase, s, r) = (ID(idCase,s), r)

  (* lex (inputFun) = (token, region) stream
     inputFun maintains state, reading input one line at a time and
     returning a string terminated by <newline> each time.
     The end of the stream is signalled by a string consisting only of ^D
  *)
  fun lex (inputFun:Paths.pos -> string) =
  let
    local (* local state maintained by the lexer *)
      val s = ref ""			(* current string (line) *)
      and left = ref 0			(* position of first character in s *)
      and right = ref 0			(* position after last character in s *)
      val _ = Paths.resetLines ()	(* initialize line counter *)

      (* neither lexer nor parser should ever try to look beyond EOF *)
      val EOFString = String.str #"\^D"

      (* readNext () = ()
         Effect: read the next line, updating s, left, and right

         readNext relies on the invariant that identifiers are never
         spread across lines *)
      fun readNext () =
	  let
	    val nextLine = inputFun (!right)
	    val nextSize = String.size (nextLine)
	  in
	    if nextSize = 0		(* end of file? *)
	      then (s := EOFString;	(* fake EOF character string *)
		    left := !right;
		    right := !right + 1)
	    else (s := nextLine;
		  left := !right;
		  right := !right + nextSize;
		  Paths.newLine (!left)) (* remember new line position *)
	  end
    in
      (* char (i) = character at position i
         Invariant: i >= !left
	 Effects: will read input if i >= !right
      *)
      fun char (i) =
	  (* if i < !left then error("looking into the past") else *)
	  if i >= !right then (readNext (); char (i))
	  else String.sub (!s, i - !left)

      (* string (i,j) = substring at region including i, excluding j
         Invariant: i >= !left and i < j and j < !right
                    Note that the relevant parts must already have been read!
	 Effects: None
      *)
      fun string (i,j) =
	  (* if i < !left then error ("looking into the past") *)
	  (* else if j >= !right then error ("looking into the future") else *)
	  String.substring (!s, i - !left, j-i)
    end

    (* The remaining functions do not access the state or *)
    (* stream directly, using only functions char and string *)

    fun idToToken (idCase, (i,j)) = stringToToken (idCase, string (i,j), (i,j))

    (* Quote characters are part of the name *)
    (* Treat quoted identifiers as lowercase, since they no longer *)
    (* override infix state.  Quoted identifiers are now only used *)
    (* inside pragmas *)
    fun qidToToken (i,j) = (ID(Lower, string(i,j+1)), (i,j+1))

    (* The main lexing functions take a character c and the next
       input position i and return a token with its region
       The name convention is lexSSS, where SSS indicates the state
       of the lexer (e.g. what has been lexed so far).

       Lexing errors are currently fatal---some error recovery code is
       indicated in comments.
    *)
    fun lexInitial (#":", i) = (COLON, (i-1,i))
      | lexInitial (#".", i) = (DOT, (i-1,i))
      | lexInitial (#"(", i) = (LPAREN, (i-1,i))
      | lexInitial (#")", i) = (RPAREN, (i-1,i))
      | lexInitial (#"[", i) = (LBRACKET, (i-1,i))
      | lexInitial (#"]", i) = (RBRACKET, (i-1,i))
      | lexInitial (#"{", i) = (LBRACE, (i-1,i))
      | lexInitial (#"}", i) = (RBRACE, (i-1,i))
      | lexInitial (#"%", i) = lexPercent (char(i), i+1)
      | lexInitial (#"_", i) = lexID (Upper,(i-1,i))
      | lexInitial (#"'", i) = lexQUID (i-1,i)
      | lexInitial (#"\^D", i) = (EOF, (i-1,i-1))
      | lexInitial (c, i) =
	if Char.isSpace (c) then lexInitial (char (i),i+1)
	else if Char.isUpper(c) then lexID (Upper, (i-1,i))
	else if Char.isDigit(c) then lexID (Lower, (i-1,i))
	else if Char.isLower(c) then lexID (Lower, (i-1,i))
	else if isSym(c) then lexID (Lower, (i-1,i))
	else error ((i-1,i), "Illegal character " ^ Char.toString (c))
        (* recover by ignoring: lexInitial (char(i), i+1) *)

    and lexID (idCase, (i,j)) =
        let fun lexID' (j) =
	        if isIdChar (char(j)) then lexID' (j+1)
		else idToToken (idCase, (i,j))
	in
	  lexID' (j)
	end

    and lexQUID (i,j) =
        if Char.isSpace (char(j))
	  then error ((i,j+1), "Whitespace in quoted identifier")
	       (* recover by adding implicit quote? *)
	       (* qidToToken (i, j) *)
	else if isQuote (char(j)) then qidToToken (i,j)
	     else lexQUID (i, j+1)

    and lexPercent (#".", i) = (EOF, (i-2,i))
      | lexPercent (#"{", i) = lexPercentBrace (char(i), i+1)
      | lexPercent (c, i) =
        if isIdChar(c) then lexPragmaKey (lexID (Quoted, (i-1,i)))
	else lexComment (c, i)

    and lexPragmaKey (ID(_, "infix"), r) = (INFIX, r)
      | lexPragmaKey (ID(_, "prefix"), r) = (PREFIX, r)
      | lexPragmaKey (ID(_, "postfix"), r) = (POSTFIX, r)
      | lexPragmaKey (ID(_, "mode"), r) = (MODE, r)
      | lexPragmaKey (ID(_, "name"), r) = (NAME, r)
      | lexPragmaKey (_, (_,j)) = lexComment (char(j), j+1)

    and lexComment (#"\n", i) = lexInitial (char(i), i+1)
      | lexComment (#"%", i) = lexCommentPercent (char(i), i+1)
      | lexComment (#"\^D", i) =
          error ((i-1, i-1), "Unclosed single-line comment at end of file")
	  (* recover: (EOF, (i-1,i-1)) *)
      | lexComment (c, i) = lexComment (char(i), i+1)

    and lexCommentPercent (#".", i) = (EOF, (i-2, i))
      | lexCommentPercent (c, i) = lexComment (c, i)

    and lexPercentBrace (c, i) = lexDComment (c, 1, i)

    (* functions lexing delimited comments below take nesting level l *)
    (* as additional argument *)
    and lexDComment (#"}", l, i) = lexDCommentRBrace (char(i), l, i+1)
      | lexDComment (#"%", l, i) = lexDCommentPercent (char(i), l, i+1)
      | lexDComment (#"\^D", l, i) =
          (* pass comment beginning for error message? *)
          error ((i-1,i-1), "Unclosed delimited comment at end of file")
	  (* recover: (EOF, (i-1,i-1)) *)
      | lexDComment (c, l, i) = lexDComment (char(i), l, i+1)

    and lexDCommentPercent (#"{", l, i) = lexDComment (char(i), l+1, i+1)
      | lexDCommentPercent (#".", l, i) =
          error ((i-2, i), "Unclosed delimited comment at end of file token `%.'")
          (* recover: (EOF, (i-2,i)) *)
      | lexDCommentPercent (c, l, i) = lexDComment (c, l, i)

    and lexDCommentRBrace (#"%", 1, i) = lexInitial (char(i), i+1)
      | lexDCommentRBrace (#"%", l, i) = lexDComment (char(i), l-1, i+1)
      | lexDCommentRBrace (c, l, i) = lexDComment (c, l, i)

    fun lexContinue (j) = Stream.delay (fn () => lexContinue' (j))
    and lexContinue' (j) = lexContinue'' (lexInitial (char(j), j+1))

    and lexContinue'' (mt as (token, (i,j))) =
          Stream.Cons (mt, lexContinue (j))
  in
    lexContinue (0)
  end  (* fun lex (inputFun) = let ... in ... end *)
  *)

  (*
  fun lexStream (instream) = lex (fn i => TextIO.inputLine (instream))

  fun lexTerminal (prompt0, prompt1) =
        lex (fn 0 => (TextIO.print (prompt0) ;
		      TextIO.inputLine (TextIO.stdIn))
	      | i => (TextIO.print (prompt1) ;
		      TextIO.inputLine (TextIO.stdIn)))
  *)

  (* lexString does not count lines properly *)
  (*
  fun lexString (string) =
        lex (fn 0 => string ^ "\n"
	      | i => "")
  *)

  fun toString' (DOT) = "."
    | toString' (COLON) = ":"
    | toString' (LPAREN) = "("
    | toString' (RPAREN) = ")"
    | toString' (LBRACKET) = "["
    | toString' (RBRACKET) = "]"
    | toString' (LBRACE) = "{"
    | toString' (RBRACE) = "}"
    | toString' (BACKARROW) = "<-"
    | toString' (ARROW) = "->"
    | toString' (TYPE) = "type"
    | toString' (EQUAL) = "="
    | toString' (UNDERSCORE) = "_"
    | toString' (INFIX) = "%infix"
    | toString' (PREFIX) = "%prefix"
    | toString' (POSTFIX) = "%postfix"
    | toString' (NAME) = "%name"
    | toString' (MODE) = "%mode"
    | toString' _ = raise Match
    (* | toString' (EOF) = "end of file" *)
    (* | toString' (ID(_,s)) = s *)

 fun toString (ID(_,s)) = "identifier `" ^ s ^ "'"
   | toString (EOF) = "end of file or `%.'"
   | toString (token) = "`" ^ toString' token ^ "'"

end;  (* functor Lexer *)





(* parse-lib.sml *)

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

structure Fixity (* : FIXITY *) =
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

structure ExtSyn (* : EXTSYN *) =
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

    (*[  datasort 'a Atom = Atom of 'a
              and 'a Infix = Infix of (int * FX.associativity) * ('a * 'a -> 'a)
              and 'a Prefix = Prefix of int * ('a -> 'a)
              and 'a Postfix = Postfix of int * ('a -> 'a);
         datasort 'a shiftable = Infix of (int * FX.associativity) * ('a * 'a -> 'a)
                               | Prefix of int * ('a -> 'a)
	                       | Atom of 'a                  ]*)

    (* Stacks of unresolved operators *)
    datatype 'a stack = snil | ::: of 'a operator * 'a stack;
    infixr 5 :::

    (* Various refinements of stacks to enforce invariants *)
    (*[ datasort 'a pSnil = snil
	     and 'a pOp = ::: of 'a Infix * 'a pAtom
			| ::: of 'a Prefix * 'a pSnil
			| ::: of 'a Prefix * 'a pOp
	     and 'a pAtom = ::: of 'a Atom * 'a pSnil
			  | ::: of 'a Atom * 'a pOp
	datasort 'a pStable =  (* 'a pOp | 'a pAtom | 'a pSnil *)
			snil
		      | ::: of 'a Atom * 'a pSnil
		      | ::: of 'a Atom * 'a pOp
		      | ::: of 'a Infix * 'a pAtom
		      | ::: of 'a Prefix * 'a pSnil
		      | ::: of 'a Prefix * 'a pOp
 	datasort 'a pComplete = ::: of 'a Atom * 'a pSnil
			      | ::: of 'a Atom * 'a pOp
			      | ::: of 'a Postfix * 'a pAtom
(*	datasort 'a pRedex = ::: of 'a Postfix * 'a pAtom
			   | ::: of 'a Atom * 'a pOp
	datasort 'a p = snil (* 'a pStable | 'a pRedex *)
		      | ::: of 'a Atom * 'a pSnil
		      | ::: of 'a Atom * 'a pOp
		      | ::: of 'a Infix * 'a pAtom
		      | ::: of 'a Postfix * 'a pAtom
		      | ::: of 'a Prefix * 'a pSnil
		      | ::: of 'a Prefix * 'a pOp   *) ]*)
