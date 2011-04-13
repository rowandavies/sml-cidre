

(*General.sml*)

infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

    type unit = unit
    type exn = exn
    type 'a ref = 'a ref

(*
    exception Bind = Bind
    exception Match = Match
    exception Subscript
    exception Size
    exception Overflow = Overflow
    exception Domain
    exception Div = Div
    exception Chr
    exception Fail of string 
*)
(*  
    fun exnName (e: exn) : string = prim("exnName", "exnNameProfiling", e)   (* exomorphic by copying *)
    fun exnMessage (e: exn) : string = exnName e 
*)
    datatype 'a option = NONE | SOME of 'a
(*    exception Option
    fun getOpt (NONE, a) = a
      | getOpt (SOME a, b) = a
    fun isSome NONE = false
      | isSome _ = true
    fun valOf (SOME a) = a
(*       | valOf _ = raise Option *)
*)
    datatype order = LESS | EQUAL | GREATER

    fun !(x: 'a ref): 'a = ! x
    fun (x: 'a ref) := (y: 'a): unit = (x := y)
    fun (f o g) x = f(g x)
    fun a before () = a
    fun ignore (a) = ()


(* Top-level identifiers; Some are here - some are introduced later *)

fun op = (x: ''a, y: ''a): bool =  (x = y)

fun not true = false
  | not false = true

fun a <> b = not (a = b)

fun print (s:string) : unit = ()

fun implode (chars : char list) : string = ""
fun concat (ss : string list) : string = ""
fun (s : string) ^ (s' : string) : string = ""
fun str (c : char) : string = ""
fun size (s:string): int = 0
fun explode (s:string) : char list = explode s 

(* Stream Library *)
(* Author: Frank Pfenning. *)
(* Additonal sorts added by Rowan Davies *)

exception Subscript;

(*[
datasort truebool = true
datasort falsebool = false;
]*)
signature STREAM =
sig
  type 'a stream
  datatype 'a front = Empty | Cons of 'a * 'a stream

  (*
  datasort 'a infStream
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

(*[  datasort 'a infStream = Stream of unit -> 'a infFront
          and 'a infFront = Cons of 'a * 'a infStream
]*)

  (*[ val delay :> (unit -> 'a front) -> 'a stream
                 & (unit -> 'a infFront) -> 'a infStream ]*)
  fun delay (d:unit -> 'a front) = Stream(d)

  (*[ val expose :> 'a stream -> 'a front
                  & 'a infStream -> 'a infFront ]*)
  fun expose (Stream(d:unit -> 'a front)) = d ()

  val empty = Stream (fn () => Empty:'a front)
  (* val empty : 'a stream *)

  (*[ val cons :> 'a * 'a stream -> 'a stream
                & 'a * 'a infStream -> 'a infStream ]*)
  fun cons (x, s:'a stream) = Stream (fn () => Cons (x, s))

  exception EmptyStream

  (* functions null, hd, tl, map, filter, exists, take, drop *)
  (* parallel the functions in the List structure *)
  (*[ val null :> 'a stream -> bool & 'a infStream -> falsebool
      val null' :> 'a front -> bool & 'a infFront -> falsebool ]*)
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
                & ('a -> 'b) -> 'a infFront -> 'b infFront  ]*)
  fun map (f:'a -> 'b)  s = delay (fn () => map' f (expose s))
  and map' (f:'a -> 'b) (Empty) = Empty
    | map' f (Cons(x,s)) = Cons (f(x), map f s)

  fun filter p s = delay (fn () => filter' p (expose s))
  and filter' p (Empty) = Empty
    | filter' p (Cons(x,s)) =
        if p(x) then Cons (x, filter p s)
	else filter' p (expose s)

  (*[ val exists :> ('a -> bool) -> ('a stream -> bool & 'a infStream -> truebool)              
      val exists' :> ('a -> bool) -> ('a front -> bool & 'a infFront -> truebool)  ]*)
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

  (* (*[ val toList' : 'a infFront -> empty[list] ]*) *)
  fun toList (s) = toList' (expose s)
  and toList' (Empty) = nil
    | toList' (Cons(x,s)) = x::toList(s)

  (*[ val dropPos :> 'a stream * int -> 'a stream & 'a infStream * int -> 'a infStream
      val drop' :> 'a front * int -> 'a stream & 'a infFront * int -> 'a infStream  ]*)
  fun dropPos (s, 0) = s
    | dropPos (s, n) = drop' (expose s, n)
  and drop' (Empty:'a front, _) = empty
    | drop' (Cons(x,s), n) = dropPos (s, n-1)

  (*[ val drop :> 'a stream * int -> 'a stream & 'a infStream * int -> 'a infStream  ]*)
  fun drop (s:'a stream,n) = if n < 0 then raise Subscript else dropPos (s,n)

  (*[ val tabulate' :> (int -> 'a) -> 'a infFront
      val tabulate :> (int -> 'a) -> 'a infStream  ]*)
  fun tabulate (f:int -> 'a) = delay (fn () => tabulate' f)
  and tabulate' (f:int -> 'a) = Cons (f(0), tabulate (fn i => f(i+1)))

end;


(* Copyright (c) 1997 by Carnegie Mellon University *)
(* Author: Frank Pfenning <fp@cs.cmu.edu>           *)

(* Stub code for paths, since functors are not yet sort-checked. *)
signature PATHS =
sig
  type region
  val join : region * region -> region
end;  (* signature PATHS *)

structure Paths : PATHS =
struct
  type region = int * int
  fun join ((l1,r1):region,(l2,r2):region) = (l1,r2)
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

(*[ datasort NotEofID = 
      DOT				(* `.' *)
    | COLON				(* `:' *)
    | LPAREN | RPAREN			(* `(' `)' *)
    | LBRACKET | RBRACKET		(* `[' `]' *)
    | LBRACE | RBRACE			(* `{' `}' *)
    | BACKARROW | ARROW			(* `<-' `->' *)
    | TYPE				(* `type' *)
    | EQUAL				(* `=' *)
    | UNDERSCORE			(* `_' *)
    | INFIX | PREFIX | POSTFIX		(* `%infix' `%prefix' `%postfix' *)
    | NAME				(* `%name' *)
    | MODE				(* `%mode' *)
]*)

  exception Error of string;

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

(*[ val toString' :> NotEofID -> string ]*)
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
    (* | toString' (EOF) = "end of file" *)
    (* | toString' (ID(_,s)) = s *)
    | toString' _ = raise Match

 fun toString (ID(_,s)) = "identifier `" ^ s ^ "'"
   | toString (EOF) = "end of file or `%.'"
   | toString (token) = "`" ^ toString' token ^ "'"

end;  (* functor Lexer *)
