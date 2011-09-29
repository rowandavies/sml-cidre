functor TopdecLex(structure Tokens: Topdec_TOKENS
			   structure LexBasics: LEX_BASICS
			   structure LexUtils: LEX_UTILS
			     sharing type LexUtils.svalue = Tokens.svalue
			     sharing type LexUtils.token = Tokens.token
			     sharing type LexUtils.pos = LexBasics.pos
			     sharing type LexUtils.SourceReader
					  = LexBasics.SourceReader
			  )
	  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
C | S | INITIAL
    structure UserDeclarations = 
      struct

(* Lexical specification for Standard ML.		NICK, August 1990. *)

  open Tokens
  type pos = LexBasics.pos
  type arg = LexUtils.LexArgument
  type lexresult = (svalue, LexBasics.pos) token
  fun eof _ = Tokens.EOF(LexBasics.DUMMY, LexBasics.DUMMY)
  val lParen = "(" and rParen = ")"

(* Something which returns a (pos * pos) for a token. This is what ML-Yacc
   works with, and we use it in the productions below. *)

  fun ofLength(arg, yypos, yytext) =
    let
      val yypos = yypos - 2
	(* If somebody can tell be why the yypos seems to be permanently
	   two characters ahead of where it should be, I'd be interested... *)

      val LexBasics.SOURCE_READER{positionFn, ...} =
	LexUtils.sourceReaderOf arg

      val lPos = positionFn yypos
      val rPos = positionFn(yypos + size yytext)
    in
      (lPos, rPos)
    end

  fun token0(tokFn, arg, yypos, yytext) =
        tokFn(ofLength(arg, yypos, yytext))

  and token1(tokFn, value, arg, yypos, yytext) =
	let
	  val (l, r) = ofLength(arg, yypos, yytext)
	in
	  tokFn(value, l, r)
	end

  fun positionOfStream(arg, yypos) =
    let
      val LexBasics.SOURCE_READER{positionFn, ...} =
	LexUtils.sourceReaderOf arg
    in
      positionFn yypos
    end

  fun error(arg, yypos, msg) =
    raise LexBasics.LEXICAL_ERROR(positionOfStream(arg, yypos), msg)

 (* addAsciiChar can fail, so we need to generate position info for it. *)
  fun addAsciiChar(arg, yypos, yytext) =
    LexUtils.addAsciiChar (positionOfStream(arg, yypos), yytext) arg

 (*addUnicodeChar can fail, so we need to generate position info for it.*)
  fun addUnicodeChar (arg, yypos, yytext) =
        LexUtils.addUnicodeChar (positionOfStream (arg, yypos), yytext) arg


      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as (arg: UserDeclarations.arg)) () = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction1 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (token1(LexUtils.identifier, yytext,
				   arg, yypos, yytext
				  )
			   )
      end
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (token1(LexUtils.identifier, yytext,
				   arg, yypos, yytext
				  )
			   )
      end
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (token1(if LexUtils.isQualStar yytext
				   then (LexBasics.shifting "QUAL_STAR";
					 QUAL_STAR
					)
				   else (LexBasics.shifting "QUAL_ID";
					 QUAL_ID
					),
				   LexUtils.asQualId yytext,
				   arg, yypos, yytext
				  )
			   )
      end
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (token0(DOTDOTDOT, arg, yypos, yytext))
      end
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (token0(LPAREN, arg, yypos, yytext))
      end
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (token0(RPAREN, arg, yypos, yytext))
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (token0(LBRACKET, arg, yypos, yytext))
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (token0(RBRACKET, arg, yypos, yytext))
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (token0(LBRACE, arg, yypos, yytext))
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (token0(RBRACE, arg, yypos, yytext))
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (token0(COMMA, arg, yypos, yytext))
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (token0(SEMICOLON, arg, yypos, yytext))
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (token0(UNDERBAR, arg, yypos, yytext))
      end
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (LexBasics.shifting "REAL(...)";
			    token1(REAL, LexUtils.asReal yytext,
				   arg, yypos, yytext))
      end
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (LexBasics.shifting "DIGIT(...)";
			    token1(DIGIT, LexUtils.asDigit yytext,
				   arg, yypos, yytext))
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (LexBasics.shifting "DECPOSINTEGER(...)";
			    token1(DECPOSINTEGER, LexUtils.asInteger yytext,
				   arg, yypos, yytext))
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (LexBasics.shifting "DECNEGINTEGER(...)";
			    token1(DECNEGINTEGER, LexUtils.asInteger yytext,
				   arg, yypos, yytext))
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (LexBasics.shifting "HEXINTEGER(...)";
			    token1(HEXINTEGER, LexUtils.asInteger yytext,
				   arg, yypos, yytext))
      end
fun yyAction19 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (LexBasics.shifting "WORD(...)";
			    token1(WORD, LexUtils.asWord yytext,
				   arg, yypos, yytext))
      end
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (LexBasics.shifting "TYVAR(...)";
			    token1(TYVAR, yytext, arg, yypos, yytext))
      end
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN S; lex (LexUtils.clearString arg) ()))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN C; lex (LexUtils.newComment arg) ()))
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (token0(LCOMSPEC, arg, yypos, yytext))
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (token0(RCOMSPEC, arg, yypos, yytext))
      end
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (lex (LexUtils.addChars yytext arg) ())
      end
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN INITIAL;
			    LexBasics.shifting "STRING(...)";
			    token1(STRING, LexUtils.asString arg,
				   arg, yypos, yytext
				  )
			   )
      end
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error(arg, yypos, "unclosed string");
			    YYBEGIN INITIAL;
			    LexBasics.shifting "STRING(bad)";
			    token1(STRING, "", arg, yypos, yytext)
			   )
      end
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lex (LexUtils.addChars (str(chr 7)) arg) ()))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lex (LexUtils.addChars (str(chr 8)) arg) ()))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lex (LexUtils.addChars "\t" arg) ()))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lex (LexUtils.addChars "\n" arg) ()))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lex (LexUtils.addChars (str(chr 11)) arg) ()))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lex (LexUtils.addChars (str(chr 12)) arg) ()))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lex (LexUtils.addChars (str(chr 13)) arg) ()))
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (lex (LexUtils.addControlChar yytext arg) ())
      end
fun yyAction37 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (lex (addAsciiChar (arg, yypos, yytext)) ())
      end
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (lex (addUnicodeChar (arg, yypos, yytext)) ())
      end
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lex (LexUtils.addChars "\"" arg) ()))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lex (LexUtils.addChars "\\" arg) ()))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (error(arg, yypos, "illegal string escape");
			    continue()
			   ))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lex (LexUtils.incComment arg) ()))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (case LexUtils.decComment arg
			      of (0, arg') => (YYBEGIN INITIAL; lex arg' ())
			       | (_, arg') => lex arg' ()
			    ))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction46 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error(arg, yypos, "cannot lex \"" ^ yytext ^ "\"");
			    continue()
			   )
      end
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ61(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp < #"0"
              then yyAction14(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ61(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ61(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ61(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ61(strm', lastMatch)
            else if inp = #"~"
              then yyQ62(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction14(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp <= #"/"
                  then yyAction14(strm, yyNO_MATCH)
                  else yyQ63(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp = #"E"
              then yyQ60(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ63(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ63(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ58(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ59(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = #"E"
              then yyQ60(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < #"E"
              then if inp <= #"9"
                  then yyQ58(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ65(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ65(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction18(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ65(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ65(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ65(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ65(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ65(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ65(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ65(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ65(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ65(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ65(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction17(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ59(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                  else yyQ58(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = #"F"
              then yyAction17(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"E"
                  then yyQ60(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = #"x"
              then yyQ64(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"<"
              then yyQ66(strm', lastMatch)
            else if inp < #"<"
              then if inp = #","
                  then yystuck(lastMatch)
                else if inp < #","
                  then if inp = #"#"
                      then yyQ66(strm', lastMatch)
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ66(strm', lastMatch)
                          else yystuck(lastMatch)
                    else if inp = #"'"
                      then yystuck(lastMatch)
                    else if inp < #"'"
                      then yyQ66(strm', lastMatch)
                    else if inp <= #")"
                      then yystuck(lastMatch)
                      else yyQ66(strm', lastMatch)
                else if inp = #"0"
                  then yystuck(lastMatch)
                else if inp < #"0"
                  then if inp = #"."
                      then yystuck(lastMatch)
                      else yyQ66(strm', lastMatch)
                else if inp = #":"
                  then yyQ66(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"`"
              then yyQ66(strm', lastMatch)
            else if inp < #"`"
              then if inp = #"\\"
                  then yyQ66(strm', lastMatch)
                else if inp < #"\\"
                  then if inp = #"A"
                      then yyQ67(strm', lastMatch)
                    else if inp < #"A"
                      then yyQ66(strm', lastMatch)
                    else if inp = #"["
                      then yystuck(lastMatch)
                      else yyQ67(strm', lastMatch)
                else if inp = #"^"
                  then yyQ66(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"}"
              then yystuck(lastMatch)
            else if inp < #"}"
              then if inp = #"{"
                  then yystuck(lastMatch)
                else if inp = #"|"
                  then yyQ66(strm', lastMatch)
                  else yyQ67(strm', lastMatch)
            else if inp = #"~"
              then yyQ66(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"."
                  then yyQ56(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp < #"."
                  then if inp = #"'"
                      then yyQ67(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                      else yyAction3(strm, yyNO_MATCH)
                else if inp = #"/"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ67(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"_"
              then yyQ67(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"A"
                  then yyQ67(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction3(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ67(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ67(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"a"
              then yyAction3(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ67(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
and yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #","
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < #","
                  then if inp = #"#"
                      then yyQ66(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ66(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                          else yyAction3(strm, yyNO_MATCH)
                    else if inp = #"'"
                      then yyAction3(strm, yyNO_MATCH)
                    else if inp < #"'"
                      then yyQ66(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                    else if inp <= #")"
                      then yyAction3(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp = #"/"
                  then yyQ66(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp < #"/"
                  then if inp = #"-"
                      then yyQ66(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                      else yyQ56(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp = #":"
                  then yyQ66(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyQ66(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"`"
              then if inp = #"]"
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < #"]"
                  then if inp = #"A"
                      then yyAction3(strm, yyNO_MATCH)
                    else if inp < #"A"
                      then yyQ66(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                    else if inp = #"\\"
                      then yyQ66(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                      else yyAction3(strm, yyNO_MATCH)
                else if inp = #"^"
                  then yyQ66(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ66(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ66(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #","
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #","
                  then if inp = #"#"
                      then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                          else yyAction2(strm, yyNO_MATCH)
                    else if inp = #"'"
                      then yyAction2(strm, yyNO_MATCH)
                    else if inp < #"'"
                      then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp <= #")"
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #"/"
                  then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"/"
                  then if inp = #"-"
                      then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyQ56(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #":"
                  then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"`"
              then if inp = #"]"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #"]"
                  then if inp = #"A"
                      then yyAction2(strm, yyNO_MATCH)
                    else if inp < #"A"
                      then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp = #"\\"
                      then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = #"^"
                  then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #","
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #","
                  then if inp = #"#"
                      then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                          else yyAction2(strm, yyNO_MATCH)
                    else if inp = #"'"
                      then yyAction2(strm, yyNO_MATCH)
                    else if inp < #"'"
                      then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp <= #")"
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #"0"
                  then yyQ57(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"."
                      then yyQ56(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #":"
                  then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ58(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"`"
              then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"`"
              then if inp = #"]"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #"]"
                  then if inp = #"A"
                      then yyAction2(strm, yyNO_MATCH)
                    else if inp < #"A"
                      then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp = #"\\"
                      then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = #"^"
                  then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ69(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ68(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"."
                  then yyQ56(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"."
                  then if inp = #"'"
                      then yyQ70(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyAction1(strm, yyNO_MATCH)
                else if inp = #"/"
                  then yyAction1(strm, yyNO_MATCH)
                  else yyQ70(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"_"
              then yyQ70(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"A"
                  then yyQ70(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction1(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ70(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ70(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"a"
              then yyAction1(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ70(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"."
                  then yyQ56(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"."
                  then if inp = #"'"
                      then yyQ70(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyAction1(strm, yyNO_MATCH)
                else if inp = #"/"
                  then yyAction1(strm, yyNO_MATCH)
                  else yyQ70(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"_"
              then yyQ70(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"A"
                  then yyQ70(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction1(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ70(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ70(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"a"
              then yyAction1(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ70(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ71(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ59(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp = #"E"
              then yyQ60(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < #"E"
              then if inp <= #"9"
                  then yyQ71(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ71(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ59(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp = #"E"
              then yyQ60(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"E"
              then if inp <= #"9"
                  then yyQ71(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ75(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ75(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ75(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ75(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ75(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ75(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ75(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ75(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ75(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ75(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ75(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ75(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"0"
              then yyAction19(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ73(strm', lastMatch)
            else if inp = #"x"
              then yyQ74(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ60(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"/"
                  then yyAction15(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ59(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                      else yyAction15(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ71(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp = #"x"
              then yyQ64(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"x"
              then if inp = #"w"
                  then yyQ72(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ77(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ76(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyQ79(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ78(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ80(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ80(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ80(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ80(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ80(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ80(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ80(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction46(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #","
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #","
                  then if inp = #"("
                      then yyAction46(strm, yyNO_MATCH)
                    else if inp < #"("
                      then if inp = #"'"
                          then yyQ80(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                          else yyAction46(strm, yyNO_MATCH)
                    else if inp = #"+"
                      then yyQ80(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyAction46(strm, yyNO_MATCH)
                else if inp = #"."
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #"."
                  then yyQ80(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp <= #"/"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ80(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp = #"["
              then yyAction46(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"@"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #"@"
                  then if inp = #"?"
                      then yyQ80(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyAction46(strm, yyNO_MATCH)
                  else yyQ80(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp = #"`"
              then yyAction46(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ80(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ80(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #","
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #","
                  then if inp = #"#"
                      then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                          else yyAction2(strm, yyNO_MATCH)
                    else if inp = #"'"
                      then yyAction2(strm, yyNO_MATCH)
                    else if inp < #"'"
                      then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp <= #")"
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #"/"
                  then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"/"
                  then if inp = #"-"
                      then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyQ56(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #":"
                  then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"`"
              then if inp = #"]"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #"]"
                  then if inp = #"A"
                      then yyAction2(strm, yyNO_MATCH)
                    else if inp < #"A"
                      then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp = #"\\"
                      then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = #"^"
                  then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"\f"
              then if inp = #"\t"
                  then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\v"
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #" "
              then if inp <= #"\r"
                  then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"\f"
              then if inp = #"\t"
                  then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\v"
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #" "
              then if inp <= #"\r"
                  then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ45(strm', lastMatch)
            else if inp < #"0"
              then if inp = #"#"
                  then yyQ38(strm', lastMatch)
                else if inp < #"#"
                  then if inp = #"\f"
                      then yyQ36(strm', lastMatch)
                    else if inp < #"\f"
                      then if inp = #"\n"
                          then yyQ37(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ36(strm', lastMatch)
                              else yyQ35(strm', lastMatch)
                          else yyQ35(strm', lastMatch)
                    else if inp = #" "
                      then yyQ36(strm', lastMatch)
                    else if inp < #" "
                      then if inp <= #"\r"
                          then yyQ36(strm', lastMatch)
                          else yyQ35(strm', lastMatch)
                    else if inp = #"!"
                      then yyQ38(strm', lastMatch)
                      else yyQ39(strm', lastMatch)
                else if inp = #"*"
                  then yyQ38(strm', lastMatch)
                else if inp < #"*"
                  then if inp = #"("
                      then yyQ41(strm', lastMatch)
                    else if inp < #"("
                      then if inp = #"'"
                          then yyQ40(strm', lastMatch)
                          else yyQ38(strm', lastMatch)
                      else yyQ42(strm', lastMatch)
                else if inp = #"-"
                  then yyQ38(strm', lastMatch)
                else if inp < #"-"
                  then if inp = #","
                      then yyQ43(strm', lastMatch)
                      else yyQ38(strm', lastMatch)
                else if inp = #"."
                  then yyQ44(strm', lastMatch)
                  else yyQ38(strm', lastMatch)
            else if inp = #"^"
              then yyQ38(strm', lastMatch)
            else if inp < #"^"
              then if inp = #"A"
                  then yyQ48(strm', lastMatch)
                else if inp < #"A"
                  then if inp = #";"
                      then yyQ47(strm', lastMatch)
                    else if inp < #";"
                      then if inp = #":"
                          then yyQ38(strm', lastMatch)
                          else yyQ46(strm', lastMatch)
                      else yyQ38(strm', lastMatch)
                else if inp = #"\\"
                  then yyQ38(strm', lastMatch)
                else if inp < #"\\"
                  then if inp = #"["
                      then yyQ49(strm', lastMatch)
                      else yyQ48(strm', lastMatch)
                  else yyQ50(strm', lastMatch)
            else if inp = #"|"
              then yyQ38(strm', lastMatch)
            else if inp < #"|"
              then if inp = #"a"
                  then yyQ48(strm', lastMatch)
                else if inp < #"a"
                  then if inp = #"_"
                      then yyQ51(strm', lastMatch)
                      else yyQ38(strm', lastMatch)
                else if inp = #"{"
                  then yyQ52(strm', lastMatch)
                  else yyQ48(strm', lastMatch)
            else if inp = #"~"
              then yyQ54(strm', lastMatch)
            else if inp = #"}"
              then yyQ53(strm', lastMatch)
              else yyQ35(strm', lastMatch)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ29(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ29(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ29(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ29(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ29(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ29(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ28(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ28(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ28(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ28(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ28(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ28(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ27(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ27(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ27(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ27(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ27(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ27(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ26(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ26(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ26(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ26(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ26(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ26(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"@"
              then yyQ30(strm', lastMatch)
            else if inp < #"@"
              then yystuck(lastMatch)
            else if inp <= #"_"
              then yyQ30(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ32(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ32(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ31(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ31(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yystuck(lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yystuck(lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yystuck(lastMatch)
                      else yyQ13(strm', lastMatch)
                  else yyQ13(strm', lastMatch)
            else if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ13(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"\\"
              then yyQ33(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyAction41(strm, yyNO_MATCH)
            else if inp < #"_"
              then if inp = #"\""
                  then yyQ14(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                else if inp < #"\""
                  then if inp = #"\f"
                      then yyQ13(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                    else if inp < #"\f"
                      then if inp = #"\t"
                          then yyQ13(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                        else if inp < #"\t"
                          then yyAction41(strm, yyNO_MATCH)
                        else if inp = #"\v"
                          then yyAction41(strm, yyNO_MATCH)
                          else yyQ13(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                    else if inp = #" "
                      then yyQ13(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                    else if inp < #" "
                      then if inp <= #"\r"
                          then yyQ13(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                          else yyAction41(strm, yyNO_MATCH)
                      else yyAction41(strm, yyNO_MATCH)
                else if inp = #"\\"
                  then yyQ16(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                else if inp < #"\\"
                  then if inp = #"0"
                      then yyQ15(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction41(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ15(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                      else yyAction41(strm, yyNO_MATCH)
                else if inp = #"]"
                  then yyAction41(strm, yyNO_MATCH)
                  else yyQ17(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
            else if inp = #"o"
              then yyAction41(strm, yyNO_MATCH)
            else if inp < #"o"
              then if inp = #"c"
                  then yyAction41(strm, yyNO_MATCH)
                else if inp < #"c"
                  then if inp = #"a"
                      then yyQ18(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                    else if inp = #"b"
                      then yyQ19(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                      else yyAction41(strm, yyNO_MATCH)
                else if inp = #"g"
                  then yyAction41(strm, yyNO_MATCH)
                else if inp < #"g"
                  then if inp = #"f"
                      then yyQ20(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                      else yyAction41(strm, yyNO_MATCH)
                else if inp = #"n"
                  then yyQ21(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                  else yyAction41(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ23(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"r"
                  then yyQ22(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                  else yyAction41(strm, yyNO_MATCH)
            else if inp = #"v"
              then yyQ25(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
            else if inp = #"u"
              then yyQ24(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
              else yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyAction25(strm, yyNO_MATCH)
            else if inp < #"\""
              then if inp = #"\n"
                  then yyAction25(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp = #"\\"
              then yyAction25(strm, yyNO_MATCH)
              else yyQ34(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyAction25(strm, yyNO_MATCH)
            else if inp < #"\""
              then if inp = #"\n"
                  then yyAction25(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp = #"\\"
              then yyAction25(strm, yyNO_MATCH)
              else yyQ34(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ11(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp < #"\""
              then if inp = #"\n"
                  then yyQ10(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyQ9(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ12(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyQ9(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ7(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ8(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"("
              then yyQ5(strm', lastMatch)
            else if inp < #"("
              then if inp = #"\n"
                  then yyQ4(strm', lastMatch)
                  else yyQ3(strm', lastMatch)
            else if inp = #"*"
              then yyQ6(strm', lastMatch)
              else yyQ3(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of C => yyQ0(!(yystrm), yyNO_MATCH)
    | S => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
