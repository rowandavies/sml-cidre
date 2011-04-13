
    type lstream = (Lexer.Token * Paths.region) Stream.stream
    type lfront = (Lexer.Token * Paths.region) Stream.front

    (* should be able to define:
    sort inflstream = (Lexer.Token * Paths.region) Stream.infStream
    sort inflfront =  (Lexer.Token * Paths.region) Stream.infFront
    *)

    (*[ val parseExp : (Lexer.Token * Paths.region) Stream.infStream * ExtSyn.term pStable
                        -> ExtSyn.term * (Lexer.Token * Paths.region) Stream.infFront
        val parseExp' : (Lexer.Token * Paths.region) Stream.infFront * ExtSyn.term pStable
                        -> ExtSyn.term * (Lexer.Token * Paths.region) Stream.infFront
        val parseDec : (Lexer.Token * Paths.region) Stream.infStream -> ExtSyn.decl * (Lexer.Token * Paths.region) Stream.infFront
        val parseDec' : (Lexer.Token * Paths.region) Stream.infFront -> ExtSyn.decl * (Lexer.Token * Paths.region) Stream.infFront
        val parseDec1 : string option * (Lexer.Token * Paths.region) Stream.infFront -> ExtSyn.decl * (Lexer.Token * Paths.region) Stream.infFront
        val decideRParen : Paths.region * (ExtSyn.term * (Lexer.Token * Paths.region) Stream.infFront) * ExtSyn.term pStable -> ExtSyn.term * (Lexer.Token * Paths.region) Stream.infFront
        val decideRBrace : Paths.region * (ExtSyn.decl * (Lexer.Token * Paths.region) Stream.infFront) * ExtSyn.term pStable -> ExtSyn.term * (Lexer.Token * Paths.region) Stream.infFront
        val decideRBracket : Paths.region * (ExtSyn.decl * (Lexer.Token * Paths.region) Stream.infFront) * ExtSyn.term pStable -> ExtSyn.term * (Lexer.Token * Paths.region) Stream.infFront;
    ]*)
    fun parseExp (s, p) = parseExp' (Stream.expose s, p)

    and parseExp' (Stream.Cons((Lexer.ID(idCase,name),r), s), p) =
        let
	  val tm = idToTerm (idCase, name, r)
	in
	  (* Currently, we cannot override fixity status of identifiers *)
	  (* Thus isQuoted always returns false *)
	  if isQuoted (idCase)
	    then parseExp (s, shiftAtom (tm, p))
	  else case fixityLookup (name)
	         of FX.Nonfix =>
		      parseExp (s, shiftAtom (tm, p))
	          | FX.Infix infixity =>
		      parseExp (s, resolve (r, infixOp (infixity, tm), p))
		  | FX.Prefix (prec) =>
		      parseExp (s, resolve (r, prefixOp (prec, tm), p))
		  | FX.Postfix (prec) =>
		      parseExp (s, resolve (r, postfixOp (prec, tm), p))
	end
      | parseExp' (Stream.Cons((Lexer.UNDERSCORE,r), s), p) =
          parseExp (s, shiftAtom (ExtSyn.omitobj r, p))
      | parseExp' (Stream.Cons((Lexer.TYPE,r), s), p) =
	  parseExp (s, shiftAtom (ExtSyn.typ r, p))
      | parseExp' (Stream.Cons((Lexer.COLON,r), s), p) =
	  parseExp (s, resolve (r, colonOp, p))
      | parseExp' (Stream.Cons((Lexer.BACKARROW,r), s), p) =
	  parseExp (s, resolve (r, backArrowOp, p))
      | parseExp' (Stream.Cons((Lexer.ARROW,r), s), p) =
          parseExp (s, resolve (r, arrowOp, p))
      | parseExp' (Stream.Cons((Lexer.LPAREN,r), s), p) =
	  decideRParen (r, parseExp (s, nil), p)
      | parseExp' (f as Stream.Cons((Lexer.RPAREN,r), s), p) =
	  (reduceAll (r, p), f)
      | parseExp' (Stream.Cons((Lexer.LBRACE,r), s), p) =
	  decideRBrace (r, parseDec (s), p)
      | parseExp' (f as Stream.Cons((Lexer.RBRACE,r), s), p) =
          (reduceAll (r, p), f)
      | parseExp' (Stream.Cons((Lexer.LBRACKET,r), s), p) =
          decideRBracket (r, parseDec (s), p)
      | parseExp' (f as Stream.Cons((Lexer.RBRACKET,r), s), p) =
	  (reduceAll (r, p), f)
      | parseExp' (f as Stream.Cons((Lexer.EQUAL,r), s), p) =
	  (reduceAll (r, p), f)
      | parseExp' (f as Stream.Cons((Lexer.DOT,r), s), p) =
	  (reduceAll (r, p), f)
      | parseExp' (f as Stream.Cons((Lexer.EOF,r), s), p) =
	  (reduceAll (r, p), f)
      | parseExp' (Stream.Cons((t,r), s), p) =
	  (* should cover %postfix, %prefix, %infix, %name *)
	  (* possible error recovery: insert DOT *)
	  error (r, "Unexpected token " ^ Lexer.toString t
			    ^ " found in expression")

    and parseDec (s) = parseDec' (Stream.expose s)

    and parseDec' (Stream.Cons ((Lexer.ID (Lexer.Quoted,name), r), s')) =
          (* cannot happen at present *)
	  error (r, "Illegal bound quoted identifier " ^ name)
      | parseDec' (Stream.Cons ((Lexer.ID (idCase,name), r), s')) =
	(case fixityLookup(name)
	   of FX.Nonfix => parseDec1 (SOME(name), Stream.expose s')
	    | FX.Infix _ => error (r, "Cannot bind infix identifier " ^ name)
	    | FX.Prefix _ => error (r, "Cannot bind prefix identifier " ^ name)
            | FX.Postfix _ => error (r, "Cannot bind postfix identifier" ^ name))
      | parseDec' (Stream.Cons ((Lexer.UNDERSCORE, r), s')) =
          parseDec1 (NONE, Stream.expose s')
      | parseDec' (Stream.Cons ((Lexer.EOF, r), s')) =
	  error (r, "Unexpected end of stream in declaration")
      | parseDec' (Stream.Cons ((t, r), s')) =
	  error (r, "Expected variable name, found token "
			    ^ Lexer.toString t)

    and parseDec1 (x, Stream.Cons((Lexer.COLON, r), s')) =
        let val (tm, f'') = parseExp (s', nil)
	in (ExtSyn.decl (x, tm), f'') end
      | parseDec1 (x, f as Stream.Cons((Lexer.RBRACE, _), _)) =
          (ExtSyn.decl0 (x), f)
      | parseDec1 (x, f as Stream.Cons ((Lexer.RBRACKET, _), _)) =
          (ExtSyn.decl0 (x), f)
      | parseDec1 (x, Stream.Cons ((t,r), s')) =
	  error (r, "Expected optional type declaration, found token "
			    ^ Lexer.toString (t))

    and decideRParen (r0, (tm, Stream.Cons((Lexer.RPAREN,r), s)), p) =
          parseExp (s, shiftAtom(tm,p))
      | decideRParen (r0, (tm, Stream.Cons((_, r), s)), p) =
	  error (Paths.join(r0, r), "Unmatched open parenthesis")

    and decideRBrace (r0, (dec, Stream.Cons ((Lexer.RBRACE,r), s)), p) =
          let
	    val (tm, f') = parseExp (s, nil)
	  in
	    parseExp' (f', shiftAtom (ExtSyn.pi (dec, tm,
						   Paths.join (r0, r)), p))
	  end
      | decideRBrace (r0, (dec, Stream.Cons ((_, r), s)), p) =
	  error (Paths.join(r0, r), "Unmatched open brace")

    and decideRBracket (r0, (dec, Stream.Cons ((Lexer.RBRACKET,r), s)), p) =
          let
	    val (tm, f') = parseExp (s, nil)
	  in
	    parseExp' (f', shiftAtom (ExtSyn.lam (dec, tm,
						    Paths.join (r0, r)), p))
	  end
      | decideRBracket (r0, (dec, Stream.Cons ((_, r), s)), p) =
	  error (Paths.join(r0, r), "Unmatched open bracket")

  val parseTerm' : (Lexer.Token * Paths.region) Stream.infFront 
                    -> ExtSyn.term * (Lexer.Token * Paths.region) Stream.infFront =
         (fn f => parseExp' (f, nil));
  val parseDec' = parseDec';
