(* Parsing Terms and Variable Declarations *)
(* Author: Frank Pfenning *)
(* Sorts added by: Frank Pfenning and Rowan Davies *)

functor ParseTerm
  ((*! structure Parsing' : PARSING !*)
   structure ExtSyn' : EXTSYN
   (*! sharing Parsing'.Lexer.Paths = ExtSyn'.Paths !*)
   structure Names : NAMES)
   : PARSE_TERM = 
struct

  (*! structure Parsing = Parsing' !*)
  structure ExtSyn = ExtSyn'

  local
    (* some shorthands *)
    structure L = Lexer
    structure LS = Stream  
    (*! structure Paths = Lexer.Paths !*)
    structure FX = Names.Fixity

    (* Operators and atoms for operator precedence parsing *)
    datatype 'a operator =
        Atom of 'a
      | Infix of (FX.precedence * FX.associativity) * ('a * 'a -> 'a)
      | Prefix of FX.precedence * ('a -> 'a)
      | Postfix of FX.precedence * ('a -> 'a)

    (*[  datasort 'a Atom = Atom of 'a
              and 'a Infix = Infix of (FX.precedence * FX.associativity) * ('a * 'a -> 'a)
              and 'a Prefix = Prefix of FX.precedence * ('a -> 'a)
              and 'a Postfix = Postfix of FX.precedence * ('a -> 'a);
         datasort 'a shiftable = Infix of (FX.precedence * FX.associativity) * ('a * 'a -> 'a)
                               | Prefix of FX.precedence * ('a -> 'a)
                               | Atom of 'a
    ]*)

    (* Predeclared infix operators *)
    val juxOp = Infix ((FX.inc FX.maxPrec, FX.Left), ExtSyn.app) (* juxtaposition *)
    val arrowOp = Infix ((FX.dec FX.minPrec, FX.Right), ExtSyn.arrow)
    val backArrowOp = Infix ((FX.dec FX.minPrec, FX.Left), ExtSyn.backarrow)
    val colonOp = Infix ((FX.dec (FX.dec FX.minPrec), FX.Left), ExtSyn.hastype)

    fun infixOp (infixity, tm) =
          Infix (infixity, (fn (tm1, tm2) => ExtSyn.app (ExtSyn.app (tm, tm1), tm2)))
    fun prefixOp (prec, tm) =
          Prefix (prec, (fn tm1 => ExtSyn.app (tm, tm1)))
    fun postfixOp (prec, tm) =
          Postfix (prec, (fn tm1 => ExtSyn.app (tm, tm1)))

    fun idToTerm (L.Lower, ids, name, r) = ExtSyn.lcid (ids, name, r)
      | idToTerm (L.Upper, ids, name, r) = ExtSyn.ucid (ids, name, r)
      | idToTerm (L.Quoted, ids, name, r) = ExtSyn.quid (ids, name, r)

    fun isQuoted (L.Quoted) = true
      | isQuoted _ = false

    type term = ExtSyn.term    
    type opr = term operator


    (* type stack = (ExtSyn.term operator) list *)    

    (* This datatype replaces the use of the standard lists above, so that we can
       define non-parametric refinements. *)
    datatype stack = snil | ::: of opr * stack;
    infixr 5 :::
    
    (* Various refinements of stacks to enforce invariants *)
    (*[ datasort pSnil = snil
             and pOp = ::: of term Infix * pAtom
                        | ::: of term Prefix * pSnil
                        | ::: of term Prefix * pOp
             and pAtom = ::: of term Atom * pSnil
                          | ::: of term Atom * pOp
        datasort pStable =  (* pOp | pAtom | pSnil *)
                        snil
                      | ::: of term Atom * pSnil
                      | ::: of term Atom * pOp
                      | ::: of term Infix * pAtom
                      | ::: of term Prefix * pSnil
                      | ::: of term Prefix * pOp
        datasort pComplete = ::: of term Atom * pSnil
                              | ::: of term Atom * pOp
                              | ::: of term Postfix * pAtom
        datasort pRedex = ::: of term Postfix * pAtom
                           | ::: of term Atom * pOp
        datasort p = snil (* pStable | pRedex *)
                      | ::: of term Atom * pSnil
                      | ::: of term Atom * pOp
                      | ::: of term Infix * pAtom
                      | ::: of term Postfix * pAtom
                      | ::: of term Prefix * pSnil
                      | ::: of term Prefix * pOp    ]*)

    (* Instantiation of error to a particular sort *)
    val serror = (Parsing.error (*[ :> Paths.region * string -> (pAtom & pSnil) ]*) )
    (*val error_res = (Parsing.error (*[ :> Paths.region * string -> unit * Stream.infFront ]*) )*)


    (* The next section deals generically with fixity parsing          *)
    (* Because of juxtaposition, it is not clear how to turn this      *)
    (* into a separate module without passing a juxtaposition operator *)
    (* into the shift and resolve functions                            *)

    structure P :>
      sig
        (*[ reduce <: pRedex -> pAtom ]*)
        val reduce : stack -> stack
        (*[ reduceAll :> Paths.region * p -> term ]*)
        val reduceAll : Paths.region * stack -> ExtSyn.term
        (*[ shiftAtom :> term * pStable -> pStable ]*)
        val shiftAtom : ExtSyn.term * stack -> stack
        (*[ shift :>  Paths.region * term shiftable * pStable -> pStable
                    & Paths.region * term Postfix * pAtom -> pRedex
                    & Paths.region * term Postfix * pSnil -> pStable
                    & Paths.region * term Postfix * pOp -> pStable  ]*)
        val shift : Paths.region * opr * stack -> stack
        (*[ resolve :> Paths.region * term operator * pStable -> pStable  ]*)
        val resolve : Paths.region * opr * stack -> stack
      end =
    struct
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
      (*[ reduce :> pRedex -> pAtom ]*)
      fun reduce (Atom(tm2):::Infix(_,con):::Atom(tm1):::p') =
             Atom(con(tm1,tm2)):::p'
        | reduce (Atom(tm):::Prefix(_,con):::p') = Atom(con(tm)):::p'
        | reduce (Postfix(_,con):::Atom(tm):::p') = Atom(con(tm)):::p'
        (* no other cases should be possible by stack invariant *)

      (* val reduceRec : <pStable> -> ExtSyn.term *)
      (*[ reduceRec :> pComplete -> term ]*)
      fun reduceRec (Atom(e):::snil) = e
        | reduceRec (p) = reduceRec (reduce p)

      (* val reduceAll : <p> -> ExtSyn.term *)
      (*[ val reduceAll :> Paths.region * p -> term ]*)
      fun reduceAll (r, Atom(e):::snil) = e
        | reduceAll (r, Infix _:::p') = Parsing.error (r, "Incomplete infix expression")
        | reduceAll (r, Prefix _:::p') = Parsing.error (r, "Incomplete prefix expression")
        | reduceAll (r, snil) = Parsing.error (r, "Empty expression")
        | reduceAll (r, p) = reduceRec (reduce p)

      (* val shiftAtom : term * <pStable> -> <p> *)
      (* does not raise Error exception *)
      (*[ shiftAtom :> term * pStable -> pStable ]*)
      fun shiftAtom (tm, p as (Atom _:::p')) =
          (* insert juxOp operator and reduce *)
          (* juxtaposition binds most strongly *)
            reduce (Atom(tm):::juxOp:::p)
        | shiftAtom (tm, p) = Atom(tm):::p

      (* val shift : Paths.region * opr * <pStable> -> <p> *)
      (*[ shift :>  Paths.region * term shiftable * pStable -> pStable
                  & Paths.region * term Postfix * pAtom -> pRedex
                  & Paths.region * term Postfix * pSnil -> pStable
                  & Paths.region * term Postfix * pOp -> pStable  ]*)
      fun shift (r, opr as Atom _, p as (Atom _:::p')) =
            (* insert juxOp operator and reduce *)
            (* juxtaposition binds most strongly *)
            reduce (opr:::juxOp:::p)
        (* Atom/Infix: shift *)
        (* Atom/Prefix: shift *)
        (* Atom/Postfix cannot arise *)
        (* Atom/Empty: shift *)
        (* Infix/Atom: shift *)
        | shift (r, Infix _, Infix _:::p') =
            serror (r, "Consective infix operators")
        | shift (r, Infix _, Prefix _:::p') =
            serror (r, "Infix operator following prefix operator")
        (* Infix/Postfix cannot arise *)
        | shift (r, Infix _, snil) =
            serror (r, "Leading infix operator")
        | shift (r, opr as Prefix _, p as (Atom _:::p')) =
           (* insert juxtaposition operator *)
           (* will be reduced later *)
           opr:::juxOp:::p
        (* Prefix/{Infix,Prefix,Empty}: shift *)
        (* Prefix/Postfix cannot arise *)
        (* Postfix/Atom: shift, reduced immediately *)
        | shift (r, Postfix _, Infix _:::p') =
            serror (r, "Postfix operator following infix operator")
        | shift (r, Postfix _, Prefix _:::p') =
            serror (r, "Postfix operator following prefix operator")
        (* Postfix/Postfix cannot arise *)
        | shift (r, Postfix _, snil) =
            serror (r, "Leading postfix operator")
        | shift (r, opr, p) = opr:::p

      (* val resolve : Paths.region * opr * <pStable> -> <p> *)
      (* Decides, based on precedence of opr compared to the top of the
         stack whether to shift the new operator or reduce the stack
      *)
      (*[ val resolve :> Paths.region * term operator * pStable -> pStable  ]*)
      fun resolve (r, opr as Infix((prec, assoc), _),
                     p as (Atom(_):::Infix((prec', assoc'), _):::p')) =
          (case (FX.compare(prec,prec'), assoc, assoc')
             of (GREATER,_,_) => shift(r, opr, p)
              | (LESS,_,_) => resolve (r, opr, reduce(p))
              | (EQUAL, FX.Left, FX.Left) => resolve (r, opr, reduce(p))
              | (EQUAL, FX.Right, FX.Right) => shift(r, opr, p)
              | _ => serror (r, "Ambiguous: infix following infix of identical precedence"))
        | resolve (r, opr as Infix ((prec, assoc), _),
                     p as (Atom(_):::Prefix(prec', _):::p')) =
          (case FX.compare(prec,prec')
             of GREATER => shift(r, opr, p)
              | LESS => resolve (r, opr, reduce(p))
              | EQUAL => serror (r, "Ambiguous: infix following prefix of identical precedence"))
        (* infix/atom/atom cannot arise *)
        (* infix/atom/postfix cannot arise *)
        (* infix/atom/<empty>: shift *)

        (* always shift prefix *)
        | resolve (r, opr as Prefix _, p) =
            shift(r, opr, p)

        (* always reduce postfix, possibly after prior reduction *)
        | resolve (r, opr as Postfix(prec, _),
                     p as (Atom _:::Prefix(prec', _):::p')) =
            (case FX.compare(prec,prec')
               of GREATER => reduce (shift (r, opr, p))
                | LESS => resolve (r, opr, reduce (p))
                | EQUAL => serror (r, "Ambiguous: postfix following prefix of identical precedence"))
        (* always reduce postfix *)
        | resolve (r, opr as Postfix(prec, _),
                     p as (Atom _:::Infix((prec', _), _):::p')) =
            (case FX.compare(prec,prec')
               of GREATER => reduce (shift (r, opr, p))
                | LESS => resolve (r, opr, reduce (p))
                | EQUAL => serror (r, "Ambiguous: postfix following infix of identical precedence"))
        | resolve (r, opr as Postfix _, p as (Atom _:::snil)) =
            reduce (shift (r, opr, p))

        (* default is shift *)
        | resolve (r, opr, p) =
            shift(r, opr, p)

    end  (* structure P *)


    (*[ sortdef inflstream = (Lexer.Token * Paths.region) LS.infStream 
        sortdef inflfront =  (Lexer.Token * Paths.region) LS.infFront    ]*)

    val perror1 = (Parsing.error (*[ :> Paths.region * string -> inflfront ]*) )
    val perror = (Parsing.error (*[ :> Paths.region * string -> 'a * inflfront ]*) )
    val perror2 = (Parsing.error (*[ :> Paths.region * string -> inflfront * 'a ]*) )

    type decRes = string option * term option

    (*[ val parseExp :> inflstream * pStable -> term * inflfront
        val parseExp' :> inflfront * pStable -> term * inflfront
        val parseDec :> inflstream -> decRes * inflfront
        val parseDec' :> inflfront -> decRes * inflfront
        val parseDec1 :> string option * inflfront -> decRes * inflfront
        val decideRParen :> Paths.region * (term * inflfront) * pStable -> term * inflfront
        val decideRBrace :> Paths.region * (decRes * inflfront) * pStable -> term * inflfront
        val decideRBracket :> Paths.region * (decRes * inflfront) * pStable -> term * inflfront

        (* The token returned by parseQualId' is always an identifier. *)
        val parseQualId' :> inflfront -> (string list * (L.idToken * Paths.region))  * inflfront
        val stripBar :> inflfront -> inflfront
        val stripRBrace :> inflfront -> inflfront * Paths.region
        val parseQualIds' :> inflfront -> ((string list * string) list)  * inflfront
        val parseQualIds1 :> ((string list * string) list) * inflfront -> 
                       ((string list * string) list)  * inflfront
        val parseFreeze' :> inflfront * (string list * string) list -> 
                            ((string list * string) list) * inflfront
        val parseDeterministic' : inflfront * (string list * string) list -> 
                                  ((string list * string) list)  * inflfront
        val parseBracedDec :> Paths.region * inflfront -> ExtSyn.dec * inflfront 
        val parseCtx :> bool * ExtSyn.dec list * inflfront -> (ExtSyn.dec list)  * inflfront
    ]*)


    (* parseQualifier' f = (ids, f')
       pre: f begins with L.ID

       Note: precondition for recursive call is enforced by the lexer. *)
    fun parseQualId' (f as LS.Cons ((t as L.ID (_, id), r), s')) =
        (case LS.expose s'
           of LS.Cons ((L.PATHSEP, _), s'') =>
              let
                val ((ids, (t, r)), f') = parseQualId' (LS.expose s'')
              in
                ((id::ids, (t, r)), f')
              end
            | f' => ((nil, (t, r)), f'))

         (* Added the following to avoid warning from sort checker. Better would be to
            check the invariant, but streams are a parametric type constructor, so this
            would require them to be manually instantiated.  - Rowan *) 
      | parseQualId' _ = raise Match
   
    fun stripBar (LS.Cons ((L.ID (_, "|"), r), s')) = (LS.expose s')
      | stripBar (f as LS.Cons ((L.RPAREN, r), s')) = f
      | stripBar (LS.Cons ((t, r), s')) =
          perror1 (r, "Expected `|', found token " ^ L.toString t)
           
    fun parseQualIds1 (ls, f as LS.Cons ((t as L.ID (_, id), r0), s')) =
        let
          val ((ids, (L.ID (idCase, name), r1)), f') = parseQualId' f
          val r = Paths.join (r0, r1)
          val f'' = stripBar f'
        in
          parseQualIds1 ((ids, name) :: ls, f'')
        end
      | parseQualIds1 (ls,  LS.Cons ((L.RPAREN, r), s')) =
         (ls, LS.expose s')
      | parseQualIds1 (ls, LS.Cons ((t, r), s)) =
         perror (r, "Expected label, found token " ^ L.toString t)

    fun parseQualIds' (LS.Cons ((L.LPAREN, r), s')) =
        parseQualIds1 (nil, LS.expose s')
      | parseQualIds' (LS.Cons ((t, r), s')) =
          perror (r, "Expected list of labels, found token " ^ L.toString t)

    fun parseFreeze' (f as LS.Cons ((L.ID _, _), _), qids) =
        let
          val ((ids, (L.ID (idCase, name), r1)), f') = parseQualId' f
        in
          parseFreeze' (f', (ids, name)::qids)
        end
      | parseFreeze' (f as LS.Cons ((L.DOT, _), _), qids) =
          (List.rev qids, f)
      | parseFreeze' (LS.Cons ((t, r), s'), qids) = 
          perror (r, "Expected identifier, found token "
                            ^ L.toString t)

    fun parseDeterministic' (f as LS.Cons ((L.ID _, _), _), qids) =
        let
          val ((ids, (L.ID (idCase, name), r1)), f') = parseQualId' f
        in
          parseDeterministic' (f', (ids, name)::qids)
        end
      | parseDeterministic' (f as LS.Cons ((L.DOT, _), _), qids) =
          (List.rev qids, f)
      | parseDeterministic' (LS.Cons ((t, r), s'), qids) = 
          perror (r, "Expected identifier, found token "
                            ^ L.toString t)

    (* val parseExp : (L.token * L.region) LS.stream * <p>
                        -> ExtSyn.term * (L.token * L.region) LS.front *)
    fun parseExp (s, p) = parseExp' (LS.expose s, p)

    and parseExp' (f as LS.Cons((L.ID _, r0), _), p) =
        let
          val ((ids, (L.ID (idCase, name), r1)), f') = parseQualId' f
          val r = Paths.join (r0, r1)
          val tm = idToTerm (idCase, ids, name, r)
        in
          (* Currently, we cannot override fixity status of identifiers *)
          (* Thus isQuoted always returns false *)
          if isQuoted (idCase)
            then parseExp' (f', P.shiftAtom (tm, p))
          else case Names.fixityLookup (Names.Qid (ids, name))
                 of FX.Nonfix =>
                      parseExp' (f', P.shiftAtom (tm, p))
                  | FX.Infix infixity =>
                      parseExp' (f', P.resolve (r, infixOp (infixity, tm), p))
                  | FX.Prefix (prec) =>
                      parseExp' (f', P.resolve (r, prefixOp (prec, tm), p))
                  | FX.Postfix (prec) =>
                      parseExp' (f', P.resolve (r, postfixOp (prec, tm), p))
        end
      | parseExp' (LS.Cons((L.UNDERSCORE,r), s), p) =
          parseExp (s, P.shiftAtom (ExtSyn.omitted r, p))
      | parseExp' (LS.Cons((L.TYPE,r), s), p) =
          parseExp (s, P.shiftAtom (ExtSyn.typ r, p))
      | parseExp' (LS.Cons((L.COLON,r), s), p) =
          parseExp (s, P.resolve (r, colonOp, p))
      | parseExp' (LS.Cons((L.BACKARROW,r), s), p) =
          parseExp (s, P.resolve (r, backArrowOp, p))
      | parseExp' (LS.Cons((L.ARROW,r), s), p) =
          parseExp (s, P.resolve (r, arrowOp, p))
      | parseExp' (LS.Cons((L.LPAREN,r), s), p) =
          decideRParen (r, parseExp (s, snil), p)
      | parseExp' (f as LS.Cons((L.RPAREN,r), s), p) =
          (P.reduceAll (r, p), f)
      | parseExp' (LS.Cons((L.LBRACE,r), s), p) =
          decideRBrace (r, parseDec (s), p)
      | parseExp' (f as LS.Cons((L.RBRACE,r), s), p) =
          (P.reduceAll (r, p), f)
      | parseExp' (LS.Cons((L.LBRACKET,r), s), p) =
          decideRBracket (r, parseDec (s), p)
      | parseExp' (f as LS.Cons((L.RBRACKET,r), s), p) =
          (P.reduceAll (r, p), f)
      | parseExp' (f as LS.Cons((L.EQUAL,r), s), p) =
          (P.reduceAll (r, p), f)
      | parseExp' (f as LS.Cons((L.DOT,r), s), p) =
          (P.reduceAll (r, p), f)
      | parseExp' (f as LS.Cons((L.EOF,r), s), p) =
          (P.reduceAll (r, p), f)
        (* for some reason, there's no dot after %define decls -kw *)
      | parseExp' (f as LS.Cons((L.SOLVE,r), s), p) =
          (P.reduceAll (r, p), f)
      | parseExp' (f as LS.Cons((L.DEFINE,r), s), p) =
          (P.reduceAll (r, p), f)
      | parseExp' (LS.Cons((L.STRING(str),r), s), p) =
          parseExp (s, P.shiftAtom (ExtSyn.scon (str,r), p))
      | parseExp' (LS.Cons((t,r), s), p) =
          (* possible error recovery: insert DOT *)
          perror (r, "Unexpected token " ^ L.toString t
                            ^ " found in expression")

    and parseDec (s) = parseDec' (LS.expose s)
    and parseDec' (LS.Cons ((L.ID (L.Quoted,name), r), s')) =
          (* cannot happen at present *)
          perror (r, "Illegal bound quoted identifier " ^ name)
      | parseDec' (LS.Cons ((L.ID (idCase,name), r), s')) =
        (case Names.fixityLookup (Names.Qid (nil, name))
           of FX.Nonfix => parseDec1 (SOME(name), LS.expose s')
            | FX.Infix _ => perror (r, "Cannot bind infix identifier " ^ name)
            | FX.Prefix _ => perror (r, "Cannot bind prefix identifier " ^ name)
            | FX.Postfix _ => perror (r, "Cannot bind postfix identifier " ^ name))
      | parseDec' (LS.Cons ((L.UNDERSCORE, r), s')) =
          parseDec1 (NONE, LS.expose s')
      | parseDec' (LS.Cons ((L.EOF, r), s')) =
          perror (r, "Unexpected end of stream in declaration")
      | parseDec' (LS.Cons ((t, r), s')) =
          perror (r, "Expected variable name, found token " ^ L.toString t)

    and parseDec1 (x, LS.Cons((L.COLON, r), s')) =
        let val (tm, f'') = parseExp (s', snil)
        in ((x, SOME tm), f'') end
      | parseDec1 (x, f as LS.Cons((L.RBRACE, _), _)) =
          ((x, NONE), f)
      | parseDec1 (x, f as LS.Cons ((L.RBRACKET, _), _)) =
          ((x, NONE), f)
      | parseDec1 (x, LS.Cons ((t,r), s')) =
          perror (r, "Expected optional type declaration, found token "
                            ^ L.toString t)

    and decideRParen (r0, (tm, LS.Cons((L.RPAREN,r), s)), p) =
          parseExp (s, P.shiftAtom(tm,p))
      | decideRParen (r0, (tm, LS.Cons((_, r), s)), p) =
          perror (Paths.join(r0, r), "Unmatched open parenthesis")

    and decideRBrace (r0, ((x, yOpt), LS.Cons ((L.RBRACE,r), s)), p) =
          let
            val dec = (case yOpt
                         of NONE => ExtSyn.dec0 (x, Paths.join (r0, r))
                          | SOME y => ExtSyn.dec (x, y, Paths.join (r0, r)))
            val (tm, f') = parseExp (s, snil)
          in
            parseExp' (f', P.shiftAtom (ExtSyn.pi (dec, tm), p))
          end
      | decideRBrace (r0, (_, LS.Cons ((_, r), s)), p) =
          perror (Paths.join(r0, r), "Unmatched open brace")

    and decideRBracket (r0, ((x, yOpt), LS.Cons ((L.RBRACKET,r), s)), p) =
          let
            val dec = (case yOpt
                         of NONE => ExtSyn.dec0 (x, Paths.join (r0, r))
                          | SOME y => ExtSyn.dec (x, y, Paths.join (r0, r)))
            val (tm, f') = parseExp (s, snil)
          in
            parseExp' (f', P.shiftAtom (ExtSyn.lam (dec, tm), p))
          end
      | decideRBracket (r0, (dec, LS.Cons ((_, r), s)), p) =
          perror (Paths.join(r0, r), "Unmatched open bracket")


    (* Parses contexts of the form  G ::= {id:term} | G, {id:term} *)
    fun stripRBrace (LS.Cons ((L.RBRACE, r), s')) = (LS.expose s', r)
      | stripRBrace (LS.Cons ((t, r), _))  = 
          perror2 (r, "Expected `}', found " ^ L.toString t)

    (* parseDec "{id:term} | {id}" *)
    and parseBracedDec (r, f) =
        let 
          val ((x, yOpt), f') = parseDec' f
          val (f'', r2) = stripRBrace f'
          val d = (case yOpt
                       of NONE => ExtSyn.dec0 (x, Paths.join (r, r2))
                        | SOME y => ExtSyn.dec (x, y, Paths.join (r, r2)))
        in
          (d, f'')
        end

    (* parseCtx (b, ds, f) = ds'
       if   f is a stream "{x1:V1}...{xn:Vn} s"
       and  b is true if no declarations has been parsed yet
       and  ds is a context of declarations
       then ds' = ds, x1:V1, ..., xn:Vn
    *)
    fun parseCtx (b, ds, LS.Cons (BS as ((L.LBRACE, r), s'))) = 
        let
          val (d, f') = parseBracedDec (r, LS.expose s')
        in
          parseCtx (false,  d :: ds, f')
        end
      | parseCtx (b, ds, f as LS.Cons ((t, r), s')) =
        if b then perror (r, "Expected `{', found " ^ L.toString t)
        else (ds, f)
 
  in
    (*[
    val parseQualId' :> inflfront -> (string list * (Lexer.idToken * Paths.region))  * inflfront
    val parseQualIds' :> inflfront -> ((string list * string) list)  * inflfront
    val parseFreeze' :> inflfront -> ((string list * string) list)  * inflfront
    val parseDeterministic' : inflfront -> ((string list * string) list)  * inflfront
    val parseTerm' :> inflfront -> ExtSyn.term  * inflfront
    val parseDec' :> inflfront -> decRes  * inflfront
    val parseCtx' : inflfront -> (ExtSyn.dec list)  * inflfront
    ]*)

    val parseQualId' = parseQualId'
    val parseQualIds' = parseQualIds'
    val parseFreeze' = (fn f => parseFreeze' (f, nil))
    val parseDeterministic' = (fn f => parseDeterministic' (f, nil))
    val parseTerm' = (fn f => parseExp' (f, snil))
    val parseDec' = parseDec'
    val parseCtx' = (fn f => (parseCtx (true, nil, f)) )
  end  (* local ... in *)

end  (* functor ParseTerm *)
