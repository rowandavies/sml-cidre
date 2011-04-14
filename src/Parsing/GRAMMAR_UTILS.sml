(* Contains functions to assist the analysis of derived forms, patch up the
   holes resulting from ambiguities in the SML grammar, and generally assist
   in the building of the parse tree. *)

(*$GRAMMAR_UTILS : DEC_GRAMMAR TOPDEC_GRAMMAR*)
signature GRAMMAR_UTILS =
  sig
    structure TopdecGrammar : TOPDEC_GRAMMAR
    type info = TopdecGrammar.info
    type Report

   (*We can get syntax errors while analysing layered patterns (this is
    during the activity of the parser; we aren't post-passing yet).*)

    type pos
    exception LAYERPAT_ERROR of (pos * pos) 

    val topdecOfExp: TopdecGrammar.DecGrammar.exp -> TopdecGrammar.topdec
    val composeStrDec: info * TopdecGrammar.strdec * TopdecGrammar.strdec -> TopdecGrammar.strdec
    val composeSpec: info * TopdecGrammar.spec * TopdecGrammar.spec -> TopdecGrammar.spec
    val inventStrId: unit -> TopdecGrammar.strid
    val composeDec: info * TopdecGrammar.DecGrammar.dec * TopdecGrammar.DecGrammar.dec -> TopdecGrammar.DecGrammar.dec
    val tuple_atexp_with_info : info -> TopdecGrammar.DecGrammar.exp list -> TopdecGrammar.DecGrammar.atexp
    val tuple_atexp : TopdecGrammar.DecGrammar.exp list -> TopdecGrammar.DecGrammar.atexp
    val case_exp : info -> TopdecGrammar.DecGrammar.exp * TopdecGrammar.DecGrammar.match -> TopdecGrammar.DecGrammar.exp
    val sequenceExp: TopdecGrammar.DecGrammar.exp list -> TopdecGrammar.DecGrammar.exp
    val inventId: unit -> TopdecGrammar.DecGrammar.id
    val inventId_from_atpat: TopdecGrammar.DecGrammar.atpat -> TopdecGrammar.DecGrammar.id
    val atexpOfIdent : info -> TopdecGrammar.DecGrammar.id -> TopdecGrammar.DecGrammar.atexp
    val patOfIdent : info -> TopdecGrammar.DecGrammar.id * bool -> TopdecGrammar.DecGrammar.pat
    val patOfAtpat: TopdecGrammar.DecGrammar.atpat -> TopdecGrammar.DecGrammar.pat
    val expOfAtexp: TopdecGrammar.DecGrammar.atexp -> TopdecGrammar.DecGrammar.exp
    val list_atexp : info -> TopdecGrammar.DecGrammar.exp list -> TopdecGrammar.DecGrammar.atexp
    val hash : info -> TopdecGrammar.DecGrammar.lab -> TopdecGrammar.DecGrammar.atexp
    val exp_true : info -> TopdecGrammar.DecGrammar.exp
    val exp_false : info -> TopdecGrammar.DecGrammar.exp
    val if_then_else_exp : info -> TopdecGrammar.DecGrammar.exp * TopdecGrammar.DecGrammar.exp * TopdecGrammar.DecGrammar.exp -> TopdecGrammar.DecGrammar.exp
    val while_exp : info -> TopdecGrammar.DecGrammar.exp * TopdecGrammar.DecGrammar.exp -> TopdecGrammar.DecGrammar.exp
    val rewriteDatBind: TopdecGrammar.DecGrammar.datbind * TopdecGrammar.DecGrammar.typbind -> TopdecGrammar.DecGrammar.datbind
    val tuple_atpat_with_info : info -> TopdecGrammar.DecGrammar.pat list -> TopdecGrammar.DecGrammar.atpat
    val tuple_atpat : TopdecGrammar.DecGrammar.pat list -> TopdecGrammar.DecGrammar.atpat
    val list_atpat : info -> TopdecGrammar.DecGrammar.pat list -> TopdecGrammar.DecGrammar.atpat
    val layeredPat: info * TopdecGrammar.DecGrammar.pat * TopdecGrammar.DecGrammar.pat -> TopdecGrammar.DecGrammar.pat
    val tuple_type : info -> TopdecGrammar.DecGrammar.ty list -> TopdecGrammar.DecGrammar.ty
    val rewrite_type_abbreviation_spec: TopdecGrammar.tyvar list * TopdecGrammar.tycon * TopdecGrammar.ty *
          TopdecGrammar.info * TopdecGrammar.info -> info * TopdecGrammar.spec
    val fold_specs_to_spec : (info * TopdecGrammar.spec) list -> TopdecGrammar.spec

    val raise_lexical_error_if_none : pos -> 'a option -> 'a
        (*raise LexBasics.LEXICAL_ERROR (pos, "grr") if the option is NONE.*)

   (* The following all come from the appropriate modules, but they're here
      for convenience and brevity. *)

    val mk_IdentLab: string -> TopdecGrammar.DecGrammar.lab
    val mk_IntegerLab: int -> TopdecGrammar.DecGrammar.lab
    val mk_Id: string -> TopdecGrammar.DecGrammar.id
    val mk_LongId: string list -> TopdecGrammar.DecGrammar.longid
    val mk_FunId: string -> TopdecGrammar.funid
    val mk_StrId: string -> TopdecGrammar.strid
    val longStrIdOfStrId : TopdecGrammar.strid -> TopdecGrammar.longstrid
    val mk_SigId: string -> TopdecGrammar.sigid
    val mk_LongStrId: string list -> TopdecGrammar.longstrid
    val mk_TyVar: string -> TopdecGrammar.DecGrammar.tyvar
    val mk_TyCon: string -> TopdecGrammar.DecGrammar.tycon
    val mk_LongTyCon: string list -> TopdecGrammar.longtycon
    val mk_IntSCon: int -> TopdecGrammar.DecGrammar.scon
    val mk_WordSCon: int -> TopdecGrammar.DecGrammar.scon
    val mk_StringSCon: string -> TopdecGrammar.DecGrammar.scon
    val mk_CharSCon: int -> TopdecGrammar.DecGrammar.scon
    val mk_RealSCon: string -> TopdecGrammar.DecGrammar.scon
    val impossible : string -> 'a
    val span_info : info * info -> info
    val PP : pos -> pos -> info	(* `PP L R' generates position
					   information. *)
    val un_PP : info -> pos * pos
    val rightmost :  ('a -> info) -> 'a ->
                     ('b -> info) -> 'b option -> pos
    val rightmost' : pos -> ('b -> info) -> 'b option -> pos
    val rightmost_of_three : pos -> ('b -> info) -> 'b option ->
                                    ('c -> info) -> 'c option -> pos
    val rightmost_of_four  : pos -> ('b -> info) -> 'b option ->
                                    ('c -> info) -> 'c option ->
                                    ('d -> info) -> 'd option -> pos
    val right : info -> pos
    val wi_Convert: ('a -> 'b) -> 'a TopdecGrammar.DecGrammar.WithInfo list -> 'b TopdecGrammar.DecGrammar.WithInfo list
					(* Conversion of "with-info" tagged
					   lists (of identifiers etc.) *)
    val reportPosition_from_info : info -> Report
  end;
