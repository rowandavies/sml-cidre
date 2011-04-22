(* Grammar for modules - Definition v3 pages 12-14 *)

signature TOPDEC_GRAMMAR =
  sig
    (* Core declarations. *)
    eqtype strid
    type longstrid 
    
    (* Objects from the core syntax (needed for specs). *)

    eqtype id
    type longid
    eqtype tyvar
    type ty
    eqtype tycon
    type longtycon  
    type info  (* info place-holder. *)
    type dec
		
    structure DecGrammar : DEC_GRAMMAR
		where type strid = strid
		where type longstrid = longstrid
		where type id = id
		where type longid = longid
		where type tyvar = tyvar
		where type ty = ty
		where type tycon = tycon
		where type longtycon = longtycon
		where type info = info
		where type dec = dec

    (* Various kinds of module identifiers. *)
    type funid
    type sigid
    structure FunId : FUNID  where type funid = funid
    structure SigId : SIGID  where type sigid = sigid

	
    datatype 'a WithInfo = WITH_INFO of info * 'a
      sharing type WithInfo = DecGrammar.WithInfo

			    (* Figure 6 *)

    datatype strexp =
      STRUCTstrexp of info * strdec |
      LONGSTRIDstrexp of info * longstrid |
      TRANSPARENT_CONSTRAINTstrexp of info * strexp * sigexp |
      OPAQUE_CONSTRAINTstrexp of info * strexp * sigexp |
      APPstrexp of info * funid * strexp |
      LETstrexp of info * strdec * strexp |
      ASSUMEstrexp of info * sigexp

    and strdec =
      DECstrdec of info * dec |
      STRUCTUREstrdec of info * strbind |
      LOCALstrdec of info * strdec * strdec |
      EMPTYstrdec of info |
      SEQstrdec of info * strdec * strdec

    and strbind =
      STRBIND of info * strid * strexp * strbind option

    and sigexp =
      SIGsigexp of info * spec |
      SIGIDsigexp of info * sigid |
      WHERE_TYPEsigexp of info * sigexp * tyvar list * longtycon * ty

    and sigdec =
      SIGNATUREsigdec of info * sigbind

    and sigbind =
      SIGBIND of info * sigid * sigexp * sigbind option

			    (* Figure 7 *)

    and spec =
      VALspec of info * valdesc |
      VALsspec of info * valdesc |
      TYPEspec of info * typdesc |
      SORTspec of info * sortdesc |
      SUBSORTspec of info * spec * longtycon WithInfo list * longtycon WithInfo list |
      EQTYPEspec of info * typdesc |
      DATATYPEspec of info * datdesc |
      DATATYPE_REPLICATIONspec of info * tycon * longtycon |
      DATASORTspec of info * datdesc |
      EXCEPTIONspec of info * exdesc |
      STRUCTUREspec of info * strdesc |
      INCLUDEspec of info * sigexp |
      SHARING_TYPEspec of info * spec * longtycon WithInfo list |
      SHARINGspec of info * spec * longstrid WithInfo list |
      EMPTYspec of info |
      SEQspec of info * spec * spec

    and valdesc =
      VALDESC of info * id * ty * valdesc option

    and typdesc =
      TYPDESC of info * tyvar list * tycon * typdesc option

    and sortdesc = (* bool false => longtycon is a compatible sort, true => is upper bound *)
      SORTDESC of info * tyvar list * tycon * bool * longtycon * sortdesc option

(*    and subsortdesc =  (* s1 & s2 & .... sn < r1 & r2 & ... rn *)           
        SUBSORTDESC of info * longtycon WithInfo list * longtycon WithInfo list 
                         * subsortdesc option *)
    and datdesc =
      DATDESC of info * tyvar list * tycon * condesc * datdesc option

    and condesc =
      CONDESC of info * longid * ty option * condesc option

    and exdesc =
      EXDESC of info * id * ty option * exdesc option

    and strdesc =
      STRDESC of info * strid * sigexp * strdesc option

			    (* Figure 8 *)

    and fundec =
      FUNCTORfundec of info * funbind 

    and funbind =
      FUNBIND of info * funid * strid * sigexp * strexp * funbind option

    and topdec =
      STRtopdec of info * strdec * topdec option |
      SIGtopdec of info * sigdec * topdec option |
      FUNtopdec of info * fundec * topdec option

    val empty_topdec : topdec

    val getExplicitTyVarsCondesc : condesc -> tyvar list
    val info_on_strexp : strexp -> info
    val info_on_strdec : strdec -> info
    val info_on_strbind : strbind -> info
    val info_on_sigexp : sigexp -> info
    val info_on_sigdec : sigdec -> info
    val info_on_sigbind : sigbind -> info
    val info_on_spec : spec -> info
    val info_on_valdesc : valdesc -> info
    val info_on_typdesc : typdesc -> info
    val info_on_sortdesc : sortdesc -> info
    val info_on_datdesc : datdesc -> info
    val info_on_condesc : condesc -> info
    val info_on_exdesc : exdesc -> info
    val info_on_strdesc : strdesc -> info
    val info_on_fundec : fundec -> info
    val info_on_funbind : funbind -> info
    val info_on_topdec : topdec -> info

    val map_strexp_info : (info -> info) -> strexp -> strexp
      (* does not proceed into sigexp's *)


    val layoutStrexp  : strexp  -> StringTree.t
    and layoutStrdec  : strdec  -> StringTree.t
    and layoutStrbind : strbind -> StringTree.t
    and layoutSigexp  : sigexp  -> StringTree.t
    and layoutSpec    : spec    -> StringTree.t
    and layoutFunbind : funbind -> StringTree.t
    and layoutTopdec  : topdec  -> StringTree.t
  end;
