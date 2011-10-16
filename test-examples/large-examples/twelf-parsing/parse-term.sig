(* Parsing Terms and Declarations *)
(* Author: Frank Pfenning *)

(* Sorts added by: Rowan Davies *)

signature PARSE_TERM =
sig

  (*! structure Parsing : PARSING !*)
  structure ExtSyn : EXTSYN

  val parseQualId' : (string list * Parsing.lexResult) Parsing.parser
  val parseQualIds' : ((string list * string) list) Parsing.parser 
  val parseFreeze' : ((string list * string) list) Parsing.parser
  val parseDeterministic' : ((string list * string) list) Parsing.parser
  val parseTerm' : ExtSyn.term Parsing.parser
  val parseDec' : (string option * ExtSyn.term option) Parsing.parser
  val parseCtx' : (ExtSyn.dec list) Parsing.parser


  (* To specify the refinements for these functions, we need to expand the uses of
     Parsing.parser, since the sort checker currently does not support transparent
     sort specs in signatures (although it does support transparent datasort specs). *)

  (* Here are the value specifications with Parsing.parser expanded, just for comparison
     with the value sort specifications below. *)
  (*
  val parseQualId' : Parsing.lexResult Stream.front -> 
                     (string list * Parsing.lexResult)  * Parsing.lexResult Stream.front
  val parseQualIds' : Parsing.lexResult Stream.front -> 
                      ((string list * string) list)  * Parsing.lexResult Stream.front
  val parseFreeze' : Parsing.lexResult Stream.front -> 
                     ((string list * string) list)  * Parsing.lexResult Stream.front
  val parseDeterministic' : Parsing.lexResult Stream.front -> 
                            ((string list * string) list)  * Parsing.lexResult Stream.front
  val parseTerm' : Parsing.lexResult Stream.front -> 
                   ExtSyn.term  * Parsing.lexResult Stream.front
  val parseDec' : Parsing.lexResult Stream.front -> 
                  (string option * ExtSyn.term option)  * Parsing.lexResult Stream.front
  val parseCtx' : Parsing.lexResult Stream.front -> 
                  (ExtSyn.dec list)  * Parsing.lexResult Stream.front
  *)


  (* Here are the corresponding sorts, using the refinement infFront of front for infinite
     streams, i.e. ones that are guaranteed not to ever reach nil. *)
  (*[
  val parseQualId' :> Parsing.lexResult Stream.infFront -> 
                       (string list * (Lexer.idToken * Paths.region))
                       * Parsing.lexResult Stream.infFront
  val parseQualIds' :> Parsing.lexResult Stream.infFront -> 
                       ((string list * string) list)  * Parsing.lexResult Stream.infFront
  val parseFreeze' :> Parsing.lexResult Stream.infFront -> 
                      ((string list * string) list)  * Parsing.lexResult Stream.infFront
  val parseDeterministic' : Parsing.lexResult Stream.infFront -> 
                            ((string list * string) list)  * Parsing.lexResult Stream.infFront
  val parseTerm' :> Parsing.lexResult Stream.infFront -> 
                    ExtSyn.term  * Parsing.lexResult Stream.infFront
  val parseDec' :> Parsing.lexResult Stream.infFront -> 
                   (string option * ExtSyn.term option)  * Parsing.lexResult Stream.infFront
  val parseCtx' : Parsing.lexResult Stream.infFront -> 
                  (ExtSyn.dec list)  * Parsing.lexResult Stream.infFront
  ]*)

end;  (* signature PARSE_TERM *)
