(* General basis for parsing modules *)
(* Author: Frank Pfenning *)

signature PARSING =
sig
  (* structure Stream : STREAM *)

(*  structure Lexer : LEXER
    sharing Lexer.Stream = Stream
*)

  type lexResult = Lexer.Token * Paths.region

  type 'a parser = lexResult Stream.front -> 'a * lexResult Stream.front
  (* sortdef 'a parser = lexResult Stream.infFront -> 'a * lexResult Stream.infFront *)

  (* recursive parser (allows parsing functions that need to parse
     a signature expression to temporarily suspend themselves) *)
  datatype 'a RecParseResult =
    Done of 'a
  | Continuation of 'a RecParseResult parser

  type 'a recparser = 'a RecParseResult parser

  (* useful combinator for recursive parsers *)
  val recwith : 'a recparser * ('a -> 'b) -> 'b recparser

  exception Error of string
  val error : Paths.region * string -> 'a       (* always raises Error *)
end;  (* signature PARSING *)


(* Stub code to allow sorting checking the parser without including a bunch of
   other files.  *)
structure Parsing :> PARSING =
struct 
   (*[ assumesig PARSING ]*)
end
