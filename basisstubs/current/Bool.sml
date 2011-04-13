(*BOOL.sml*)

structure Bool:BOOL = struct(*[ assumesig BOOL ]*)
type bool = unit
  val not : bool -> bool  = fn _ => raise Match
  val fromString : string -> bool option  = fn _ => raise Match
  val scan : (char, 'a) StringCvt.reader -> 'a -> (bool * 'a) option  = fn _ => raise Match
  val toString : bool -> string  = fn _ => raise Match
end; (*signature BOOL*)

(* 
   [not b] is the logical negation of b.

   [toString b] returns the string "false" or "true" according as b is
   false or true.

   [fromString s] scans a boolean b from the string s, after possible
   initial whitespace (blanks, tabs, newlines).  Returns (SOME b) if s
   has a prefix which is either "false" or "true"; the value b is the
   corresponding truth value; otherwise NONE is returned.

   [scan getc src] scans a boolean b from the stream src, using the
   stream accessor getc.  In case of success, returns SOME(b, rst)
   where b is the scanned boolean value and rst is the remainder of
   the stream; otherwise returns NONE.
*)
