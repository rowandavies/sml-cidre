
(* If this file is loaded followed by the file top-level-val-sort-specs-propagate-to-other-files2
   an error occurs.  Probably the val sort spec should be limited to one file.  - Rowan 7sep04 *)

datatype T = C

(*[ val bugfun :> T -> T ]*)
fun bugfun x = x

