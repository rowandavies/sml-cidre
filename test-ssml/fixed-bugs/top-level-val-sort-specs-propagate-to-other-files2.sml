
(* If this file is loaded after top-level-val-sort-specs-propagate-to-other-files, 
   an error occurs.  *)

datatype T = C

fun bugfun C = C

