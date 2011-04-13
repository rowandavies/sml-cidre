
signature S = 
sig
  type t
  (*[ sortdef s1 |: t 
      sortdef s2 |: t 
      sortdef s3 |: t
      sortdef s4 |: t
      sortdef s5 |: t
      sortdef s6 |: t
      sortdef s7 |: t
      sortdef s8 |: t
  ]*)

  (*[ subsort s1 < t ]*)
  (*[ subsort s2 < s1 ]*)
  (*[ subsort s3 < s2 ]*)
  (*[ subsort s4 < s3 ]*)
  (*[ subsort s5 < s4 ]*)
  (*[ subsort s5 < s4 ]*)
  (*[ subsort s6 < s5 ]*)
  (*[ subsort s7 < s6 ]*)
  (*[ subsort s8 < s7 ]*)

end

(*
- R.refine_file "../test-ssml/illustrative-examples/abstract-lattice-large.sml";
Sort Checking file: ../test-ssml/illustrative-examples/abstract-lattice-large.sml

Time for Elaboration and Sort Checking
	         non-gc         system             gc      wallclock
	        105.682          0.000         39.617        105.682
> signature S =
    sig
      type t
      sortdef t = t |: t
      sortdef s1 = s1 |: t
      sortdef s2 = s2 |: t
      sortdef s3 = s3 |: t
      sortdef s4 = s4 |: t
      sortdef s5 = s5 |: t
      sortdef s6 = s6 |: t
      sortdef s7 = s7 |: t
      sortdef s8 = s8 |: t
      sortdef s9 = s9 |: t
      sortdef s10 = s10 |: t
        
        Table of Intersections:
        [Skipped large table with 2047 X 2047 elements.]
        
    end
val it = () : unit

*)
