

(* Constructing the lattice takes a long time for this kind of example.  The lattice is large, 
   and the inclusion algortithm requires many unrollings before matching against an assumption.
*)

datatype nat = z | s of nat


(*[ datasort s0 = z]*)
(*[ datasort s1 = s of s0 ]*)
(*[ datasort s2 = s of s1 ]*) 
(*[ datasort s3 = s of s2 ]*) 
(*[ datasort s4 = s of s3 ]*) 
(*[ datasort s5 = s of s4 ]*) 
(*[ datasort s6 = s of s5 ]*) 
(*[ datasort s7 = s of s6 ]*) 
(*[ datasort s8 = s of s7 ]*) 
(*[ datasort s9 = s of s8 ]*) 


(*[ datasort ss0 =     s of s0 | s of s1 | s of s2 | s of s3 | s of s4 | s of s5 | s of s6 | s of s7 ]*) 
(*[ datasort ss1 = z |           s of s1 | s of s2 | s of s3 | s of s4 | s of s5 | s of s6 | s of s7 ]*) 
(*[ datasort ss2 = z | s of s0 |           s of s2 | s of s3 | s of s4 | s of s5 | s of s6 | s of s7 ]*) 
(*[ datasort ss3 = z | s of s0 | s of s1 |           s of s3 | s of s4 | s of s5 | s of s6 | s of s7 ]*) 
(*[ datasort ss4 = z | s of s0 | s of s1 | s of s2 |           s of s4 | s of s5 | s of s6 | s of s7 ]*) 

(*
(*[ datasort ss0 =     s of s0 | s of s1 | s of s2 | s of s3 | s of s4 | s of s5 | s of s6 | s of s7 | s of s8 | s of s9 ]*) 
(*[ datasort ss1 = z |           s of s1 | s of s2 | s of s3 | s of s4 | s of s5 | s of s6 | s of s7 | s of s8 | s of s9 ]*) 
(*[ datasort ss2 = z | s of s0 |           s of s2 | s of s3 | s of s4 | s of s5 | s of s6 | s of s7 | s of s8 | s of s9 ]*) 
(*[ datasort ss3 = z | s of s0 | s of s1 |           s of s3 | s of s4 | s of s5 | s of s6 | s of s7 | s of s8 | s of s9 ]*) 
(*[ datasort ss4 = z | s of s0 | s of s1 | s of s2 |           s of s4 | s of s5 | s of s6 | s of s7 | s of s8 | s of s9 ]*) 
*)
(*
(*[ datasort ss5 = z | s of s0 | s of s1 | s of s2 | s of s3 |           s of s5 | s of s6 | s of s7 | s of s8 | s of s9 ]*) 
(*[ datasort ss6 = z | s of s0 | s of s1 | s of s2 | s of s3 | s of s4 |           s of s6 | s of s7 | s of s8 | s of s9 ]*) 
(*[ datasort ss7 = z | s of s0 | s of s1 | s of s2 | s of s3 | s of s4 | s of s5 |           s of s7 | s of s8 | s of s9 ]*) 
(*[ datasort ss8 = z | s of s0 | s of s1 | s of s2 | s of s3 | s of s4 | s of s5 | s of s6 |           s of s8 | s of s9 ]*) 
(*[ datasort ss9 = z | s of s0 | s of s1 | s of s2 | s of s3 | s of s4 | s of s5 | s of s6 | s of s7 |           s of s9 ]*) 
(*[ datasort ss10 = z | s of s0 | s of s1 | s of s2 | s of s3 | s of s4 | s of s5 | s of s6 | s of s7 | s of s8           ]*) 
 *)
