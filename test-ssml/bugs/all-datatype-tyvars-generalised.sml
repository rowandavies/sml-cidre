(* This example crashes the elaborator in the ML/Kit 3.0, because all
   tyvars are incorrectly generalised, but the explicitly scoped variable
   'a should not be.
   
   If the elaborator was fixed, then the sort checking code would also need to
   be fixed otherwise it would crash.  - Rowan 14aug02

   [Checked 12sep03, this is still the case.  - Rowan] *)


local fun 'a f x:'a = x
in 
  datatype T = c of 'a * 'b  (* Both 'a and 'b are generalised, each of which leads to a crash *)
end                          (* instead of an error message. *)