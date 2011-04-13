(* This example illustrates the calculation of the sorts of
   constructors. *)

(*[ datasort tt = true and ff = false ]*)

datatype d = C of bool -> bool

(*[ datasort s1 = C of tt -> tt 
    datasort s2 = C of ff -> ff
  ]*)
