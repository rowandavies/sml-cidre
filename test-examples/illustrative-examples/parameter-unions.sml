

(*[ datasort tt = true and ff = false ]*)

datatype 'a prod = PROD1 of 'a * 'a
                 | PROD2 of 'a

(*[ datasort 'a prod1 = PROD1 of 'a * 'a
         and 'a prod2 = PROD2 of 'a  ]*)

datatype bools = B of bool prod

(*[ datasort ttffbool = B of bool prod1
                      | B of bool prod2
]*)


(*
(*[ val f :> bools -> tt ]*)
fun f (B (PROD (true, true))) = true
  | f (B (PROD (false, false))) = true
  | f (B (PROD _)) = false

val x = f (B (PROD (true, false)))
*)