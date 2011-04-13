

(*[ datasort tt = true and ff = false ]*)

datatype 'a prod = PROD of 'a * 'a

datatype bools = B of bool prod

(*[ datasort ttffbool = B of tt prod
                    | B of ff prod
]*)


(*[ val f :> bools -> tt ]*)
fun f (B (PROD (true, true))) = true
  | f (B (PROD (false, false))) = true
  | f (B (PROD _)) = false

val x = f (B (PROD (true, false)))
