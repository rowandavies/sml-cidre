
local 
(*   datatype bool = datatype bool
   datatype option = datatype option
*)
in
   datatype abcd = C of bool * bool

(*[ datasort tt = true ]*)
(*[ datasort '?a none = NONE ]*)
(*[ datasort '+a some = SOME of '+a ]*)

(*[ sortdef abc = int * int ]*)

(*[ val x :> bool none ]*)
val x = NONE

(*[ val y :> tt none ]*)
val y = x

(*[ val x2 :> bool some ]*)
val x2 = SOME false


(*[ val y2 :> tt none ]*)
val y2 = x

end