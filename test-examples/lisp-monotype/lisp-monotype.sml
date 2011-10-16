structure ListMonotype = struct

datatype monotype = Int of int | Bool of bool 
                  | Nil | Cons of monotype * monotype
                  | Fun of monotype -> monotype

(*[ datasort monotypeInt = Int of int
         and monotypeBool = Bool of bool
         and monotypeList = Nil | Cons of monotype * monotypeList
         and monotypeFun = Fun of monotype -> monotype 
         and monottypeIntArrowInt = Fun of monotypeInt -> monotypeInt  
]*)

(*[ val double :> monotypeInt -> monotypeInt ]*)
fun double (Int x) = Int (x + x)
val monoDouble = Fun double

end
