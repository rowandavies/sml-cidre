structure s = struct

datatype monotype = Int of int  |  Bool of bool 
                  | Nil  |  Cons of monotype * monotype
                  | Fun of monotype -> monotype

(*[ datasort mtInt = Int of int
         and mtBool = Bool of bool
         and mtList = Nil | Cons of monotype * mtList
         and mtFun = Fun of monotype -> monotype
         and mtIntArrowInt = Fun of mtInt -> mtInt  
]*)


(*[ sortdef monotype = mtInt ]*)
fun double (Int x) = Int (x + x)
val monoDouble = Fun double

end
