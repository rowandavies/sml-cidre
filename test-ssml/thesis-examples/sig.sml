  signature MONOTYPE =
  sig
     datatype monotype = 
         Int of int  |  Bool of bool 
       | Nil  |  Cons of monotype * monotype
       | Fun of monotype -> monotype

     (*[ datasort mtInt = Int of int ]*)
     type mtArrow
     (*[ sortdef mtIntArrow <| mtArrow ]*)

     (*[ val double :> mtIntArrow ]*)
     val double : mtArrow

     (*[ val quiteLikelyFour :> mtInt ]*)
     val quiteLikelyFour : monotype
  end
