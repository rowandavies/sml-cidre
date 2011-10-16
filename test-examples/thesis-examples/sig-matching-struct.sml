signature MONOTYPE =
   sig
     datatype monotype =
         Int of int  |  Bool of bool
       | Nil  |  Cons of monotype * monotype
       | Fun of monotype -> monotype

     (*[ datasort mtInt = Int of int ]*)

     type mtArrow
     (*[ sortdef mtIntToInt |: mtArrow ]*)
     (*[ sortdef mtAllToInt < mtArrow ]*)
     (*[ subsort mtAllToInt < mtIntToInt ]*)

     (*[ val double :> mtInt -> mtInt ]*)
     val double : monotype -> monotype

     (*[ val tryDouble :> mtAllToInt ]*)
     val tryDouble : mtArrow

     (*[ val quiteLikelyFour :> mtInt ]*)
     val quiteLikelyFour : monotype
  end;


  structure Monotype =
  struct
     datatype monotype = 
         Int of int  |  Bool of bool 
       | Nil  |  Cons of monotype * monotype
       | Fun of monotype -> monotype

     (*[ datasort mtInt = Int of int ]*)

     type mtArrow = monotype -> monotype
     (*[ sortdef mtIntToInt = mtInt -> mtInt ]*)
     (*[ sortdef mtAllToInt = monotype -> mtInt ]*)

     (*[ val double :> mtIntToInt ]*)
     fun double (Int x) = Int (x + x)

     (*[ val tryDouble :> mtAllToInt ]*)
     fun tryDouble (Int x) = double (Int x)
       | tryDouble nonInt = raise Match

     val quiteLikelyFour = double (Int 2 (*[ :> mtInt ]*) )
  end

  structure Monotype2 :> MONOTYPE = Monotype

