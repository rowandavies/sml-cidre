
structure M :> sig 
		  structure Monotype : sig type mtArrow  (*[ sortdef mtIntArrow <| mtArrow ]*) end
		  structure mtBaseList : sig type mtArrow (*[ sortdef mtIntArrow <| mtArrow ]*) end
                  sharing type Monotype.mtArrow = mtBaseList.mtArrow
	      end

= 
struct
  structure Monotype = 
  struct
     datatype monotype = 
         Int of int  |  Bool of bool 
       | Nil  |  Cons of monotype * monotype
       | Fun of monotype -> monotype

     (*[ datasort mtInt = Int of int ]*)

     type mtArrow = monotype -> monotype

     (*[ sortdef mtIntArrow = mtInt -> mtInt ]*)

     (*[ val double :> mtIntArrow ]*)
     fun double (Int x) = Int (x + x)

     val quiteLikelyFour = double (Int 2 (*[ :> mtInt ]*) )
  end

  structure mtBaseList =
  struct
       datatype monotype = datatype Monotype.monotype
       type mtArrow = Monotype.mtArrow
       (*[ sortdef mtIntArrow = Monotype.mtIntArrow ]*)
    (*[ datasort mtIntList = 
            Monotype.Nil 
          | Monotype.Cons of Monotype.mtInt * mtIntList
    ]*)

    (*[ val append :> mtIntList -> mtIntList 
                      -> mtIntList   ]*)
    fun append (Monotype.Cons (h, t)) l = 
           Monotype.Cons (h, append t l)
      | append Monotype.Nil l = l
  end
end