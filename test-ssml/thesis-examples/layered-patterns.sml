  datatype monotype = Int of int | Bool of bool 
		    | Nil | Cons of monotype * monotype
		    | Fun of monotype -> monotype

  (*[ datasort mtInt = Int of int  ]*)

  (*[ val double :> mtInt -> mtInt ]*)
  fun double (Int x) = Int (x + x)

(* The above appear before the layered patterns example.  *)
(*-------------------------------------------*)

  (*[ datasort ff = false
      datasort mtBool = Bool of bool
      datasort mtBaseList = Nil | Cons of mtInt * mtBaseList
                                | Cons of mtBool * mtBaseList
      and mtIntBaseList   = Nil | Cons of mtInt * mtBaseList
  ]*)

  (*[ val tailFrom :> (mtInt -> bool  &  mtBool -> ff)
                      -> mtBaseList -> mtIntBaseList   ]*)
  fun tailFrom choose (wholeList as Cons (head, tail)) =
         if choose head then
              wholeList
         else
              tailFrom choose tail
    | tailFrom check Nil = Nil


  (*[ val tailFrom :> (mtInt -> bool  &  mtBool -> ff) 
                      -> mtBaseList -> mtIntBaseList   ]*)
  fun tailFrom choose (Cons (head, tail)) =
       let val wholeList = Cons (head, tail)
       in
         if choose head then
              wholeList
         else
              tailFrom choose tail
       end
    | tailFrom check Nil = Nil


  (*[ val f :> monotype -> mtInt ]*)
  fun f x =
     case x of 
	x as Int y => double x
      | _ => Int 0


  (*[ val f :> monotype -> mtInt ]*)
  fun f x =
     case x of 
	Int y => double x  (* Error here *)
      | _ => Int 0
