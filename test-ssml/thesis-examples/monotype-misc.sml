
  datatype monotype = Int of int | Bool of bool 
		    | Nil | Cons of monotype * monotype
		    | Fun of monotype -> monotype

  (*[ datasort mtInt = Int of int
	   and mtBool = Bool of bool
	   and mtList = Nil | Cons of monotype * mtList
	   and mtFun = Fun of monotype -> monotype 
	   and mtIntArrowInt = Fun of mtInt -> mtInt  
  ]*)

  (*[ val abs :> mtInt -> mtInt ]*)
  fun abs (x as (Int i)) = if i > 0 then x else Int (~ i)


  (*[ datasort mtBaseFun = Fun of mtInt -> mtInt 
                         | Fun of mtBool -> mtBool ]*)

  (*[ datasort mtIntFun = Fun of mtInt -> mtInt
           and mtBoolFun = Fun of mtBool -> mtBool ]*)

(*
  (*[ val twice :> mtBaseFun -> mtBaseFun ]*)
  fun twice (x as (Fun f)) = Fun (f o f)
*)

(*  (*[ val funPower :> mtInt -> mtBaseFun -> mtBaseFun ]*)
  fun funPower (Int i) (x as (Fun f)) = 
       if i=0 then  Fun id
       else let val half = funPower (i/2) x  in
*)
(*
  (*[ val funPower :> mtBaseFun -> mtBaseFun ]*)
  fun fixedPoint (baseFun as (Fun f)) x = 
       if f x = x then x
       else 

       else let val half = funPower (i/2) x  in

  (*[ datasort mtBase = Int of int | Bool of bool ]*)


  (*[ datasort mtBaseList = Nil | Cons of mtBase * mtBaseList ]*)
*)

(*
  (*[ val eqMtBase :> mtInt * mtInt -> bool 
                    & mtBool * mtBool -> bool  ]*)
  fun eqMtBase (Int i1, Int i2) = (i1 = i2)
    | eqMtBase (Bool b1, Bool b2) = (b1 = b2)

  (*[ datasort mtBasePairsList = Nil | Cons of mtInt * mtIntBasePairsList 
                                     | Cons of mtBool * mtBoolBasePairsList 
           and mtIntBasePairsList = Cons of mtInt * mtBasePairsList
           and mtBoolBasePairsList = Cons of mtBool * mtBasePairsList ]*)

  (*[ val tailFromEqPair :> mtBasePairsList -> mtBasePairsList ]*)
  fun tailFromEqPair (wholeList as Cons (head1, Cons (head2, tail))) = 
      if eqMtBase (head1, head2) then wholeList
      else tailFromEqPair tail
    | tailFromEqPair Nil = Nil

  (*[ val maybeTwice :> (unit -> bool) -> mtBaseFun -> mtBaseFun ]*)
  fun maybeTwice decide (funf as Fun f) = 
      if decide() then Fun (fn x => (f (f x))) else funf

*)

  (*[ datasort ff = false ]*)
  (*[ datasort mtBase = Int of int | Bool of bool  ]*)

  (*[ val eqMtBase :>  mtBase * mtBase -> bool
                     & mtInt * mtBool -> ff   
                     & mtBool * mtInt -> ff    ]*)
  fun eqMtBase (Int i1, Int i2) = (i1 = i2)
    | eqMtBase (Bool b1, Bool b2) = (b1 = b2)
    | eqMtBase _ = false

  (*[ datasort mtBaseList = Nil | Cons of mtInt * mtBaseList 
                                | Cons of mtBool * mtBaseList 
           and mtIntBaseList =  Nil | Cons of mtInt * mtBaseList
           and mtBoolBaseList = Nil | Cons of mtBool * mtBaseList ]*)

  (*[ val tailFromElem :>  mtInt -> mtBaseList -> mtIntBaseList 
                         & mtBool -> mtBaseList -> mtBoolBaseList  ]*)
  fun tailFromElem elem (wholeList as Cons (head, tail)) = 
      if eqMtBase (head, elem) then 
           wholeList
      else 
           tailFromElem elem tail
    | tailFromElem elem Nil = Nil

  (*[ val tailFromCheck :>  (mtInt -> bool & mtBool -> ff) -> mtBaseList -> mtIntBaseList 
                          & (mtBool -> bool & mtInt -> ff) -> mtBaseList -> mtBoolBaseList  ]*)
  fun tailFromCheck check (wholeList as Cons (head, tail)) =
      if check head then
           wholeList
      else 
           tailFromCheck check tail
    | tailFromCheck check Nil = Nil

                

(*
  (*[ val eqMtBase :>  mtInt * mtInt -> bool 
                     & mtBool * mtInt -> ff ]*)
  fun eqMtBase (Int i1, Int i2) = (i1 = i2)
    | eqMtBase (Bool b1, Bool b2) = (b1 = b2)
    | eqMtBase _ = false

  (*[ datasort mtBaseList = Nil | Cons of mtInt * mtBaseList 
                                | Cons of mtBool * mtBaseList 
           and mtIntBaseList = Nil | Cons of mtInt * mtBaseList  ]*)

  (*[ val tailFromElem :>  mtInt -> mtBaseList -> mtIntBaseList  ]*)
  fun tailFromElem elem (wholeList as Cons (head, tail)) = 
      if eqMtBase (head, elem) then 
           wholeList
      else 
           tailFromElem elem tail
    | tailFromElem elem Nil = Nil
*)