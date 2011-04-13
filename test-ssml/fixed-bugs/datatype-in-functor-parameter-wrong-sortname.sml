
(*** BUG: This program seems to result in a TYNAMEENV that isn't consistent with
          the sortname for P.MYNIL in the "duplicate" function.
     This bug has been fixed.  - Rowan 12sep03     
***)

(*** LOG:   
- R.refine_file "../test-ssml/bugs/datatype-in-functor-parameter.sml";
Impossible: RefDec.dividePatsort(2)

uncaught exception CRASH
  raised at: Common/Crash.sml:11.8-11.13
             Manager/ParseElab.sml:164.59
             Manager/ParseElab.sml:169.34


***)

signature MySig = 
  sig 
    datatype 'a MyList = MYNIL | MYCONS of 'a * 'a MyList
  end

functor testFunctor(  P : MySig  
(*** inlining MySig avoids the bug ***)
(* structure P : sig 
    datatype 'a MyList = MYNIL | MYCONS of 'a * 'a MyList
  end
*)
                   ) = 
  struct
    open P
    fun duplicate P.MYNIL = P.MYNIL
      | duplicate (P.MYCONS (x, t)) = P.MYCONS (x, P.MYCONS (x, duplicate t))
  end
