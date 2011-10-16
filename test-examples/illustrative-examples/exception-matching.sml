
exception MyException

(* This is accepted.  We don't check coverage in a handle.  *)
fun g x = raise MyException handle Match => () 

(* This is rejected.  *)
fun f MyException = ()
